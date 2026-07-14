/* hafod.c -- Native launcher for hafod (scsh on Chez Scheme)
 *
 * A tiny C trampoline that initialises the Chez Scheme runtime and
 * loads the pre-compiled hafod program.  Replaces the shell wrapper,
 * eliminating one fork+exec and the shell parse overhead.
 *
 * At install time, LIBDIR is baked in (e.g. /usr/local/lib/hafod).
 * The binary expects:
 *   LIBDIR/petite.boot   -- Chez Scheme base boot file (or symlink)
 *   LIBDIR/scheme.boot   -- Chez Scheme compiler boot file (optional but recommended)
 *   LIBDIR/hafod.so      -- compiled hafod launcher program
 *   LIBDIR/src/          -- compiled hafod libraries
 */

/* Suppress -Wformat-truncation: we intentionally use snprintf with
 * PATH_MAX buffers where truncation is harmless (checked via access). */
#pragma GCC diagnostic ignored "-Wformat-truncation"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <limits.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
#ifdef __FreeBSD__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#include <scheme.h>

#ifndef LIBDIR
#define LIBDIR "/usr/local/lib/hafod"
#endif

/* Resolve the path to the running executable.
 * Returns > 0 on success (length written to buf, not counting NUL). */
static int get_exe_path(char *buf, size_t bufsz) {
#if defined(__linux__)
    ssize_t len = readlink("/proc/self/exe", buf, bufsz - 1);
    if (len > 0) { buf[len] = '\0'; return (int)len; }
#elif defined(__APPLE__)
    uint32_t sz = (uint32_t)bufsz;
    if (_NSGetExecutablePath(buf, &sz) == 0) {
        /* Resolve symlinks so dirname works predictably */
        char resolved[PATH_MAX];
        if (realpath(buf, resolved)) {
            strncpy(buf, resolved, bufsz - 1);
            buf[bufsz - 1] = '\0';
        }
        return (int)strlen(buf);
    }
#elif defined(__FreeBSD__)
    int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
    size_t len = bufsz;
    if (sysctl(mib, 4, buf, &len, NULL, 0) == 0)
        return (int)(len - 1);
#endif
    return 0;
}

static const char *find_libdir(const char *argv0) {
    const char *env = getenv("HAFOD_LIBDIR");
    if (env) return env;

    /* Try relative to the executable: ../lib/hafod */
    char self[PATH_MAX];
    if (get_exe_path(self, sizeof(self)) > 0) {
        char *dir = dirname(self);
        static char rel[PATH_MAX];
        snprintf(rel, sizeof(rel), "%s/../lib/hafod", dir);
        if (access(rel, R_OK) == 0) return rel;
    }

    return LIBDIR;
}

static void custom_init(void) {
    /* nothing needed */
}

/* Parse one run of decimal digits, advancing *pp past it. Returns 0 on no
 * digits or an absurd value, rather than wrapping silently. */
static int parse_uint(const char **pp, unsigned *out) {
    const char *p = *pp;
    unsigned v = 0;

    if (*p < '0' || *p > '9')
        return 0;
    while (*p >= '0' && *p <= '9') {
        if (v > 99999u)
            return 0;
        v = v * 10u + (unsigned)(*p - '0');
        p++;
    }
    *pp = p;
    *out = v;
    return 1;
}

/* Read the recorded build-Chez triple out of the generated library source and
 * render it as "major.minor.sub".  Same file, and the same strict digits-only
 * shape, as the bin/hafod shell wrapper's anchored sed reads.  Returns 1 on
 * success, 0 when the file is missing or malformed. */
static int read_recorded_chez_version(const char *libdir, char *out, size_t outsz) {
    static const char key[] = "build-chez-version-number '(";
    char path[PATH_MAX];
    char buf[8192];
    const char *p;
    unsigned major, minor, sub;
    size_t n;
    FILE *f;

    snprintf(path, sizeof(path), "%s/src/hafod/internal/chez-version.ss", libdir);
    f = fopen(path, "r");
    if (!f)
        return 0;
    n = fread(buf, 1, sizeof(buf) - 1, f);
    fclose(f);
    buf[n] = '\0';

    p = strstr(buf, key);
    if (!p)
        return 0;
    p += sizeof(key) - 1;

    if (!parse_uint(&p, &major) || *p++ != ' ')
        return 0;
    if (!parse_uint(&p, &minor) || *p++ != ' ')
        return 0;
    if (!parse_uint(&p, &sub) || *p != ')')
        return 0;

    snprintf(out, outsz, "%u.%u.%u", major, minor, sub);
    return 1;
}

/* Pre-flight version guard, the counterpart of the one in the bin/hafod shell
 * wrapper -- and, until now, the reason `make install` could ship a launcher
 * that had none.  The wrapper checks the Chez it is about to exec; this binary
 * IS its Chez (libkernel.a is linked in), so the version to check is the
 * kernel's own, and it is fixed at link time.
 *
 * The skew is real and it is not hypothetical: `make native` outside the dev
 * shell links the system Chez's kernel, while `make compile-wpo` and
 * `make install` inside it build the fasls and copy the boot files from another.
 * The binary then loads a petite.boot and a hafod.so built by a Chez that is not
 * the one it carries, and dies in the kernel with "is for Version 10.4.1; need
 * Version 10.0.0" -- exactly the raw crash the wrapper's guard exists to replace
 * with a sentence a person can act on.  Runs before Sregister_boot_file, because
 * that is where the raw failure happens.
 *
 * Returns 0 to proceed, or an exit status to fail with. */
static int check_chez_version(const char *libdir) {
    char expected[64];
    const char *running = Skernel_version();

    if (!read_recorded_chez_version(libdir, expected, sizeof(expected))) {
        /* We cannot prove the fasls under libdir match the kernel linked here.
         * hafod.so is a whole-program image tied to one Chez, and loading it
         * under a mismatched one is the raw crash this guard prevents -- so
         * refuse, rather than load it and hope.  Never fires in a dev tree or an
         * installed one: both always carry the recorded version. */
        fprintf(stderr,
                "hafod: cannot verify the required Chez Scheme version "
                "(%s/src/hafod/internal/chez-version.ss is unreadable).\n",
                libdir);
        fprintf(stderr,
                "hafod: run 'nix develop', then 'make native' to rebuild.\n");
        return 1;
    }

    if (running && strcmp(expected, running) != 0) {
        fprintf(stderr,
                "hafod: Chez Scheme %s required, but this binary is built "
                "against %s.\n",
                expected, running);
        fprintf(stderr,
                "hafod: run 'nix develop', then 'make native' to rebuild "
                "against a matching Chez.\n");
        return 1;
    }

    return 0;
}

int main(int argc, const char **argv) {
    const char *libdir = find_libdir(argv[0]);

    char petitepath[PATH_MAX];
    char schemepath[PATH_MAX];
    char progpath[PATH_MAX];
    char srcdir[PATH_MAX];
    int bad_version;

    snprintf(petitepath, sizeof(petitepath), "%s/petite.boot", libdir);
    snprintf(schemepath, sizeof(schemepath), "%s/scheme.boot", libdir);
    snprintf(progpath, sizeof(progpath), "%s/hafod.so", libdir);
    snprintf(srcdir, sizeof(srcdir), "%s/src", libdir);

    if (access(petitepath, R_OK) != 0) {
        fprintf(stderr, "hafod: boot file not found: %s\n", petitepath);
        return 1;
    }
    if (access(progpath, R_OK) != 0) {
        fprintf(stderr, "hafod: program not found: %s\n", progpath);
        return 1;
    }

    bad_version = check_chez_version(libdir);
    if (bad_version != 0)
        return bad_version;

    Sscheme_init(0);
    Sregister_boot_file(petitepath);
    /* scheme.boot provides the compiler, needed for foreign-procedure
     * when libraries are loaded at runtime via eval/import. */
    if (access(schemepath, R_OK) == 0)
        Sregister_boot_file(schemepath);
    Sbuild_heap(0, custom_init);

    /* Set library-directories so (import (hafod)) finds compiled libs.
     * Must be done after Sbuild_heap, before Sscheme_program.
     * Format: ((srcdir . srcdir)) */
    {
        ptr pair = Scons(Sstring(srcdir), Sstring(srcdir));
        ptr dirs = Scons(pair, Snil);
        Scall1(Stop_level_value(Sstring_to_symbol("library-directories")), dirs);
    }

    int ret = Sscheme_program(progpath, argc, argv);
    Sscheme_deinit();
    return ret;
}
