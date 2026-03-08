/* hafod.c -- Native launcher for hafod (scsh on Chez Scheme)
 *
 * A tiny C trampoline that initialises the Chez Scheme runtime and
 * loads the pre-compiled hafod program.  Replaces the shell wrapper,
 * eliminating one fork+exec and the shell parse overhead.
 *
 * At install time, LIBDIR is baked in (e.g. /usr/local/lib/hafod).
 * The binary expects:
 *   LIBDIR/petite.boot   -- Chez Scheme boot file (or symlink)
 *   LIBDIR/hafod.so      -- compiled hafod launcher program
 *   LIBDIR/src/          -- compiled hafod libraries
 */

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

int main(int argc, const char **argv) {
    const char *libdir = find_libdir(argv[0]);

    char bootpath[PATH_MAX];
    char progpath[PATH_MAX];
    char srcdir[PATH_MAX];

    snprintf(bootpath, sizeof(bootpath), "%s/petite.boot", libdir);
    snprintf(progpath, sizeof(progpath), "%s/hafod.so", libdir);
    snprintf(srcdir, sizeof(srcdir), "%s/src", libdir);

    if (access(bootpath, R_OK) != 0) {
        fprintf(stderr, "hafod: boot file not found: %s\n", bootpath);
        return 1;
    }
    if (access(progpath, R_OK) != 0) {
        fprintf(stderr, "hafod: program not found: %s\n", progpath);
        return 1;
    }

    Sscheme_init(0);
    Sregister_boot_file(bootpath);
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
