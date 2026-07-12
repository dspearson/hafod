/* probe-platform-constants.c -- Independent cross-check of the platform constants.
 *
 * A second, separately authored witness to the platform ABI facts recorded in
 * the generated (hafod internal platform-constants) library. Every value here is
 * computed directly from the constant's NAME -- an offsetof, a sizeof, or a
 * system #define resolved through the real headers -- rather than by reusing the
 * mappings of tools/gen-platform-constants.c. Because the two paths are derived
 * independently, a wrong field, a wrong signed/unsigned category, a wrong name,
 * or a wrongly hardcoded literal in the generator surfaces here as a value that
 * disagrees with the committed library.
 *
 * It prints exactly one canonical "NAME VALUE" line per constant to stdout and
 * nothing else (no headers, blank lines, or commentary), so a comparator can
 * read the output as plain data and assert agreement, naming any constant that
 * differs. Signed facts are printed with %ld, unsigned flag masks with %lu, and
 * every value is widened to long / unsigned long so no wide bit is truncated.
 *
 * Build and run with the same C toolchain the library is compiled against:
 *   cc -Wall -o probe-platform-constants probe-platform-constants.c
 *   ./probe-platform-constants
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <termios.h>
#include <time.h>
#include <sys/times.h>
#include <regex.h>
#include <glob.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <spawn.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/utsname.h>
#include <locale.h>
#include <fnmatch.h>   /* the probe resolves the real FNM_* macros, not literals */

/* Emission helpers -- the bare "NAME VALUE" twin of the generator's EMIT_*
 * family. They agree only on the wire format (the %ld / %lu signed-vs-unsigned
 * split and the long / unsigned long widening); the field and macro each line
 * names is re-derived from the constant NAME, never copied from the generator.
 *
 * Signed family (%ld / long): offsets, whole-type and field sizes, signal
 * numbers, cc indices, termios actions, fcntl, the utsname field length,
 * LC_ALL, and the -1 sentinels (which must stay signed so they never wrap). */
#define P_OFF(name, type, field) printf("%s %ld\n", name, (long)offsetof(type, field))
#define P_SZ(name, type)         printf("%s %ld\n", name, (long)sizeof(type))
#define P_FSZ(name, type, field) printf("%s %ld\n", name, (long)sizeof(((type *)0)->field))
#define P_S(name, val)           printf("%s %ld\n", name, (long)(val))
/* Unsigned family (%lu / unsigned long): open flags, every TTY mask, the two
 * ioctls, O_NOCTTY, the FNM flags, and the regex flags (a high bit may be set). */
#define P_U(name, val)           printf("%s %lu\n", name, (unsigned long)(val))

int main(void) {
    /* struct stat -- whole size, field offsets, and the seven field sizes. */
    P_SZ ("SIZEOF-STAT", struct stat);
    P_OFF("STAT-ST-DEV",     struct stat, st_dev);
    P_OFF("STAT-ST-INO",     struct stat, st_ino);
    P_OFF("STAT-ST-NLINK",   struct stat, st_nlink);
    P_OFF("STAT-ST-MODE",    struct stat, st_mode);
    P_OFF("STAT-ST-UID",     struct stat, st_uid);
    P_OFF("STAT-ST-GID",     struct stat, st_gid);
    P_OFF("STAT-ST-RDEV",    struct stat, st_rdev);
    P_OFF("STAT-ST-SIZE",    struct stat, st_size);
    P_OFF("STAT-ST-BLKSIZE", struct stat, st_blksize);
    P_OFF("STAT-ST-BLOCKS",  struct stat, st_blocks);
    /* The three timespec fields are spelt st_*timespec on Apple, st_*tim elsewhere. */
#ifdef __APPLE__
    P_OFF("STAT-ST-ATIM", struct stat, st_atimespec);
    P_OFF("STAT-ST-MTIM", struct stat, st_mtimespec);
    P_OFF("STAT-ST-CTIM", struct stat, st_ctimespec);
#else
    P_OFF("STAT-ST-ATIM", struct stat, st_atim);
    P_OFF("STAT-ST-MTIM", struct stat, st_mtim);
    P_OFF("STAT-ST-CTIM", struct stat, st_ctim);
#endif
    P_FSZ("SIZEOF-ST-DEV",     struct stat, st_dev);
    P_FSZ("SIZEOF-ST-INO",     struct stat, st_ino);
    P_FSZ("SIZEOF-ST-NLINK",   struct stat, st_nlink);
    P_FSZ("SIZEOF-ST-MODE",    struct stat, st_mode);
    P_FSZ("SIZEOF-ST-RDEV",    struct stat, st_rdev);
    P_FSZ("SIZEOF-ST-BLKSIZE", struct stat, st_blksize);
    P_FSZ("SIZEOF-ST-BLOCKS",  struct stat, st_blocks);

    /* struct dirent -- d_off exists only on Linux; elsewhere it is the -1 sentinel. */
    P_OFF("DIRENT-D-INO", struct dirent, d_ino);
#ifdef __linux__
    P_OFF("DIRENT-D-OFF", struct dirent, d_off);
#else
    P_S("DIRENT-D-OFF", -1);
#endif
    P_OFF("DIRENT-D-RECLEN", struct dirent, d_reclen);
    P_OFF("DIRENT-D-TYPE",   struct dirent, d_type);
    P_OFF("DIRENT-D-NAME",   struct dirent, d_name);

    /* struct passwd */
    P_OFF("PW-NAME",   struct passwd, pw_name);
    P_OFF("PW-PASSWD", struct passwd, pw_passwd);
    P_OFF("PW-UID",    struct passwd, pw_uid);
    P_OFF("PW-GID",    struct passwd, pw_gid);
    P_OFF("PW-GECOS",  struct passwd, pw_gecos);
    P_OFF("PW-DIR",    struct passwd, pw_dir);
    P_OFF("PW-SHELL",  struct passwd, pw_shell);

    /* struct group */
    P_OFF("GR-NAME",   struct group, gr_name);
    P_OFF("GR-PASSWD", struct group, gr_passwd);
    P_OFF("GR-GID",    struct group, gr_gid);
    P_OFF("GR-MEM",    struct group, gr_mem);

    /* struct termios */
    P_SZ ("SIZEOF-TERMIOS", struct termios);
    P_OFF("TERMIOS-C-IFLAG",  struct termios, c_iflag);
    P_OFF("TERMIOS-C-OFLAG",  struct termios, c_oflag);
    P_OFF("TERMIOS-C-CFLAG",  struct termios, c_cflag);
    P_OFF("TERMIOS-C-LFLAG",  struct termios, c_lflag);
    P_OFF("TERMIOS-C-CC",     struct termios, c_cc);
    P_OFF("TERMIOS-C-ISPEED", struct termios, c_ispeed);
    P_OFF("TERMIOS-C-OSPEED", struct termios, c_ospeed);

    /* struct tm */
    P_SZ ("SIZEOF-TM", struct tm);
    P_OFF("TM-SEC",    struct tm, tm_sec);
    P_OFF("TM-MIN",    struct tm, tm_min);
    P_OFF("TM-HOUR",   struct tm, tm_hour);
    P_OFF("TM-MDAY",   struct tm, tm_mday);
    P_OFF("TM-MON",    struct tm, tm_mon);
    P_OFF("TM-YEAR",   struct tm, tm_year);
    P_OFF("TM-WDAY",   struct tm, tm_wday);
    P_OFF("TM-YDAY",   struct tm, tm_yday);
    P_OFF("TM-ISDST",  struct tm, tm_isdst);
    P_OFF("TM-GMTOFF", struct tm, tm_gmtoff);
    P_OFF("TM-ZONE",   struct tm, tm_zone);

    /* struct tms */
    P_SZ("SIZEOF-TMS", struct tms);

    /* regex_t, regmatch_t, regoff_t */
    P_SZ("SIZEOF-REGEX-T",    regex_t);
    P_SZ("SIZEOF-REGMATCH-T", regmatch_t);
    P_SZ("SIZEOF-REGOFF-T",   regoff_t);

    /* glob_t */
    P_SZ ("SIZEOF-GLOB-T", glob_t);
    P_OFF("GLOB-GL-PATHC", glob_t, gl_pathc);
    P_OFF("GLOB-GL-PATHV", glob_t, gl_pathv);

    /* posix_spawn_file_actions_t */
    P_SZ("SIZEOF-SPAWN-FA", posix_spawn_file_actions_t);

    /* struct winsize */
    P_SZ ("SIZEOF-WINSZ", struct winsize);
    P_OFF("WINSZ-WS-ROW", struct winsize, ws_row);
    P_OFF("WINSZ-WS-COL", struct winsize, ws_col);

    /* Open flags -- unsigned masks. */
    P_U("PLAT-O-CREAT",    O_CREAT);
    P_U("PLAT-O-EXCL",     O_EXCL);
    P_U("PLAT-O-TRUNC",    O_TRUNC);
    P_U("PLAT-O-APPEND",   O_APPEND);
    P_U("PLAT-O-NONBLOCK", O_NONBLOCK);

    /* Signal numbers -- signed; their values shift between platforms. */
    P_S("PLAT-SIGBUS",    SIGBUS);
    P_S("PLAT-SIGUSR1",   SIGUSR1);
    P_S("PLAT-SIGUSR2",   SIGUSR2);
    P_S("PLAT-SIGCHLD",   SIGCHLD);
    P_S("PLAT-SIGCONT",   SIGCONT);
    P_S("PLAT-SIGSTOP",   SIGSTOP);
    P_S("PLAT-SIGTSTP",   SIGTSTP);
    P_S("PLAT-SIGTTIN",   SIGTTIN);
    P_S("PLAT-SIGTTOU",   SIGTTOU);
    P_S("PLAT-SIGURG",    SIGURG);
    P_S("PLAT-SIGXCPU",   SIGXCPU);
    P_S("PLAT-SIGXFSZ",   SIGXFSZ);
    P_S("PLAT-SIGVTALRM", SIGVTALRM);
    P_S("PLAT-SIGPROF",   SIGPROF);
    P_S("PLAT-SIGWINCH",  SIGWINCH);
    P_S("PLAT-SIGIO",     SIGIO);
    P_S("PLAT-SIGSYS",    SIGSYS);
    /* Platform-varying signals -- the -1 sentinel stays on the signed path so it
       never wraps to ULONG_MAX under %lu. */
#ifdef SIGPWR
    P_S("PLAT-SIGPWR", SIGPWR);
#else
    P_S("PLAT-SIGPWR", -1);
#endif
#ifdef SIGINFO
    P_S("PLAT-SIGINFO", SIGINFO);
#else
    P_S("PLAT-SIGINFO", -1);
#endif

    /* errno constants -- signed; EINTR is a small positive int on every platform.
       Named-keyed against the generator, so this line must match it exactly. */
    P_S("PLAT-EINTR", EINTR);

    /* TTY input flags -- unsigned masks. */
    P_U("PLAT-IGNBRK", IGNBRK);
    P_U("PLAT-BRKINT", BRKINT);
    P_U("PLAT-IGNPAR", IGNPAR);
    P_U("PLAT-PARMRK", PARMRK);
    P_U("PLAT-INPCK",  INPCK);
    P_U("PLAT-ISTRIP", ISTRIP);
    P_U("PLAT-INLCR",  INLCR);
    P_U("PLAT-IGNCR",  IGNCR);
    P_U("PLAT-ICRNL",  ICRNL);
    P_U("PLAT-IXON",   IXON);
    P_U("PLAT-IXOFF",  IXOFF);
    P_U("PLAT-IXANY",  IXANY);

    /* TTY output flags -- unsigned masks. */
    P_U("PLAT-OPOST", OPOST);
    P_U("PLAT-ONLCR", ONLCR);

    /* TTY control flags -- unsigned masks. */
    P_U("PLAT-CSIZE",  CSIZE);
    P_U("PLAT-CS5",    CS5);
    P_U("PLAT-CS6",    CS6);
    P_U("PLAT-CS7",    CS7);
    P_U("PLAT-CS8",    CS8);
    P_U("PLAT-CSTOPB", CSTOPB);
    P_U("PLAT-CREAD",  CREAD);
    P_U("PLAT-PARENB", PARENB);
    P_U("PLAT-PARODD", PARODD);
    P_U("PLAT-HUPCL",  HUPCL);
    P_U("PLAT-CLOCAL", CLOCAL);

    /* TTY local flags -- unsigned masks. */
    P_U("PLAT-ISIG",   ISIG);
    P_U("PLAT-ICANON", ICANON);
    P_U("PLAT-ECHO",   ECHO);
    P_U("PLAT-ECHOE",  ECHOE);
    P_U("PLAT-ECHOK",  ECHOK);
    P_U("PLAT-ECHONL", ECHONL);
    P_U("PLAT-NOFLSH", NOFLSH);
    P_U("PLAT-TOSTOP", TOSTOP);
    P_U("PLAT-IEXTEN", IEXTEN);

    /* TTY control-character indices -- signed positions into c_cc. */
    P_S("PLAT-VINTR",    VINTR);
    P_S("PLAT-VQUIT",    VQUIT);
    P_S("PLAT-VERASE",   VERASE);
    P_S("PLAT-VKILL",    VKILL);
    P_S("PLAT-VEOF",     VEOF);
    P_S("PLAT-VTIME",    VTIME);
    P_S("PLAT-VMIN",     VMIN);
    P_S("PLAT-VSTART",   VSTART);
    P_S("PLAT-VSTOP",    VSTOP);
    P_S("PLAT-VSUSP",    VSUSP);
    P_S("PLAT-VEOL",     VEOL);
    P_S("PLAT-VREPRINT", VREPRINT);
    P_S("PLAT-VDISCARD", VDISCARD);
    P_S("PLAT-VWERASE",  VWERASE);
    P_S("PLAT-VLNEXT",   VLNEXT);
    P_S("PLAT-VEOL2",    VEOL2);
    P_S("PLAT-NCCS",     NCCS);

    /* termios actions -- signed enums. */
    P_S("PLAT-TCSANOW",   TCSANOW);
    P_S("PLAT-TCSADRAIN", TCSADRAIN);
    P_S("PLAT-TCSAFLUSH", TCSAFLUSH);
    P_S("PLAT-TCIFLUSH",  TCIFLUSH);
    P_S("PLAT-TCOFLUSH",  TCOFLUSH);
    P_S("PLAT-TCIOFLUSH", TCIOFLUSH);
    P_S("PLAT-TCOOFF",    TCOOFF);
    P_S("PLAT-TCOON",     TCOON);
    P_S("PLAT-TCIOFF",    TCIOFF);
    P_S("PLAT-TCION",     TCION);

    /* fcntl commands and the close-on-exec flag -- signed. */
    P_S("PLAT-F-GETFD",    F_GETFD);
    P_S("PLAT-F-SETFD",    F_SETFD);
    P_S("PLAT-FD-CLOEXEC", FD_CLOEXEC);
    P_S("PLAT-F-GETFL",    F_GETFL);
    P_S("PLAT-F-SETFL",    F_SETFL);

    /* ioctl request numbers -- unsigned (the _IOC encoding may set a high bit). */
    P_U("PLAT-TIOCSCTTY",  TIOCSCTTY);
    P_U("PLAT-TIOCGWINSZ", TIOCGWINSZ);

    /* utsname field length -- signed; the size of one fixed-length name buffer. */
    P_FSZ("PLAT-UTSNAME-FIELD-LEN", struct utsname, sysname);

    /* O_NOCTTY -- unsigned mask. */
    P_U("PLAT-O-NOCTTY", O_NOCTTY);

    /* fnmatch flags -- resolved from <fnmatch.h> rather than assumed, so a host
       whose real macro disagrees with the recorded value is exposed. */
    P_U("PLAT-FNM-NOESCAPE", FNM_NOESCAPE);
    P_U("PLAT-FNM-PATHNAME", FNM_PATHNAME);
    P_U("PLAT-FNM-PERIOD",   FNM_PERIOD);

    /* Regex flags -- unsigned masks. */
    P_U("PLAT-REG-EXTENDED", REG_EXTENDED);
    P_U("PLAT-REG-ICASE",    REG_ICASE);
    P_U("PLAT-REG-NOSUB",    REG_NOSUB);
    P_U("PLAT-REG-NEWLINE",  REG_NEWLINE);
    P_U("PLAT-REG-NOTBOL",   REG_NOTBOL);
    P_U("PLAT-REG-NOTEOL",   REG_NOTEOL);
    P_U("PLAT-REG-NOMATCH",  REG_NOMATCH);
    P_U("PLAT-REG-STARTEND", REG_STARTEND);

    /* Locale category -- signed. */
    P_S("PLAT-LC-ALL", LC_ALL);
    P_S("PLAT-LC-COLLATE", LC_COLLATE);

    return 0;
}
