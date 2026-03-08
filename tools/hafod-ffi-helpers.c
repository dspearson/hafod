/*
 * hafod-ffi-helpers.c -- Non-variadic wrappers for variadic POSIX functions.
 *
 * On ARM64 macOS, variadic arguments use a different calling convention
 * (passed on the stack) than regular arguments (passed in registers).
 * Chez Scheme's foreign-procedure always uses the non-variadic convention,
 * so calling variadic C functions like open(), fcntl(), and ioctl() directly
 * via FFI breaks on ARM64 macOS.
 *
 * These thin wrappers have fixed signatures, so Chez can call them correctly
 * on all platforms.
 *
 * Build: cc -shared -o hafod-ffi-helpers.so tools/hafod-ffi-helpers.c
 *   (macOS: cc -dynamiclib -o hafod-ffi-helpers.dylib tools/hafod-ffi-helpers.c)
 */

#include <fcntl.h>
#include <sys/ioctl.h>

int hafod_open3(const char *path, int flags, int mode) {
    return open(path, flags, mode);
}

int hafod_fcntl_int(int fd, int cmd, int arg) {
    return fcntl(fd, cmd, arg);
}

int hafod_fcntl_void(int fd, int cmd) {
    return fcntl(fd, cmd);
}

int hafod_ioctl_int(int fd, unsigned long request, int arg) {
    return ioctl(fd, request, arg);
}

int hafod_ioctl_ptr(int fd, unsigned long request, void *arg) {
    return ioctl(fd, request, arg);
}
