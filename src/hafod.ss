#!chezscheme
;;; (hafod) -- Umbrella library re-exporting all hafod subsystem bindings
;;; Users can write (import (hafod)) instead of importing 20+ individual libraries.
;;; Note: #!chezscheme required for || symbol re-export from (hafod syntax).
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod)
  (export
    ;; === (hafod compat) ===
    receive let-optionals let-optionals* :optional
    define-simple-syntax check-arg stringify
    mapv mapv! vector-every? copy-vector initialize-vector vector-append
    vfold vfold-right bogus-substring-spec? deprecated-proc real->exact-integer
    arithmetic-shift
    ;; scsh utility functions
    warn ascii->char char->ascii ->char-set
    ;; scsh char predicates
    char-letter? char-digit? char-letter+digit? char-graphic?
    char-printing? char-blank? char-iso-control? char-punctuation?
    char-symbol? char-hex-digit? char-ascii? char-alphanumeric?
    ;; SRFI-14 char-set re-exports
    char-set? char-set char-set-contains?
    char-set:letter char-set:digit char-set:letter+digit
    char-set:lower-case char-set:upper-case
    char-set:punctuation char-set:graphic char-set:printing
    char-set:control char-set:hex-digit char-set:blank
    char-set:ascii char-set:whitespace char-set:newline char-set:any
    char-set:full char-set:empty
    char-set-complement char-set-union char-set-intersection
    char-set-difference char-set-fold char-set-size char-set->list
    char-set= char-set-empty? char-set-full?
    char-set-copy char-set-adjoin char-set-adjoin! char-set-delete
    string->char-set

    ;; === (hafod fname) ===
    file-name-directory? file-name-non-directory?
    file-name-as-directory directory-as-file-name
    ensure-file-name-is-directory ensure-file-name-is-nondirectory
    file-name-absolute? file-name-directory file-name-nondirectory
    split-file-name path-list->file-name parse-file-name
    file-name-extension-index file-name-sans-extension
    file-name-extension replace-extension simplify-file-name

    ;; === (hafod command-line) ===
    command-line-arguments command-line arg arg* argv
    set-command-line-args!

    ;; === (hafod rdelim) ===
    read-line read-delimited read-delimited!
    %read-delimited! read-paragraph skip-char-set

    ;; === (hafod signal) ===
    signal-process signal-process-group pause signal
    SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE SIGKILL
    SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGCHLD SIGCONT
    SIGSTOP SIGTSTP SIGTTIN SIGTTOU
    SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGPWR SIGSYS

    ;; === (hafod user-group) ===
    user-info user-info? group-info ->uid ->username ->gid ->groupname
    home-directory home-dir home-file
    %homedir init-home-directory
    ;; scsh-compatible accessors
    user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
    group-info:name group-info:gid group-info:members
    name->user-info

    ;; === (hafod fname-system) ===
    ;; home-directory excluded (already from user-group)
    resolve-tilde-file-name resolve-file-name expand-file-name
    absolute-file-name substitute-env-vars

    ;; === (hafod posix) ===
    ;; Signal constants excluded (already from signal)
    ;; stat-info accessors excluded (already from fileinfo)
    ;; wait status accessors excluded (already from procobj)
    &posix-error make-posix-error posix-error? posix-errno posix-syscall
    raise-posix-error posix-call
    __errno_location c-strerror
    posix-fork posix-_exit posix-exec posix-waitpid posix-pipe
    posix-dup posix-dup2 posix-close posix-open posix-read posix-write
    posix-kill posix-pause
    O_RDONLY O_WRONLY O_RDWR O_CREAT O_EXCL O_TRUNC O_APPEND O_NONBLOCK
    S_IFMT S_IFDIR S_IFREG S_IFLNK S_IFIFO S_IFSOCK S_IFBLK S_IFCHR
    S_ISUID S_ISGID S_ISVTX
    S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IXGRP S_IROTH S_IWOTH S_IXOTH
    F_GETFD F_SETFD FD_CLOEXEC F_GETFL F_SETFL
    SEEK_SET SEEK_CUR SEEK_END R_OK W_OK X_OK F_OK
    ptr->string strings->c-argv free-c-argv
    posix-stat posix-lstat posix-fstat
    stat-info-rdev stat-info-blksize stat-info-blocks
    mode->type posix-chmod posix-fchmod posix-chown posix-fchown
    posix-truncate posix-ftruncate posix-link posix-symlink posix-readlink
    posix-unlink posix-rename posix-mkdir posix-rmdir posix-access
    posix-lseek posix-umask posix-chdir posix-getcwd
    posix-getenv posix-setenv posix-unsetenv
    posix-getpid posix-getppid posix-getpgrp posix-setpgid posix-setsid
    posix-getuid posix-getgid posix-geteuid posix-getegid
    posix-setuid posix-setgid posix-seteuid posix-setegid
    posix-getpwnam posix-getpwuid posix-getgrnam posix-getgrgid
    passwd-info? passwd-info-name passwd-info-passwd passwd-info-uid
    passwd-info-gid passwd-info-gecos passwd-info-dir passwd-info-shell
    group-info? group-info-name group-info-passwd group-info-gid
    group-info-members
    posix-opendir posix-readdir posix-closedir posix-fcntl posix-getgroups
    read-environ posix-execve posix-sleep posix-mkstemp posix-utimes
    posix-fnmatch FNM_PERIOD FNM_PATHNAME FNM_NOESCAPE
    posix-localtime posix-gmtime posix-mktime posix-strftime
    posix-time posix-getlogin posix-times posix-sysconf posix-gettimeofday
    posix-mkfifo posix-fsync posix-sync posix-uname
    posix-regcomp posix-regexec posix-regfree posix-regerror
    REG_EXTENDED REG_ICASE REG_NOSUB REG_NEWLINE REG_NOTBOL REG_NOTEOL REG_NOMATCH
    posix-tcgetattr posix-tcsetattr posix-isatty posix-ttyname posix-ctermid
    posix-tcsendbreak posix-tcdrain posix-tcflush posix-tcflow
    posix-tcsetpgrp posix-tcgetpgrp
    TCSANOW TCSADRAIN TCSAFLUSH TCIFLUSH TCOFLUSH TCIOFLUSH
    TCOOFF TCOON TCIOFF TCION

    ;; === (hafod fd-ports) ===
    fdes->inport fdes->outport port->fdes release-port-handle
    port-revealed call/fdes sleazy-call/fdes fdport? open-fdport?
    fd/port? close close-fdes close-after pipe flush-all-ports
    flush-all-ports-no-threads init-fdports! drain-port-guardian!
    move->fdes dup dup->fdes dup->inport dup->outport
    open-file open-input-file open-output-file
    open/read open/write open/read+write open/create open/truncate
    open/append open/exclusive open/non-blocking
    with-current-input-port with-current-output-port with-current-error-port
    with-current-input-port* with-current-output-port* with-current-error-port*
    shell-open seek tell seek/set seek/delta seek/end
    error-output-port
    with-error-output-port with-error-output-port*
    with-stdio-ports with-stdio-ports*
    create+trunc write+append+create read-only
    force-output
    file-options file-options-on? file-options-union
    set-fdport! delete-fdport! maybe-ref-fdport evict-ports
    make-input-fdport make-output-fdport %set-cloexec
    *fd-table* *port-table* *port-guardian*

    ;; === (hafod procobj) ===
    proc? proc:pid proc:finished? proc:status proc:zombie?
    new-child-proc pid->proc maybe-pid->proc ->proc pid/proc?
    wait wait-any reap-zombies obituary mark-proc-waited!
    wait/poll wait/stopped-children
    status:exit-val status:term-sig status:stop-sig

    ;; === (hafod collect) ===
    run/port+proc* run/port* run/string* run/strings*
    run/sexp* run/sexps* run/collecting* run/file*

    ;; === (hafod process) ===
    fork %fork exec exec-path exec/env exec-path/env
    exec-path-search exec-path-list %exec halts?
    init-exec-path-list exit %exit call-terminally fork/pipe
    %fork/pipe fork/pipe+ %fork/pipe+ pipe* tail-pipe tail-pipe+
    process-sleep process-sleep-until preserve-ports
    split-colon-list suspend

    ;; === (hafod environment) ===
    getenv setenv env->alist alist->env with-env* with-total-env*
    with-env with-total-env environ-resource
    alist-update alist-delete alist->env-list alist-compress
    add-before add-after align-env! read-environ-fresh

    ;; === (hafod glob) ===
    glob glob-quote maybe-directory-files

    ;; === (hafod temp-file) ===
    create-temp-file temp-file-channel temp-file-iterate *temp-file-template*

    ;; === (hafod port-collect) ===
    port->string port->string-list port->sexp-list port->list
    port-fold reduce-port make-char-port-filter make-string-port-filter
    make-string-input-port make-string-output-port
    string-output-port-output call-with-string-output-port

    ;; === (hafod process-state) ===
    cwd chdir with-cwd with-cwd* process-cwd process-chdir
    umask set-umask with-umask with-umask* pid parent-pid
    process-group set-process-group become-session-leader
    user-uid user-effective-uid user-gid user-effective-gid
    user-supplementary-gids set-uid set-gid
    set-user-effective-uid set-user-effective-gid
    with-user-effective-uid with-user-effective-uid*
    with-user-effective-gid with-user-effective-gid*
    user-login-name process-times cpu-ticks/sec
    resource make-resource resource? resource-name resource-align!
    with-resources-aligned cwd-resource umask-resource
    euid-resource egid-resource

    ;; === (hafod fileinfo) ===
    file-info file-info:type file-info:device file-info:inode
    file-info:mode file-info:nlinks file-info:uid file-info:gid
    file-info:size file-info:atime file-info:mtime file-info:ctime
    file-directory? file-regular? file-symlink? file-fifo?
    file-socket? file-special? file-readable? file-writable?
    file-executable? file-exists? file-not-readable?
    file-not-writable? file-not-executable? file-not-exists?
    file-info-directory? file-info-regular? file-info-symlink?
    file-info-fifo? file-info-socket? file-info-special?
    file-info-readable? file-info-writable? file-info-executable?
    file-info-not-readable? file-info-not-writable?
    file-info-not-executable?
    file:type file:group file:owner file:inode file:size file:mode
    file:nlinks file:last-access file:last-mod file:last-status-change
    file-attributes file-writeable? file-not-writeable? file-mode=?
    create-fifo sync-file sync-file-system
    create-directory delete-directory delete-file delete-filesys-object
    create-symlink create-hard-link read-symlink rename-file
    set-file-mode set-file-owner set-file-group set-file-times
    truncate-file directory-files
    open-directory-stream read-directory-stream close-directory-stream
    directory-stream?
    stat-info? stat-info-type stat-info-dev stat-info-ino stat-info-mode
    stat-info-nlink stat-info-uid stat-info-gid stat-info-size
    stat-info-atime stat-info-mtime stat-info-ctime

    ;; === (hafod time) ===
    make-date date? date:seconds date:minute date:hour date:month-day
    date:month date:year date:tz-name date:tz-secs date:summer?
    date:week-day date:year-day
    set-date:seconds set-date:minute set-date:hour set-date:month-day
    set-date:month set-date:year set-date:tz-name set-date:tz-secs
    set-date:summer? set-date:week-day set-date:year-day
    time date date->string format-date
    time+ticks ticks/sec

    ;; === (hafod system) ===
    uname uname-info? uname:os-name uname:node-name uname:release
    uname:version uname:machine errno-error with-errno-handler*
    with-errno-handler hafod-major-version hafod-minor-version
    hafod-version-string
    scsh-major-version scsh-minor-version scsh-version-string
    scsh-release-name system-name

    ;; === (hafod syntax) ===
    ;; with-stdio-ports* excluded (already from fd-ports)
    exec-epf & run || :or: &&
    open-string-source stdports->stdio
    <<-port-holder <<-port-holder-set!
    run/port run/string run/strings run/sexp run/sexps
    run/port+proc run/collecting run/file

    ;; === (hafod re-adt) ===
    re-tsm re-trivial re-trivial? re-empty re-empty?
    re-any re-any? re-nonl re-bol re-bol? re-eol re-eol?
    re-bos re-bos? re-eos re-eos?
    re-string? re-string:chars
    re-seq re-seq? re-seq:elts re-seq:tsm
    re-choice re-choice? re-choice:elts re-choice:tsm
    re-repeat re-repeat? re-repeat:from re-repeat:to re-repeat:body re-repeat:tsm
    re-submatch re-submatch? re-submatch:body re-submatch:pre-dsm re-submatch:post-dsm re-submatch:tsm
    re-dsm re-dsm? re-dsm:body re-dsm:pre-dsm re-dsm:post-dsm re-dsm:tsm
    re-char-set? re-char-set:cset re-char-class?
    make-re-string make-re-seq make-re-seq/tsm make-re-choice make-re-choice/tsm
    make-re-repeat make-re-repeat/tsm make-re-submatch make-re-submatch/tsm
    make-re-dsm make-re-dsm/tsm make-re-char-set
    really-make-re-string really-make-re-seq really-make-re-choice
    really-make-re-repeat really-make-re-submatch really-make-re-dsm
    really-make-re-char-set
    open-dsm static-char-class? simplify-regexp

    ;; === (hafod re) ===
    rx regexp? make-regexp string->regexp regexp-search
    regexp-search? regexp-match match:start match:end
    match:substring match:count regexp-match? regexp-substitute
    regexp-substitute/global regexp-fold regexp-for-each
    let-match if-match match-cond regexp-fold-right
    if-sre-form sre-form?
    sre->regexp regexp->sre posix-string->regexp regexp->posix-string
    flush-submatches uncase uncase-string uncase-char-set

    ;; === (hafod tty) ===
    make-tty-info copy-tty-info tty-info?
    tty-info:control-chars set-tty-info:control-chars
    tty-info:input-flags set-tty-info:input-flags
    tty-info:output-flags set-tty-info:output-flags
    tty-info:control-flags set-tty-info:control-flags
    tty-info:local-flags set-tty-info:local-flags
    tty-info:input-speed set-tty-info:input-speed
    tty-info:output-speed set-tty-info:output-speed
    tty-info:min set-tty-info:min tty-info:time set-tty-info:time
    tty-info set-tty-info/now set-tty-info/drain set-tty-info/flush
    ttychar/eof ttychar/eol ttychar/delete-char ttychar/delete-line
    ttychar/interrupt ttychar/quit ttychar/suspend ttychar/start
    ttychar/stop ttychar/min ttychar/time ttychar/delete-word
    ttychar/reprint ttychar/literal-next ttychar/discard
    ttychar/delayed-suspend ttychar/eol2 ttychar/status
    num-ttychars disable-tty-char
    ttyin/ignore-break ttyin/interrupt-on-break ttyin/ignore-bad-parity-chars
    ttyin/mark-parity-errors ttyin/check-parity ttyin/7bits ttyin/nl->cr
    ttyin/ignore-cr ttyin/cr->nl ttyin/output-flow-ctl ttyin/input-flow-ctl
    ttyin/xon-any ttyin/beep-on-overflow ttyin/lowercase
    ttyout/enable ttyout/nl->crnl ttyout/discard-eot ttyout/expand-tabs
    ttyout/cr->nl ttyout/fill-w/del ttyout/delay-w/fill-char ttyout/uppercase
    ttyout/nl-does-cr ttyout/no-col0-cr
    ttyout/nl-delay ttyout/nl-delay0 ttyout/nl-delay1
    ttyout/tab-delay ttyout/tab-delay0 ttyout/tab-delay1
    ttyout/tab-delay2 ttyout/tab-delayx
    ttyout/cr-delay ttyout/cr-delay0 ttyout/cr-delay1
    ttyout/cr-delay2 ttyout/cr-delay3
    ttyout/vtab-delay ttyout/vtab-delay0 ttyout/vtab-delay1
    ttyout/bs-delay ttyout/bs-delay0 ttyout/bs-delay1
    ttyout/ff-delay ttyout/ff-delay0 ttyout/ff-delay1
    ttyout/all-delay
    ttyc/char-size ttyc/char-size5 ttyc/char-size6 ttyc/char-size7
    ttyc/char-size8 ttyc/2-stop-bits ttyc/enable-read
    ttyc/enable-parity ttyc/odd-parity ttyc/hup-on-close
    ttyc/no-modem-sync ttyc/ignore-flags
    ttyc/CTS-output-flow-ctl ttyc/RTS-input-flow-ctl ttyc/carrier-flow-ctl
    ttyl/visual-delete ttyl/echo-delete-line ttyl/echo ttyl/echo-nl
    ttyl/canonical ttyl/enable-signals ttyl/extended ttyl/ttou-signal
    ttyl/no-flush-on-interrupt ttyl/visual-delete-line ttyl/hardcopy-delete
    ttyl/echo-ctl ttyl/flush-output ttyl/reprint-unread-chars
    ttyl/alt-delete-word ttyl/no-kernel-status ttyl/case-map
    encode-baud-rate decode-baud-rate baud-rates
    tty? tty-file-name control-tty-file-name
    send-tty-break drain-tty flush-tty/input flush-tty/output
    flush-tty/both start-tty-output stop-tty-output
    start-tty-input stop-tty-input
    open-control-tty make-control-tty
    tty-process-group set-tty-process-group

    ;; === (hafod field-reader) ===
    field-splitter infix-splitter suffix-splitter
    sloppy-suffix-splitter record-reader field-reader join-strings

    ;; === (hafod awk) ===
    awk awk/posix-string next-range next-:range next-range: next-:range:

    ;; === (hafod pty) ===
    open-pty fork-pty-session pty-name->tty-name
    tty-name->pty-name make-pty-generator

    ;; === (chezscheme) re-exports for scsh compatibility ===
    bitwise-and bitwise-ior bitwise-xor bitwise-not arithmetic-shift

    ;; === (hafod exit-hooks) ===
    add-exit-hook! call-exit-hooks-and-run call-exit-hooks!

    ;; === (hafod dot-locking) ===
    obtain-dot-lock release-dot-lock break-dot-lock
    with-dot-lock with-dot-lock*

    ;; === (hafod lib-dirs) ===
    lib-dirs lib-dirs-prepend! lib-dirs-append!
    reset-lib-dirs! clear-lib-dirs! find-library-file
    default-lib-dirs lib-dirs-append-script-dir! lib-dirs-prepend-script-dir!

    ;; === (hafod process) — fast spawn ===
    spawn-program spawn-program/pipe

    ;; === (hafod threads) ===
    spawn thread? thread-name current-thread
    yield thread-sleep thread-join thread-terminate!
    make-channel channel? channel-send channel-receive
    channel-try-send channel-try-receive channel-close channel-closed?
    make-thread-local thread-local-ref thread-local-set!
    run-threads threads-start! threads-shutdown! threads-running?

    ;; === (chezscheme) re-exports for scsh compatibility ===
    error current-error-port
    write display newline read-char write-char char-ready?
    input-port? output-port?
    call-with-input-file call-with-output-file
    with-input-from-file with-output-to-file
    format
    ;; R5RS char predicates re-export
    char-alphabetic? char-numeric? char-whitespace?
    char-lower-case? char-upper-case?
    ;; R5RS string/char re-exports
    char->integer integer->char
    number->string string->number
    string-copy string-append substring
    string->list list->string
    string-upcase string-downcase)

  (import
    (only (chezscheme) bitwise-and bitwise-ior bitwise-xor bitwise-not
      ;; scsh standard re-exports
      error current-error-port
      write display newline read-char write-char char-ready?
      input-port? output-port?
      call-with-input-file call-with-output-file
      with-input-from-file with-output-to-file
      format
      ;; R5RS char predicates
      char-alphabetic? char-numeric? char-whitespace?
      char-lower-case? char-upper-case?
      ;; R5RS string/char re-exports
      char->integer integer->char
      number->string string->number
      string-copy string-append substring
      string->list list->string
      string-upcase string-downcase)
    (hafod compat)
    (hafod fname)
    (hafod command-line)
    (hafod rdelim)
    (hafod signal)
    (hafod user-group)
    (except (hafod fname-system) home-directory)
    (except (hafod posix)
      ;; Signal constants (from hafod signal)
      SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE SIGKILL
      SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGCHLD SIGCONT
      SIGSTOP SIGTSTP SIGTTIN SIGTTOU
      SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGPWR SIGSYS
      ;; stat-info accessors (from hafod fileinfo)
      stat-info? stat-info-type stat-info-dev stat-info-ino stat-info-mode
      stat-info-nlink stat-info-uid stat-info-gid stat-info-size
      stat-info-atime stat-info-mtime stat-info-ctime
      ;; wait status accessors (from hafod procobj)
      status:exit-val status:term-sig status:stop-sig
      wait/poll wait/stopped-children)
    (hafod fd-ports)
    (hafod procobj)
    (hafod collect)
    (hafod process)
    (hafod environment)
    (hafod glob)
    (hafod temp-file)
    (hafod port-collect)
    (hafod process-state)
    (hafod fileinfo)
    (hafod time)
    (hafod system)
    (except (hafod syntax) with-stdio-ports*)
    (except (hafod re-adt)
      ;; Already exported via (hafod re)
      flush-submatches regexp->posix-string regexp? uncase uncase-char-set uncase-string)
    (hafod re)
    (hafod tty)
    (hafod field-reader)
    (hafod awk)
    (hafod pty)
    (hafod exit-hooks)
    (hafod dot-locking)
    (hafod lib-dirs)
    (hafod threads))

  ;; No body needed -- this library is a pure re-export aggregator.
  ;; Body definitions live in sub-libraries to avoid import conflicts.
)
