#!chezscheme
;;; (scsh) -- Compatibility alias for (hafod)
;;; Allows (import (scsh)) as an alternative to (import (hafod)).
;;; AUTO-GENERATED -- 1026 symbols re-exported from (hafod).

(library (scsh)
  (export  %exec %exit %fork %fork/pipe %fork/pipe+ %homedir %read-delimited! %set-cloexec & 
    && &posix-error *fd-table* *port-guardian* *port-table* *temp-file-template* ->char-set 
    ->gid ->groupname ->proc ->uid ->username :optional :or: <<-port-holder 
    <<-port-holder-set! FD_CLOEXEC FNM_NOESCAPE FNM_PATHNAME FNM_PERIOD F_GETFD F_GETFL F_OK 
    F_SETFD F_SETFL O_APPEND O_CREAT O_EXCL O_NONBLOCK O_RDONLY O_RDWR O_TRUNC O_WRONLY 
    REG_EXTENDED REG_ICASE REG_NEWLINE REG_NOMATCH REG_NOSUB REG_NOTBOL REG_NOTEOL R_OK 
    SEEK_CUR SEEK_END SEEK_SET SIGABRT SIGALRM SIGBUS SIGCHLD SIGCONT SIGFPE SIGHUP SIGILL 
    SIGINT SIGIO SIGKILL SIGPIPE SIGPROF SIGPWR SIGQUIT SIGSEGV SIGSTOP SIGSYS SIGTERM 
    SIGTRAP SIGTSTP SIGTTIN SIGTTOU SIGURG SIGUSR1 SIGUSR2 SIGVTALRM SIGWINCH SIGXCPU SIGXFSZ 
    S_IFBLK S_IFCHR S_IFDIR S_IFIFO S_IFLNK S_IFMT S_IFREG S_IFSOCK S_IRGRP S_IROTH S_IRUSR 
    S_ISGID S_ISUID S_ISVTX S_IWGRP S_IWOTH S_IWUSR S_IXGRP S_IXOTH S_IXUSR TCIFLUSH TCIOFF 
    TCIOFLUSH TCION TCOFLUSH TCOOFF TCOON TCSADRAIN TCSAFLUSH TCSANOW W_OK X_OK 
    __errno_location absolute-file-name add-after add-before add-exit-hook! align-env! 
    alist->env alist->env-list alist-compress alist-delete alist-update arg arg* argv 
    arithmetic-shift ascii->char awk awk/posix-string baud-rates become-session-leader 
    bitwise-and bitwise-ior bitwise-not bitwise-xor bogus-substring-spec? break-dot-lock 
    c-strerror call-exit-hooks! call-exit-hooks-and-run call-terminally call-with-input-file 
    call-with-output-file call-with-string-output-port call/fdes channel-close 
    channel-closed? channel-receive channel-send channel-try-receive channel-try-send 
    channel? char->ascii char->integer char-alphabetic? char-alphanumeric? char-ascii? 
    char-blank? char-digit? char-graphic? char-hex-digit? char-iso-control? 
    char-letter+digit? char-letter? char-lower-case? char-numeric? char-printing? 
    char-punctuation? char-ready? char-set char-set->list char-set-adjoin char-set-adjoin! 
    char-set-complement char-set-contains? char-set-copy char-set-delete char-set-difference 
    char-set-empty? char-set-fold char-set-full? char-set-intersection char-set-size 
    char-set-union char-set:any char-set:ascii char-set:blank char-set:control char-set:digit 
    char-set:empty char-set:full char-set:graphic char-set:hex-digit char-set:letter 
    char-set:letter+digit char-set:lower-case char-set:newline char-set:printing 
    char-set:punctuation char-set:upper-case char-set:whitespace char-set= char-set? 
    char-symbol? char-upper-case? char-whitespace? chdir check-arg clear-lib-dirs! close 
    close-after close-directory-stream close-fdes command-line command-line-arguments 
    control-tty-file-name copy-tty-info copy-vector cpu-ticks/sec create+trunc 
    create-directory create-fifo create-hard-link create-symlink create-temp-file 
    current-error-port current-thread cwd cwd-resource date date->string date:hour 
    date:minute date:month date:month-day date:seconds date:summer? date:tz-name date:tz-secs 
    date:week-day date:year date:year-day date? decode-baud-rate default-lib-dirs 
    define-simple-syntax delete-directory delete-fdport! delete-file delete-filesys-object 
    deprecated-proc directory-as-file-name directory-files directory-stream? disable-tty-char 
    display drain-port-guardian! drain-tty dup dup->fdes dup->inport dup->outport 
    egid-resource encode-baud-rate ensure-file-name-is-directory 
    ensure-file-name-is-nondirectory env->alist environ-resource errno-error error 
    error-output-port euid-resource evict-ports exec exec-epf exec-path exec-path-list 
    exec-path-search exec-path/env exec/env exit expand-file-name fd/port? fdes->inport 
    fdes->outport fdport? field-reader field-splitter file-attributes file-directory? 
    file-executable? file-exists? file-fifo? file-info file-info-directory? 
    file-info-executable? file-info-fifo? file-info-not-executable? file-info-not-readable? 
    file-info-not-writable? file-info-readable? file-info-regular? file-info-socket? 
    file-info-special? file-info-symlink? file-info-writable? file-info:atime file-info:ctime 
    file-info:device file-info:gid file-info:inode file-info:mode file-info:mtime 
    file-info:nlinks file-info:size file-info:type file-info:uid file-mode=? 
    file-name-absolute? file-name-as-directory file-name-directory file-name-directory? 
    file-name-extension file-name-extension-index file-name-non-directory? 
    file-name-nondirectory file-name-sans-extension file-not-executable? file-not-exists? 
    file-not-readable? file-not-writable? file-not-writeable? file-options file-options-on? 
    file-options-union file-readable? file-regular? file-socket? file-special? file-symlink? 
    file-writable? file-writeable? file:group file:inode file:last-access file:last-mod 
    file:last-status-change file:mode file:nlinks file:owner file:size file:type 
    find-library-file flush-all-ports flush-all-ports-no-threads flush-submatches 
    flush-tty/both flush-tty/input flush-tty/output force-output fork fork-pty-session 
    fork/pipe fork/pipe+ format format-date free-c-argv getenv glob glob-quote group-info 
    group-info-gid group-info-members group-info-name group-info-passwd group-info:gid 
    group-info:members group-info:name group-info? hafod-major-version hafod-minor-version 
    hafod-version-string halts? home-dir home-directory home-file if-match if-sre-form 
    infix-splitter init-exec-path-list init-fdports! init-home-directory initialize-vector 
    input-port? integer->char join-strings let-match let-optionals let-optionals* lib-dirs 
    lib-dirs-append! lib-dirs-append-script-dir! lib-dirs-prepend! 
    lib-dirs-prepend-script-dir! list->string make-channel make-char-port-filter 
    make-control-tty make-date make-input-fdport make-output-fdport make-posix-error 
    make-pty-generator make-re-char-set make-re-choice make-re-choice/tsm make-re-dsm 
    make-re-dsm/tsm make-re-repeat make-re-repeat/tsm make-re-seq make-re-seq/tsm 
    make-re-string make-re-submatch make-re-submatch/tsm make-regexp make-resource 
    make-string-input-port make-string-output-port make-string-port-filter make-thread-local 
    make-tty-info mapv mapv! mark-proc-waited! match-cond match:count match:end match:start 
    match:substring maybe-directory-files maybe-pid->proc maybe-ref-fdport mode->type 
    move->fdes name->user-info new-child-proc newline next-:range next-:range: next-range 
    next-range: num-ttychars number->string obituary obtain-dot-lock open-control-tty 
    open-directory-stream open-dsm open-fdport? open-file open-input-file open-output-file 
    open-pty open-string-source open/append open/create open/exclusive open/non-blocking 
    open/read open/read+write open/truncate open/write output-port? parent-pid 
    parse-file-name passwd-info-dir passwd-info-gecos passwd-info-gid passwd-info-name 
    passwd-info-passwd passwd-info-shell passwd-info-uid passwd-info? path-list->file-name 
    pause pid pid->proc pid/proc? pipe pipe* port->fdes port->list port->sexp-list 
    port->string port->string-list port-fold port-revealed posix-_exit posix-access 
    posix-call posix-chdir posix-chmod posix-chown posix-close posix-closedir posix-ctermid 
    posix-dup posix-dup2 posix-errno posix-error? posix-exec posix-execve posix-fchmod 
    posix-fchown posix-fcntl posix-fnmatch posix-fork posix-fstat posix-fsync posix-ftruncate 
    posix-getcwd posix-getegid posix-getenv posix-geteuid posix-getgid posix-getgrgid 
    posix-getgrnam posix-getgroups posix-getlogin posix-getpgrp posix-getpid posix-getppid 
    posix-getpwnam posix-getpwuid posix-gettimeofday posix-getuid posix-gmtime posix-isatty 
    posix-kill posix-link posix-localtime posix-lseek posix-lstat posix-mkdir posix-mkfifo 
    posix-mkstemp posix-mktime posix-open posix-opendir posix-pause posix-pipe posix-read 
    posix-readdir posix-readlink posix-regcomp posix-regerror posix-regexec posix-regfree 
    posix-rename posix-rmdir posix-setegid posix-setenv posix-seteuid posix-setgid 
    posix-setpgid posix-setsid posix-setuid posix-sleep posix-stat posix-strftime 
    posix-string->regexp posix-symlink posix-sync posix-syscall posix-sysconf posix-tcdrain 
    posix-tcflow posix-tcflush posix-tcgetattr posix-tcgetpgrp posix-tcsendbreak 
    posix-tcsetattr posix-tcsetpgrp posix-time posix-times posix-truncate posix-ttyname 
    posix-umask posix-uname posix-unlink posix-unsetenv posix-utimes posix-waitpid 
    posix-write preserve-ports proc:finished? proc:pid proc:status proc:zombie? proc? 
    process-chdir process-cwd process-group process-sleep process-sleep-until process-times 
    ptr->string pty-name->tty-name raise-posix-error re-any re-any? re-bol re-bol? re-bos 
    re-bos? re-char-class? re-char-set:cset re-char-set? re-choice re-choice:elts 
    re-choice:tsm re-choice? re-dsm re-dsm:body re-dsm:post-dsm re-dsm:pre-dsm re-dsm:tsm 
    re-dsm? re-empty re-empty? re-eol re-eol? re-eos re-eos? re-nonl re-repeat re-repeat:body 
    re-repeat:from re-repeat:to re-repeat:tsm re-repeat? re-seq re-seq:elts re-seq:tsm 
    re-seq? re-string:chars re-string? re-submatch re-submatch:body re-submatch:post-dsm 
    re-submatch:pre-dsm re-submatch:tsm re-submatch? re-trivial re-trivial? re-tsm read-char 
    read-delimited read-delimited! read-directory-stream read-environ read-environ-fresh 
    read-line read-only read-paragraph read-symlink real->exact-integer 
    really-make-re-char-set really-make-re-choice really-make-re-dsm really-make-re-repeat 
    really-make-re-seq really-make-re-string really-make-re-submatch reap-zombies receive 
    record-reader reduce-port regexp->posix-string regexp->sre regexp-fold regexp-fold-right 
    regexp-for-each regexp-match regexp-match? regexp-search regexp-search? regexp-substitute 
    regexp-substitute/global regexp? release-dot-lock release-port-handle rename-file 
    replace-extension reset-lib-dirs! resolve-file-name resolve-tilde-file-name resource 
    resource-align! resource-name resource? run run-threads run/collecting run/collecting* 
    run/file run/file* run/port run/port* run/port+proc run/port+proc* run/sexp run/sexp* 
    run/sexps run/sexps* run/string run/string* run/strings run/strings* rx 
    scsh-major-version scsh-minor-version scsh-release-name scsh-version-string seek 
    seek/delta seek/end seek/set send-tty-break set-command-line-args! set-date:hour 
    set-date:minute set-date:month set-date:month-day set-date:seconds set-date:summer? 
    set-date:tz-name set-date:tz-secs set-date:week-day set-date:year set-date:year-day 
    set-fdport! set-file-group set-file-mode set-file-owner set-file-times set-gid 
    set-process-group set-tty-info/drain set-tty-info/flush set-tty-info/now 
    set-tty-info:control-chars set-tty-info:control-flags set-tty-info:input-flags 
    set-tty-info:input-speed set-tty-info:local-flags set-tty-info:min 
    set-tty-info:output-flags set-tty-info:output-speed set-tty-info:time 
    set-tty-process-group set-uid set-umask set-user-effective-gid set-user-effective-uid 
    setenv shell-open signal signal-process signal-process-group simplify-file-name 
    simplify-regexp skip-char-set sleazy-call/fdes sloppy-suffix-splitter spawn 
    split-colon-list split-file-name sre->regexp sre-form? start-tty-input start-tty-output 
    stat-info-atime stat-info-blksize stat-info-blocks stat-info-ctime stat-info-dev 
    stat-info-gid stat-info-ino stat-info-mode stat-info-mtime stat-info-nlink stat-info-rdev 
    stat-info-size stat-info-type stat-info-uid stat-info? static-char-class? status:exit-val 
    status:stop-sig status:term-sig stdports->stdio stop-tty-input stop-tty-output 
    string->char-set string->list string->number string->regexp string-append string-copy 
    string-downcase string-output-port-output string-upcase stringify strings->c-argv 
    substitute-env-vars substring suffix-splitter suspend sync-file sync-file-system 
    system-name tail-pipe tail-pipe+ tell temp-file-channel temp-file-iterate thread-join 
    thread-local-ref thread-local-set! thread-name thread-sleep thread-terminate! thread? 
    threads-running? threads-shutdown! threads-start! ticks/sec time time+ticks truncate-file 
    tty-file-name tty-info tty-info:control-chars tty-info:control-flags tty-info:input-flags 
    tty-info:input-speed tty-info:local-flags tty-info:min tty-info:output-flags 
    tty-info:output-speed tty-info:time tty-info? tty-name->pty-name tty-process-group tty? 
    ttyc/2-stop-bits ttyc/CTS-output-flow-ctl ttyc/RTS-input-flow-ctl ttyc/carrier-flow-ctl 
    ttyc/char-size ttyc/char-size5 ttyc/char-size6 ttyc/char-size7 ttyc/char-size8 
    ttyc/enable-parity ttyc/enable-read ttyc/hup-on-close ttyc/ignore-flags 
    ttyc/no-modem-sync ttyc/odd-parity ttychar/delayed-suspend ttychar/delete-char 
    ttychar/delete-line ttychar/delete-word ttychar/discard ttychar/eof ttychar/eol 
    ttychar/eol2 ttychar/interrupt ttychar/literal-next ttychar/min ttychar/quit 
    ttychar/reprint ttychar/start ttychar/status ttychar/stop ttychar/suspend ttychar/time 
    ttyin/7bits ttyin/beep-on-overflow ttyin/check-parity ttyin/cr->nl 
    ttyin/ignore-bad-parity-chars ttyin/ignore-break ttyin/ignore-cr ttyin/input-flow-ctl 
    ttyin/interrupt-on-break ttyin/lowercase ttyin/mark-parity-errors ttyin/nl->cr 
    ttyin/output-flow-ctl ttyin/xon-any ttyl/alt-delete-word ttyl/canonical ttyl/case-map 
    ttyl/echo ttyl/echo-ctl ttyl/echo-delete-line ttyl/echo-nl ttyl/enable-signals 
    ttyl/extended ttyl/flush-output ttyl/hardcopy-delete ttyl/no-flush-on-interrupt 
    ttyl/no-kernel-status ttyl/reprint-unread-chars ttyl/ttou-signal ttyl/visual-delete 
    ttyl/visual-delete-line ttyout/all-delay ttyout/bs-delay ttyout/bs-delay0 
    ttyout/bs-delay1 ttyout/cr->nl ttyout/cr-delay ttyout/cr-delay0 ttyout/cr-delay1 
    ttyout/cr-delay2 ttyout/cr-delay3 ttyout/delay-w/fill-char ttyout/discard-eot 
    ttyout/enable ttyout/expand-tabs ttyout/ff-delay ttyout/ff-delay0 ttyout/ff-delay1 
    ttyout/fill-w/del ttyout/nl->crnl ttyout/nl-delay ttyout/nl-delay0 ttyout/nl-delay1 
    ttyout/nl-does-cr ttyout/no-col0-cr ttyout/tab-delay ttyout/tab-delay0 ttyout/tab-delay1 
    ttyout/tab-delay2 ttyout/tab-delayx ttyout/uppercase ttyout/vtab-delay ttyout/vtab-delay0 
    ttyout/vtab-delay1 umask umask-resource uname uname-info? uname:machine uname:node-name 
    uname:os-name uname:release uname:version uncase uncase-char-set uncase-string 
    user-effective-gid user-effective-uid user-gid user-info user-info:gid user-info:home-dir 
    user-info:name user-info:shell user-info:uid user-info? user-login-name 
    user-supplementary-gids user-uid vector-append vector-every? vfold vfold-right wait 
    wait-any wait/poll wait/stopped-children warn with-current-error-port 
    with-current-error-port* with-current-input-port with-current-input-port* 
    with-current-output-port with-current-output-port* with-cwd with-cwd* with-dot-lock 
    with-dot-lock* with-env with-env* with-errno-handler with-errno-handler* 
    with-error-output-port with-error-output-port* with-input-from-file with-output-to-file 
    with-resources-aligned with-stdio-ports with-stdio-ports* with-total-env with-total-env* 
    with-umask with-umask* with-user-effective-gid with-user-effective-gid* 
    with-user-effective-uid with-user-effective-uid* write write+append+create write-char 
    yield )
  (import (hafod))
)
