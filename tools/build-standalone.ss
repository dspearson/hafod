;; tools/build-standalone.ss -- Build a self-contained hafod binary
;;
;; Produces bin/hafod-standalone: a single native executable with
;; petite.boot, hafod.boot (all libraries baked in), and the launcher
;; program embedded.  No external files needed at runtime.
;;
;; Strategy:
;;   1. Parse all hafod source files, topologically sort dependencies
;;   2. make-boot-file bakes all hafod libraries into hafod.boot
;;   3. The original bin/hafod.sps is compiled as the program
;;   4. The C launcher embeds petite.boot + hafod.boot + program .so
;;      Boot files are loaded from memory (no disk I/O for libraries)
;;
;; Run: scheme --libdirs src --script tools/build-standalone.ss
(import (chezscheme))

(define build-dir "tools")
(define out-dir "bin")

;; ======================================================================
;; Parse library imports from source files
;; ======================================================================

(define (parse-library-info file)
  (call-with-input-file file
    (lambda (port)
      (let ([form (read port)])
        (cond
          [(and (pair? form) (eq? (car form) 'library))
           (values (cadr form) (find-imports (cdddr form)))]
          [else
           (let ([form2 (read port)])
             (if (and (pair? form2) (eq? (car form2) 'library))
                 (values (cadr form2) (find-imports (cdddr form2)))
                 (error 'parse-library-info
                        "Could not find library form" file)))])))))

(define (find-imports body)
  (let loop ([forms body])
    (cond
      [(null? forms) '()]
      [(and (pair? (car forms)) (eq? (caar forms) 'import))
       (extract-lib-names (cdar forms))]
      [(and (pair? (car forms)) (eq? (caar forms) 'export))
       (loop (cdr forms))]
      [else (loop (cdr forms))])))

(define (extract-lib-names import-specs)
  (let loop ([specs import-specs] [names '()])
    (if (null? specs) (reverse names)
        (loop (cdr specs) (cons (unwrap-import-spec (car specs)) names)))))

(define (unwrap-import-spec spec)
  (if (and (pair? spec)
           (memq (car spec) '(only except prefix rename for)))
      (unwrap-import-spec (cadr spec))
      spec))

(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)] [len (string-length str)])
    (and (>= len slen)
         (string=? (substring str (- len slen) len) suffix))))

;; ======================================================================
;; Discover and topologically sort all hafod source files
;; ======================================================================

(define all-files
  (let ([files '()])
    (define (scan dir)
      (for-each
        (lambda (entry)
          (let ([path (format "~a/~a" dir entry)])
            (cond
              [(file-directory? path) (scan path)]
              [(string-suffix? ".ss" entry)
               (set! files (cons path files))])))
        (directory-list dir)))
    (scan "src/hafod")
    (set! files (cons "src/hafod.ss" files))
    files))

(define file-info
  (map (lambda (file)
         (call-with-values
           (lambda () (parse-library-info file))
           (lambda (name imports) (list file name imports))))
       all-files))

(define name->file
  (let ([ht (make-hashtable equal-hash equal?)])
    (for-each (lambda (info) (hashtable-set! ht (cadr info) (car info)))
              file-info)
    ht))

(define name->deps
  (let ([ht (make-hashtable equal-hash equal?)])
    (for-each
      (lambda (info)
        (let* ([name (cadr info)]
               [imports (caddr info)]
               [hafod-deps (filter (lambda (imp)
                                     (and (pair? imp)
                                          (eq? (car imp) 'hafod)))
                                   imports)])
          (hashtable-set! ht name hafod-deps)))
      file-info)
    ht))

(define (toposort)
  (let* ([all-names (map cadr file-info)]
         [in-degree (make-hashtable equal-hash equal?)]
         [dependents (make-hashtable equal-hash equal?)])
    (for-each (lambda (n)
                (hashtable-set! in-degree n 0)
                (hashtable-set! dependents n '()))
              all-names)
    (for-each
      (lambda (name)
        (for-each
          (lambda (dep)
            (when (hashtable-contains? name->file dep)
              (hashtable-set! in-degree name
                (+ 1 (hashtable-ref in-degree name 0)))
              (hashtable-set! dependents dep
                (cons name (hashtable-ref dependents dep '())))))
          (hashtable-ref name->deps name '())))
      all-names)
    (let loop ([queue (filter (lambda (n) (= 0 (hashtable-ref in-degree n 0)))
                              all-names)]
               [result '()])
      (if (null? queue)
          (begin
            (let ([remaining (filter (lambda (n) (> (hashtable-ref in-degree n 0) 0))
                                     all-names)])
              (unless (null? remaining)
                (errorf 'toposort "Circular dependencies: ~a" remaining)))
            (reverse result))
          (let* ([node (car queue)]
                 [rest (cdr queue)]
                 [new-queue
                   (fold-left
                     (lambda (q dep)
                       (let ([new-deg (- (hashtable-ref in-degree dep 0) 1)])
                         (hashtable-set! in-degree dep new-deg)
                         (if (= new-deg 0) (cons dep q) q)))
                     rest
                     (hashtable-ref dependents node '()))])
            (loop new-queue (cons node result)))))))

(define sorted-names (toposort))
(define sorted-files
  (map (lambda (name) (hashtable-ref name->file name #f)) sorted-names))

(printf "Topological order: ~a libraries~n" (length sorted-names))

;; Every discovered library must reach the boot image. A boot image built from a
;; SUBSET is this build's worst failure mode and its quietest: the binary still
;; links, still starts, still runs -- and simply does not have the missing
;; libraries, which a user meets much later as "library (hafod srfi-9) not found".
;; The shipped binary carries no compiler and no source tree, so whatever the boot
;; image lacks is gone for good; there is nowhere to load it from. Fail here, where
;; the count is still in hand.
(let ([dropped (filter (lambda (name) (not (hashtable-ref name->file name #f)))
                       sorted-names)])
  (unless (null? dropped)
    (errorf 'build-standalone
            "~a discovered libraries have no source file: ~a"
            (length dropped) dropped)))
(unless (= (length sorted-files) (length all-files))
  (errorf 'build-standalone
          "the sort lost libraries: ~a discovered, ~a to be baked in"
          (length all-files) (length sorted-files)))

;; ======================================================================
;; Step 1: Find Chez lib directory
;; ======================================================================

(define chez-lib-dir
  (let* ([scheme-bin (or (getenv "SCHEME_BIN")
                         (let ([p (process "readlink -f $(which scheme)")])
                           (let ([s (get-line (car p))])
                             (close-port (car p))
                             (close-port (cadr p))
                             (if (eof-object? s) "scheme" s))))]
         [prefix (path-parent (path-parent scheme-bin))]
         [v (let ([vs (scheme-version)])
              (let f ([i (- (string-length vs) 1)])
                (if (char=? (string-ref vs i) #\space)
                    (substring vs (+ i 1) (string-length vs))
                    (f (- i 1)))))]
         [dir (format "~a/lib/csv~a/~a" prefix v (machine-type))])
    (unless (file-exists? (format "~a/scheme.h" dir))
      (errorf 'build-standalone "Cannot find Chez lib dir at ~a" dir))
    dir))

(printf "Chez lib dir: ~a~n" chez-lib-dir)

;; ======================================================================
;; The library-mode launcher's artefacts are off limits
;; ======================================================================
;;
;; bin/hafod.so is the OTHER launcher: a whole-program merge produced by
;; tools/compile-launcher.ss, and a real Make target whose prerequisites are the
;; library sources. src/**/*.so and src/**/*.wpo are the library objects that
;; launcher loads at run time. This build must not write any of them.
;;
;; Neither rule is a tidiness preference. Both are a crash, and a permanent one:
;;
;;   * The program compiled below is compiled against the compilation instances
;;     make-boot-file creates in THIS process, from source. The launcher path loads
;;     src/**/*.so, which compile-all.ss built in a DIFFERENT process. Leave this
;;     program at bin/hafod.so and every launch of the launcher -- including
;;     --version -- dies with "loading src/hafod/tty.ss yielded a different
;;     compilation instance of (hafod tty) from that required by compiled program".
;;   * And make would never repair it. The file it finds there is newer than every
;;     prerequisite, so bin/hafod.so counts as up to date and is never re-merged.
;;     The tree stays broken until somebody deletes the file by hand.
;;
;; So refuse the path before compiling anything, and then prove the artefacts were
;; not touched. The path check is the one that reads; the fingerprint check is the
;; one that cannot be got round by accident, because it asserts on the files
;; themselves rather than on the name this script happens to hand compile-program.

(define launcher-so "bin/hafod.so")

;; Walk `dir` for files with any of `suffixes`, descending into REAL directories
;; only. Chez's file-directory? follows symlinks, so a symlinked directory would be
;; walked like any other -- and one that reaches its own ancestor would be walked
;; again at every level, until the kernel's symlink limit finally refused the path.
;; That is bounded, so it is not a hang; it is just wrong. The list would hold each
;; real object forty-odd times over, under aliased paths naming nothing anyone could
;; act on, and every one of them would be stat'd on both passes.
;;
;; Nor is it hypothetical: the CI job stages the library tree for the native binary
;; with an `ln -sf src src`, which lands a link inside src/. That one is harmless
;; only by accident -- it resolves to itself, so stat refuses it at the first step --
;; and the walk should not be relying on the luck of how circular a link happens to
;; be. Fingerprint each real object once.
(define (find-files dir suffixes)
  (let ([found '()])
    (define (walk d)
      (for-each
        (lambda (entry)
          (let ([path (format "~a/~a" d entry)])
            (cond
              [(file-symbolic-link? path)]        ; never descend, never fingerprint
              [(file-directory? path) (walk path)]
              [(exists (lambda (sfx) (string-suffix? sfx entry)) suffixes)
               (set! found (cons path found))])))
        (directory-list d)))
    (when (file-directory? dir) (walk dir))
    found))

;; The launcher image, plus every library object it loads at run time.
(define protected-files
  (cons launcher-so (find-files "src" '(".so" ".wpo"))))

;; (path size mtime-seconds mtime-nanoseconds), or (path #f) when absent. Any
;; write changes at least one of these, whatever route the write took -- a
;; compile-program aimed at the wrong path, a compile-imported-libraries that
;; decided to rebuild a library, a stray generate-wpo-files. The check does not
;; care which; only that the launcher's artefacts are as this build found them.
(define (fingerprint path)
  (if (file-exists? path)
      (let ([t (file-modification-time path)]
            [size (let ([p (open-file-input-port path)])
                    (let ([n (port-length p)]) (close-port p) n))])
        (list path size (time-second t) (time-nanosecond t)))
      (list path #f)))

(define protected-before (map fingerprint protected-files))

;; `stage` names the step that was running, for the message. It must not be called
;; `when`: that would shadow the `when` syntax for the whole body below, turning
;; (when (file-exists? path) (delete-file path)) into an attempt to APPLY the
;; string -- which evaluates its arguments (deleting the file) and only then fails,
;; with "attempt to apply non-procedure" in place of the diagnosis.
(define (assert-launcher-untouched! stage)
  (let ([changed (filter (lambda (before)
                           (not (equal? before (fingerprint (car before)))))
                         protected-before)])
    (unless (null? changed)
      ;; Delete what we damaged, THEN fail. This is the whole point of the guard:
      ;; a clobbered bin/hafod.so is newer than every one of its prerequisites, so
      ;; make would count it up to date and never rebuild it -- the tree would stay
      ;; broken across every later build, on every launch, until somebody thought
      ;; to remove the file by hand. Removing it here turns a permanent brick into
      ;; an ordinary failed build that the next `make` repairs.
      (for-each (lambda (before)
                  (let ([path (car before)])
                    (when (file-exists? path) (delete-file path))))
                changed)
      (errorf 'build-standalone
              (string-append
                "this build wrote the library-mode launcher's artefacts while ~a: ~a.~n"
                "  bin/hafod.so belongs to tools/compile-launcher.ss, and src/**/*.so"
                " are the objects it loads at run time.~n"
                "  A program compiled here carries THIS process's compilation"
                " instances; the launcher loads a different set, so it would crash on"
                " every launch, --version included.~n"
                "  The damaged files have been deleted so the next `make` rebuilds"
                " them; had they been left in place, being newer than their"
                " prerequisites, make would never have done so.")
              stage (map car changed)))))

;; ======================================================================
;; Step 2: Build hafod.boot with all libraries baked in
;; ======================================================================

(define hafod-boot-file (format "~a/hafod.boot" build-dir))

(printf "Building hafod.boot (~a source files)...~n" (length sorted-files))

(generate-wpo-files #f)
(compile-imported-libraries #f)
(library-directories '(("src" . "src")))

(apply make-boot-file hafod-boot-file '("petite") sorted-files)

(printf "hafod.boot: ~a bytes~n"
  (let ([p (open-input-file hafod-boot-file)])
    (let ([n (port-length p)]) (close-port p) n)))

;; ======================================================================
;; Step 2b: Convert boot files to vfasl format for faster loading
;; ======================================================================

(define petite-boot (format "~a/petite.boot" chez-lib-dir))
(define petite-vfasl-file (format "~a/petite-vfasl.boot" build-dir))
(define hafod-vfasl-file (format "~a/hafod-vfasl.boot" build-dir))

(printf "Converting to vfasl format...~n")
(vfasl-convert-file petite-boot petite-vfasl-file '())
(vfasl-convert-file hafod-boot-file hafod-vfasl-file '("petite"))

(printf "petite-vfasl.boot: ~a bytes~n"
  (let ([p (open-input-file petite-vfasl-file)])
    (let ([n (port-length p)]) (close-port p) n)))
(printf "hafod-vfasl.boot: ~a bytes~n"
  (let ([p (open-input-file hafod-vfasl-file)])
    (let ([n (port-length p)]) (close-port p) n)))

;; ======================================================================
;; Step 3: Compile the launcher program
;; ======================================================================

(printf "Compiling launcher program...~n")
;; Need compile-imported-libraries for the program compilation
(generate-wpo-files #t)
(compile-imported-libraries #t)
(library-directories '(("src" . "src")))

;; Compile to our own build directory, NEVER to bin/hafod.so. The standalone
;; embeds this program alongside a boot image that already holds every library, so
;; it does not want -- and cannot use -- the whole-program merge that
;; tools/compile-launcher.ss applies to the library-mode launcher. See the
;; artefact guard above for what writing to bin/hafod.so would cost.
(define standalone-program-so (format "~a/hafod-program.so" build-dir))

;; Refuse the path itself, before compiling anything. An editor who redirects this
;; output at the launcher gets a build failure naming the reason, not a tree that
;; crashes on every launch and that make will never repair.
(when (string=? standalone-program-so launcher-so)
  (errorf 'build-standalone
          (string-append
            "refusing to compile the standalone's program to ~a: that file is the"
            " library-mode launcher's merged image, built by"
            " tools/compile-launcher.ss from a different set of compilation"
            " instances. Compile to ~a/ instead.")
          launcher-so build-dir))

(compile-program "bin/hafod.sps" standalone-program-so)

;; ...and prove it. The path check above only constrains the name this script
;; passes; this constrains the filesystem, so it also catches a library quietly
;; recompiled under src/ by the compile-imported-libraries setting above.
(assert-launcher-untouched! "compiling the program")

;; ======================================================================
;; Step 4: Generate C byte arrays
;; ======================================================================

(define (emit-c-bytes port sym-name data)
  (format port "const unsigned char ~a[] = {" sym-name)
  (let ([len (bytevector-length data)])
    (do ([i 0 (+ i 1)])
        ((= i len))
      (when (= 0 (mod i 16)) (format port "~n  "))
      (format port "0x~2,'0x," (bytevector-u8-ref data i))))
  (format port "~n};~n")
  (format port "const unsigned int ~a_size = sizeof(~a);~n" sym-name sym-name))

(define (emit-c-array output-path sym-name data-path)
  (let ([data (get-bytevector-all (open-file-input-port data-path))])
    (printf "Embedding ~a (~a bytes)~n" data-path (bytevector-length data))
    (with-output-to-file output-path
      (lambda ()
        (format #t "#include <stdint.h>~n")
        (emit-c-bytes (current-output-port) sym-name data))
      'replace)))

;; Emit LZ4-compressed vfasl boot file (Chez bytevector-compress format)
(define (emit-c-compressed-array output-path sym-name data-path)
  (let* ([data (get-bytevector-all (open-file-input-port data-path))]
         [orig-size (bytevector-length data)]
         [compressed (bytevector-compress data)])
    (printf "Embedding ~a (~a -> ~a bytes, ~,1f%%)~n"
      data-path orig-size (bytevector-length compressed)
      (* 100.0 (/ (bytevector-length compressed) orig-size)))
    (with-output-to-file output-path
      (lambda ()
        (format #t "#include <stdint.h>~n")
        (emit-c-bytes (current-output-port) (string-append sym-name "_cmp") compressed))
      'replace)))

(printf "Generating embedded data...~n")
(emit-c-compressed-array (format "~a/boot_data.c" build-dir) "petite_boot"
  petite-vfasl-file)
(emit-c-compressed-array (format "~a/hafod_boot_data.c" build-dir) "hafod_boot"
  hafod-vfasl-file)
(emit-c-array (format "~a/prog_data.c" build-dir) "hafod_program"
  standalone-program-so)

;; ======================================================================
;; Step 5: Compile and link C binary
;; ======================================================================

(printf "Linking standalone binary...~n")
(let* ([darwin? (let ([mt (symbol->string (machine-type))])
                  (let ([len (string-length mt)])
                    (and (>= len 3)
                         (string=? "osx" (substring mt (- len 3) len)))))]
       [solibs (string-append
                 (if darwin? "" "-ldl ")
                 "-lm -llz4 -lz -lncurses"
                 (if (threaded?) " -lpthread" "")
                 (if darwin? " -liconv" ""))]
       [ldflags (or (getenv "LDFLAGS") "")]
       [cc (or (getenv "CC") "cc")]
       [cflags (or (getenv "CFLAGS") "")]
       [cmd (format "~a -O2 -o ~a/hafod-standalone ~a/hafod-standalone.c ~a/boot_data.c ~a/hafod_boot_data.c ~a/prog_data.c ~a/libkernel.a -I~a ~a ~a ~a"
               cc out-dir build-dir build-dir build-dir build-dir chez-lib-dir chez-lib-dir cflags ldflags solibs)])
  (printf "~a~n" cmd)
  (let ([ret (system cmd)])
    (unless (= ret 0)
      (errorf 'build-standalone "Compilation failed (~a)" ret))))

;; ======================================================================
;; Cleanup intermediates
;; ======================================================================

(for-each (lambda (f) (when (file-exists? f) (delete-file f)))
  (list hafod-boot-file
        petite-vfasl-file
        hafod-vfasl-file
        (format "~a/boot_data.c" build-dir)
        (format "~a/hafod_boot_data.c" build-dir)
        (format "~a/prog_data.c" build-dir)))

(printf "~nBuilt: ~a/hafod-standalone~n" out-dir)
(let ([p (open-input-file (format "~a/hafod-standalone" out-dir))])
  (printf "Size: ~a bytes (~,1f MB)~n"
    (port-length p)
    (/ (port-length p) 1048576.0))
  (close-port p))
