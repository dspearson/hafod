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
(compile-program "bin/hafod.sps")

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
  "bin/hafod.so")

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
