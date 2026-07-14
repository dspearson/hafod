;; tools/compile-launcher.ss -- Compile hafod launcher with whole-program optimisation
;; Merges all (hafod) library dependencies into bin/hafod.so via compile-whole-program.
;; The launcher imports (hafod) at compile time; compile-whole-program inlines all
;; resolved library code into the program .so so it loads near-instantly at runtime.
;; Run: scheme --libdirs src --script tools/compile-launcher.ss
(import (chezscheme))
(compress-level 'minimum)
(generate-wpo-files #t)
;; compile-program emits bin/hafod.so plus bin/hafod.wpo (the whole-program
;; optimisation record) because generate-wpo-files is enabled above.
(compile-program "bin/hafod.sps")
;; Merge the program and every dependency's .wpo into ONE image. The THIRD argument
;; (libs-visible? = #t) is REQUIRED and load-bearing: bin/hafod.sps reaches (hafod)
;; at run time through the `environment` procedure via
;;   (eval '(import (hafod)) (interaction-environment))
;; With #f (the default) the merge inlines the libraries but leaves them INVISIBLE
;; to that import, and the launch dies flatly:
;;   Exception: attempt to import invisible library (hafod config)
;; Chez refuses the import outright; it never gets as far as loading a second copy
;; from src/. With #t the merged (hafod) stays visible, so the runtime import
;; reuses the already-inlined instance.
;;
;; Note where the failure is NOT: --version never reaches the runtime import, so it
;; runs perfectly happily on a #f image. Only -c, -s and the REPL touch it. A smoke
;; test that just asks for the version would call this build good.
(let ([left (compile-whole-program "bin/hafod.wpo" "bin/hafod.so" #t)])
  ;; `left` is the list of libraries still to be loaded at run time. A genuinely
  ;; merged launcher must leave no (hafod ...) library behind (its code is now
  ;; inlined). Boot/system libraries baked into petite.boot/scheme.boot may
  ;; legitimately remain and are reported for information only.
  ;;
  ;; This checks that nothing was left OUT of the image -- and only that. It is not
  ;; a proxy for a correct merge: compile-whole-program returns () for the #f build
  ;; above too, right before that build fails to launch. Visibility is a separate
  ;; property, and this value cannot see it.
  (let ([hafod-left (filter (lambda (lib) (and (pair? lib) (eq? (car lib) 'hafod)))
                            left)])
    (unless (null? left)
      (printf "compile-launcher: libraries left to load at runtime: ~s~n" left))
    (unless (null? hafod-left)
      (printf "compile-launcher: ERROR -- hafod libraries not inlined by the merge: ~s~n"
              hafod-left)
      ;; A reachable un-merged (hafod ...) library is exactly the "different
      ;; compilation instance" launch crash described above.  The failure
      ;; signal is already in hand, so fail the build here rather than emit a
      ;; silently-broken image that only the separate test-launcher would catch.
      (exit 1))))
;; The .wpo record is only needed to drive the merge above; drop it once merged.
(when (file-exists? "bin/hafod.wpo") (delete-file "bin/hafod.wpo"))
