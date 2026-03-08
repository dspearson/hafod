;; tools/compile-wpo.ss -- Merge all hafod libraries into a single .so
;; Requires that all libraries were compiled with (generate-wpo-files #t).
;; Replaces src/hafod.so with the whole-library optimised version.
;; Run: scheme --libdirs src --script tools/compile-wpo.ss
(compress-level 'minimum)
(compile-whole-library "src/hafod.wpo" "src/hafod.so")
