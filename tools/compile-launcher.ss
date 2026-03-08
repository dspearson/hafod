;; tools/compile-launcher.ss -- Compile hafod launcher with whole-program optimisation
;; Merges all (hafod) library dependencies into bin/hafod.so via compile-whole-program.
;; The launcher imports (hafod) at compile time; compile-whole-program inlines all
;; resolved library code into the program .so so it loads near-instantly at runtime.
;; Run: scheme --libdirs src --script tools/compile-launcher.ss
(import (chezscheme))
(compress-level 'minimum)
(generate-wpo-files #t)
(compile-program "bin/hafod.sps")
(when (file-exists? "bin/hafod.wpo") (delete-file "bin/hafod.wpo"))
