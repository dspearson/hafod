;; tools/verify-umbrella.ss -- Verify (hafod) umbrella exports all expected symbols
;; Run: scheme --libdirs src --script tools/verify-umbrella.ss
(import (chezscheme))

(let* ([env (environment '(hafod))]
       [syms (environment-symbols env)]
       [count (length syms)])
  (printf "Umbrella (hafod) exports ~d symbols~n" count)
  (when (< count 791)
    (fprintf (current-error-port)
             "ERROR: Expected at least 791 symbols, found ~d~n" count)
    (exit 1))
  (printf "Umbrella verification PASSED (~d symbols)~n" count))
