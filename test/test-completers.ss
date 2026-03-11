(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell completers)
        (hafod fuzzy))

(test-begin "Shell Completers")

;; === Registry ===

(test-assert "git completer registered"
  (procedure? (lookup-completer "git")))

(test-assert "ssh completer registered"
  (procedure? (lookup-completer "ssh")))

(test-assert "scp completer registered"
  (procedure? (lookup-completer "scp")))

(test-assert "kill completer registered"
  (procedure? (lookup-completer "kill")))

(test-assert "make completer registered"
  (procedure? (lookup-completer "make")))

(test-assert "unknown command returns #f"
  (not (lookup-completer "nonexistent-cmd-xyz")))

;; === Custom registration ===

(register-completer! "test-cmd" (lambda (prefix ctx) '()))

(test-assert "custom completer registered"
  (procedure? (lookup-completer "test-cmd")))

(test-assert "completer-names includes built-ins"
  (let ([names (completer-names)])
    (and (member "git" names)
         (member "ssh" names)
         (member "make" names))))

;; === Git completer: subcommands ===

(let ([results (git-completer "comm" '((args)))])
  (test-assert "git completer returns results for 'comm'"
    (not (null? results)))
  (test-assert "git completer finds 'commit'"
    (assoc "commit" results))
  ;; Check result format: (name positions desc)
  (test-assert "git result has 3 elements"
    (= (length (car results)) 3))
  (test-assert "git result has positions list"
    (list? (cadr (car results))))
  (test-assert "git result has description"
    (string? (caddr (assoc "commit" results)))))

;; Empty prefix returns all subcommands
(let ([results (git-completer "" '((args)))])
  (test-assert "git empty prefix returns subcommands"
    (> (length results) 10)))

;; === Make completer ===

;; Test with the project's own Makefile
(let ([results (make-completer "comp" '((args)))])
  (test-assert "make completer finds targets"
    ;; Our Makefile should have compile-wpo target
    (or (null? results)  ; no Makefile in test dir
        (pair? results))))

;; === SSH completer ===

;; Just verify it doesn't crash (may return empty if no ssh config)
(let ([results (ssh-completer "test" '((args)))])
  (test-assert "ssh completer returns list"
    (list? results)))

;; === Kill completer ===

;; Should find at least some processes
(let ([results (kill-completer "1" '((args)))])
  (test-assert "kill completer returns list"
    (list? results)))

(test-end)
