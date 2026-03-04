(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod temp-file) (hafod posix) (hafod fd-ports) (hafod compat)
        (hafod environment)
        (except (chezscheme) vector-append open-input-file open-output-file getenv))

(test-begin "Temp Files")

;; create-temp-file
(test-assert "create-temp-file returns a path"
  (let ([path (create-temp-file)])
    (let ([exists (zero? (posix-access path F_OK))])
      (posix-unlink path)
      exists)))

(test-assert "create-temp-file path is a string"
  (let ([path (create-temp-file)])
    (posix-unlink path)
    (string? path)))

(test-assert "create-temp-file with custom prefix"
  (let ([path (create-temp-file "/tmp/myprefix-")])
    (let ([has-prefix (let ([plen (string-length "/tmp/myprefix-")])
                        (string=? (substring path 0 plen) "/tmp/myprefix-"))])
      (posix-unlink path)
      has-prefix)))

(test-assert "create-temp-file creates unique files"
  (let ([p1 (create-temp-file)]
        [p2 (create-temp-file)])
    (let ([different (not (string=? p1 p2))])
      (posix-unlink p1)
      (posix-unlink p2)
      different)))

;; temp-file-channel
(test-assert "temp-file-channel returns two ports"
  (receive (iport oport) (temp-file-channel)
    (let ([ok (and (input-port? iport) (output-port? oport))])
      (close iport)
      (close oport)
      ok)))

(test-equal "temp-file-channel write and read round-trip"
  "hello from channel"
  (receive (iport oport) (temp-file-channel)
    (display "hello from channel" oport)
    (flush-output-port oport)
    ;; The input port was opened separately so it starts at offset 0.
    (let ([content (get-string-all iport)])
      (close iport)
      (close oport)
      (if (eof-object? content) "" content))))

;; temp-file-iterate
(test-assert "temp-file-iterate calls maker with numbered names"
  (let ([names '()])
    (temp-file-iterate
      (lambda (fname)
        (set! names (cons fname names))
        ;; Succeed on third attempt
        (when (< (length names) 3)
          (error 'test "not yet"))
        fname)
      "/tmp/iterate-test-~a")
    (>= (length names) 3)))

(test-assert "temp-file-iterate first attempt can succeed"
  (string? (temp-file-iterate
             (lambda (fname) fname)
             "/tmp/iterate-ok-~a")))

(test-end)
