# hafod

**hafod** is a port of [scsh](https://scsh.net/) (the Scheme Shell) to
[Chez Scheme](https://cisco.github.io/ChezScheme/) (R6RS).  It provides
the full scsh programming interface -- process notation, POSIX bindings,
SRE regular expressions, field readers, AWK, and more -- on a modern,
high-performance Scheme implementation.

The name *hafod* is Welsh for a summer dwelling on high pasture, a
seasonal shell.

```scheme
#!/usr/bin/env hafod
-s
!#
(import (hafod))

(define executables
  (let ([split (infix-splitter (rx ":"))])
    (apply append
      (map (lambda (dir)
             (guard (e [#t '()])
               (with-cwd dir
                 (filter file-executable? (directory-files dir)))))
           (split (getenv "PATH"))))))

(for-each (lambda (f) (display f) (newline))
  (sort string<? (delete-duplicates executables)))
```

## Features

- **Process notation** -- `run`, `exec-epf`, `&`, `||`, `&&`, pipelines
  with implicit quasiquoting (`,var` evaluates, bare symbols become strings)
- **Redirections** -- `<`, `>`, `>>`, `<<` (here-string), `=` (dup), `-` (close)
- **Run collectors** -- `run/string`, `run/strings`, `run/port`, `run/sexp`,
  `run/collecting`, `run/file`
- **POSIX bindings** -- fork, exec, wait, pipe, signals, file operations,
  directory operations, user/group database, TTY/PTY, environment variables,
  process state (cwd, umask, uid/gid), file info, temp files, globbing
- **SRE regular expressions** -- `rx` macro, POSIX backend, `regexp-search`,
  `regexp-substitute/global`, `let-match`, `if-match`, `match-cond`,
  `regexp-fold`, RE ADT layer
- **Field readers and AWK** -- `field-splitter`, `infix-splitter`,
  `record-reader`, `awk` macro with patterns, ranges, and state variables
- **Delimited I/O** -- `read-line`, `read-delimited`, `read-paragraph`
- **File name utilities** -- path parsing, tilde expansion, glob patterns,
  extension manipulation
- **TTY/PTY control** -- terminal info records, baud rates, control flags,
  `fork-pty-session`, `open-pty`
- **Time and date** -- `date`, `format-date`, `time`, POSIX time functions
- **Script launcher** -- scsh-compatible CLI with `-s`, `-c`, `-e`, `--`,
  meta-argument processing, `!#` header stripping, auto-import of `(hafod)`,
  and `|` → `pipe` source preprocessing for scsh compatibility
- **Full scsh compatibility** -- 1:1 coverage of the scsh public API
  (1,003 exported symbols); `(import (scsh))` works as an alias for
  `(import (hafod))`; all scsh accessor names, predicates, char-sets,
  file-options, RE ADT layer, and version aliases

## Requirements

- [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.x or later
- A POSIX operating system (Linux, macOS, *BSD)
- C library with standard POSIX interfaces (glibc, musl, etc.)

## Building

```sh
make                    # compile all libraries
make test               # run the full test suite (1,900+ tests)
```

If `scheme` is not on your PATH or is named differently:

```sh
make SCHEME=/path/to/chez-scheme
```

### Build modes

hafod supports three build modes:

| Mode | Command | Output | Size | Startup | Runtime dependencies |
|------|---------|--------|------|---------|---------------------|
| Library | `make` | `bin/hafod` | n/a | ~87ms | `scheme` on PATH |
| Native | `make native` | `bin/hafod-native` | ~50KB | ~85ms | petite.boot, compiled libs |
| Standalone | `make standalone` | `bin/hafod-standalone` | ~5.1MB | ~62ms | none |

**Library** (default) compiles all libraries and produces a shell wrapper
that invokes `scheme`.  No C compiler needed.

**Native** links against Chez's `libkernel.a` for a small native binary
that still requires `petite.boot` and the compiled `.so` libraries at
runtime.  Useful for installed deployments where Chez is available.

**Standalone** embeds LZ4-compressed vfasl boot files and the launcher
program into a single self-contained binary.  No external files needed
at runtime — recommended for distribution.

### Nix

A `flake.nix` is provided for development:

```sh
nix develop             # enter a shell with Chez Scheme available
make && make test
```

## Installation

For a self-contained binary with no runtime dependencies, use the
standalone build:

```sh
make standalone
make install                          # installs to /usr/local
```

Other install options:

```sh
make install PREFIX=/opt/hafod        # custom prefix
make install DESTDIR=/tmp/staging     # staged install for packaging
```

`make install` automatically picks the best available binary: standalone
if built, then native, then the shell wrapper.  It also installs:
- `lib/hafod/src/` -- compiled libraries (for use as an R6RS library)
- `share/man/man1/hafod.1` -- man page

If no existing `scsh` binary is found on PATH, a `scsh -> hafod` symlink
is created in the bin directory for compatibility.

```sh
make uninstall          # removes installed files
```

## Usage

### As a library

```scheme
;; In Chez Scheme REPL or script:
(import (hafod))

;; Run a command, capture output as a string
(run/string (ls -la))

;; Pipelines
(run/strings (pipe (cat "/etc/passwd") (grep "root")))

;; Unquote Scheme variables into process forms
(let ([pattern "error"])
  (run/string (grep ,pattern "/var/log/syslog")))

;; Fork, signals, wait
(let ([child (fork (lambda () (pause) (%exit 0)))])
  (signal-process child SIGTERM)
  (wait child))

;; AWK
(awk (read-line) (line) counter ((n 0))
  (#t (set! n (+ n 1)))
  (after (display n)))
```

### As a script interpreter

```sh
# Run a script
hafod -s script.ss

# Run a script (implicit -s for .ss files)
hafod script.ss arg1 arg2

# Evaluate an expression
hafod -c '(display (+ 1 2))'

# Run with an entry point
hafod -e main -s script.ss arg1 arg2

# Interactive REPL
hafod --
```

### Shebang scripts

Single-line shebang (no import needed -- hafod auto-imports):

```scheme
#!/usr/bin/env hafod
!#
(display "Hello from hafod\n")
```

Meta-argument shebang (for passing multiple flags):

```scheme
#!/path/to/hafod \
-e main -s
!#
(define (main args)
  (for-each (lambda (a) (display a) (newline)) args))
```

## Library structure

All functionality is available via a single import:

```scheme
(import (hafod))
```

Individual subsystems can also be imported separately:

| Library | Description |
|---------|-------------|
| `(hafod compat)` | Scheme48 compatibility (receive, let-optionals, etc.) |
| `(hafod fname)` | File name parsing and manipulation |
| `(hafod command-line)` | Command-line argument access |
| `(hafod rdelim)` | Delimited readers (read-line, read-delimited) |
| `(hafod signal)` | Signal constants and signal-process |
| `(hafod user-group)` | User and group database access |
| `(hafod fname-system)` | Tilde expansion, env var substitution in paths |
| `(hafod posix)` | Low-level POSIX FFI bindings |
| `(hafod fd-ports)` | File descriptor / port mapping, dup, move->fdes |
| `(hafod procobj)` | Process objects, wait, status accessors |
| `(hafod collect)` | Run collectors (run/string*, run/port*, etc.) |
| `(hafod process)` | Fork, exec, pipelines, process-sleep |
| `(hafod environment)` | Environment variables, with-env, alist operations |
| `(hafod glob)` | File name globbing |
| `(hafod temp-file)` | Temporary file creation |
| `(hafod port-collect)` | Port-to-string/list, port-fold |
| `(hafod process-state)` | cwd, umask, uid/gid, resource alignment |
| `(hafod fileinfo)` | File info, predicates, directory operations |
| `(hafod time)` | Date/time operations, format-date |
| `(hafod system)` | uname, errno handling, version info |
| `(hafod syntax)` | Process notation macros (run, exec-epf, &, etc.) |
| `(hafod re)` | SRE regex (rx, regexp-search, substitution, folding) |
| `(hafod re-adt)` | Regex ADT layer (abstract regex types, smart constructors) |
| `(hafod tty)` | Terminal control (tty-info, flags, baud rates) |
| `(hafod field-reader)` | Field splitters, record readers, join-strings |
| `(hafod awk)` | AWK macro |
| `(hafod pty)` | Pseudo-terminal support (open-pty, fork-pty-session) |
| `(hafod exit-hooks)` | Exit hook registration and execution |
| `(hafod dot-locking)` | Dot-file locking (obtain-dot-lock, with-dot-lock) |
| `(hafod lib-dirs)` | Library directory search |
| `(scsh)` | Compatibility alias -- re-exports everything from `(hafod)` |

Internal libraries (not intended for direct use):

| Library | Description |
|---------|-------------|
| `(hafod internal base)` | Common imports shared across all internal modules |
| `(hafod internal char-sets)` | Predicate-based character sets with Latin-1 enumeration |
| `(hafod internal errno)` | errno/condition infrastructure |
| `(hafod internal posix-constants)` | POSIX constants (flags, modes, signals) |
| `(hafod internal posix-core)` | Core process syscalls (fork, exec, wait, pipe, kill) |
| `(hafod internal posix-file)` | File system operations (stat, chmod, link, etc.) |
| `(hafod internal posix-identity)` | UID/GID, setuid, process groups |
| `(hafod internal posix-misc)` | fnmatch, mkfifo, fsync, uname |
| `(hafod internal posix-regex)` | POSIX regex FFI (regcomp, regexec) |
| `(hafod internal posix-time)` | Time functions (localtime, strftime, etc.) |
| `(hafod internal posix-tty)` | TTY/termios FFI |
| `(hafod internal posix-user)` | User/group database (getpwnam, getgrnam) |
| `(hafod internal re-engine)` | Regex match engine |
| `(hafod internal re-macros)` | rx macro and match-cond |
| `(hafod internal re-parse)` | SRE parser |
| `(hafod internal re-posixstr)` | SRE-to-POSIX string compiler |
| `(hafod internal re-records)` | Regex record types |
| `(hafod internal sre-compile)` | SRE-to-POSIX regex compiler |
| `(hafod internal strings)` | String utilities |
| `(hafod internal tty-constants)` | TTY flag constants |

## Examples

Fifteen example scripts are included in `examples/`, demonstrating
real-world usage of hafod's features:

```sh
# Run any example (from the project root):
scheme --libdirs src --script examples/01-system-info.ss

# Or using the launcher:
hafod -s examples/01-system-info.ss
```

| Script | Description |
|--------|-------------|
| `01-system-info.ss` | System information and process state |
| `02-list-executables.ss` | List all executables on PATH |
| `03-pipeline-demo.ss` | Process notation, pipelines, redirections |
| `04-find-large-files.ss` | Find large files using glob and file-info |
| `05-word-frequency.ss` | Word frequency count using AWK macro |
| `06-log-analyzer.ss` | Parse structured data with field splitters |
| `07-csv-processor.ss` | CSV processing with infix-splitter |
| `08-regex-demo.ss` | SRE regex matching, substitution, folding |
| `09-directory-tree.ss` | Recursive directory listing (tree-style) |
| `10-env-manager.ss` | Environment variable manipulation |
| `11-temp-file-demo.ss` | Temp file creation and I/O channels |
| `12-process-demo.ss` | Fork, wait, signals, background processes |
| `13-file-renamer.ss` | Rename files using regex patterns |
| `14-disk-usage.ss` | Disk usage summary via pipelines |
| `15-backup-script.ss` | Backup files to a timestamped directory |

## Tests

The test suite comprises 54 Scheme test suites (1,900+ assertions) and a
60-test shell-based launcher test:

```sh
make test                       # run all Scheme tests
sh test/test-launcher.sh        # run launcher/CLI tests

# Run a single test suite:
make test-re                    # regex tests
make test-syntax                # process notation tests
make test-awk                   # AWK macro tests
```

## Differences from scsh

hafod aims for high fidelity with the scsh API.  Most scsh scripts
work with minimal changes (often just the shebang line and import).
The differences below are due to the change of host Scheme from
Scheme48 to Chez Scheme.

### Module system

scsh uses Scheme48's `define-structure` / `define-interface` / `open`.
hafod uses R6RS `library` forms.

```scheme
;; scsh
,open scsh

;; hafod
(import (hafod))    ; or equivalently: (import (scsh))
```

Individual subsystems can be imported separately:
`(import (hafod process))`, `(import (hafod re))`, etc.  See the
library table above for the full list.

### Pipe symbol

Chez Scheme's reader treats `|` as a symbol delimiter, so hafod
libraries use `pipe` where scsh uses `|`:

```scheme
;; scsh
(run (| (ls) (grep "foo")))

;; hafod (in library source loaded by bare Chez)
(run (pipe (ls) (grep "foo")))

;; hafod (in scripts run via the launcher)
(run (| (ls) (grep "foo")))   ; works -- launcher preprocesses | → pipe
```

In practice, if you run scripts via `hafod -s`, `|` works as-is.  You
only need `pipe` when loading code as an R6RS library in bare Chez.

### Macros

scsh uses Scheme48 explicit-renaming macros internally.  hafod uses
`syntax-case`.  This is transparent to user code -- the EPF process
notation (`run`, `exec-epf`, `&`, `||`, `&&`) has the same syntax and
semantics, including implicit quasiquoting:

```scheme
;; bare symbols become literal strings, ,var evaluates
(let ([pat "error"])
  (run/string (grep ,pat "file.txt")))
```

### Dynamic state

scsh uses Scheme48 `thread-fluid` for per-thread dynamic state (cwd,
umask, env, etc.).  hafod uses Chez `make-parameter` / `parameterize`.
This only matters if your code used scsh's internal thread-fluid API
directly; the public API (`with-cwd`, `with-env*`, `with-umask`, etc.)
works identically.

### Error handling

scsh uses Scheme48's condition system.  hafod uses R6RS conditions:

```scheme
;; scsh
(with-handler
  (lambda (c more) ...)
  (lambda () (delete-file "/nonexistent")))

;; hafod
(guard (e [(posix-error? e)
           (format #t "~a failed: errno ~a~%"
                   (posix-syscall e) (posix-errno e))])
  (delete-file "/nonexistent"))
```

POSIX errors raise `&posix-error` conditions with `posix-errno` and
`posix-syscall` fields.

### Threading

scsh's green threads (built on Scheme48 internals) are **not** ported.
Instead, hafod provides its own green thread system via
`(import (hafod threads))`:

- Engine-based preemptive scheduling
- `spawn`, `yield`, `thread-sleep`, `thread-join`, `thread-terminate!`
- Go-style channels (synchronous and buffered)
- Thread-local storage

The API is different from scsh's -- it is not a drop-in replacement,
but provides equivalent (and arguably richer) functionality.

### Not ported

These scsh-adjacent features are not available in hafod:

- **Berkeley DB** (ndbm/dbm bindings) -- an optional scsh extension,
  not part of the core API.  Use a Chez FFI library or shell out to a
  DB tool if needed.
- **MD5 hashing** -- an optional scsh extension, not part of the core
  API.  Use a Chez library or shell out to `md5sum`.
- **Scheme48 VM internals** -- scsh's implementation used Scheme48
  primitives (placeholders, port handlers, interrupt masking, weak
  tables) internally.  These were never part of scsh's public API.
  hafod replaces them with Chez equivalents: `make-parameter` for
  thread-fluids, guardians for weak references, R6RS conditions for
  error handling, `critical-section` for interrupt masking.

### New features (not in scsh)

hafod adds several capabilities beyond the original scsh:

- **`posix_spawn` fast path** -- the `run` macro transparently uses
  `posix_spawn(3)` for simple commands, yielding ~1.8x faster
  fork/exec.  Pipelines, redirections, and `begin` forms fall back to
  `fork`+`exec`.
- **Green threads with channels** -- `(hafod threads)` provides
  lightweight concurrency with Go-style channels
- **C `glob(3)` fast path** -- brace-free glob patterns use the C
  library directly
- **Whole-program optimisation** -- `make` produces a merged `.so`
  via `compile-whole-library` for faster startup
- **Exit hooks** -- `(hafod exit-hooks)` for registering cleanup actions
- **Dot-file locking** -- `(hafod dot-locking)` for advisory file locks
- **Library directory search** -- `(hafod lib-dirs)` for finding files
  along a search path
- **Enhanced launcher** -- `-l` preload files, `-e` entry point,
  `--` REPL mode, `|` preprocessing

### Quick migration checklist

To port a scsh script to hafod:

1. Change the shebang: `#!/usr/bin/env hafod`
2. Change imports: `(open scsh)` → `(import (hafod))` (or keep
   `(import (scsh))` which also works)
3. `|` works as-is in scripts run via `hafod -s`; use `pipe` only if
   loading as an R6RS library in bare Chez
4. Change error handlers: `with-handler` → `guard`
5. Most scripts work unchanged -- 1,028 scsh-compatible symbols are
   exported

## Performance

hafod vs scsh 0.6.7 on Scheme48, best-of-3 runs.  Ratio < 1.0 means
hafod is faster.

| Benchmark | hafod (s) | scsh (s) | Ratio | Winner |
|-----------|-----------|----------|-------|--------|
| fork-exec (500 runs) | 0.993 | 1.714 | 0.58x | hafod |
| pipeline (100 runs) | 0.785 | 1.163 | 0.67x | hafod |
| string I/O (500 runs) | 0.451 | 1.077 | 0.42x | hafod |
| regex match (10k runs) | 0.118 | 0.527 | 0.22x | hafod |
| file ops (500 runs) | 0.142 | 0.419 | 0.34x | hafod |
| computation | 0.743 | 1.600 | 0.46x | hafod |
| env ops (10k runs) | 0.120 | 0.169 | 0.71x | hafod |
| glob (100 runs) | 0.106 | 0.091 | 1.16x | scsh |
| startup | 0.101 | 0.026 | 3.88x | scsh |
| read-line (500 runs) | 0.142 | 0.538 | 0.26x | hafod |
| field split (5k runs) | 0.126 | 1.004 | 0.13x | hafod |
| AWK (200 runs) | 0.125 | 0.212 | 0.59x | hafod |
| regex subst (5k runs) | 0.134 | 1.921 | 0.07x | hafod |
| output capture (500 runs) | 0.918 | 1.797 | 0.51x | hafod |
| temp file (200 runs) | 0.115 | 0.052 | 2.21x | scsh |
| with-cwd (5k runs) | 0.114 | 0.121 | 0.94x | hafod |

**Summary:** hafod wins 14 of 16 benchmarks.  The largest gains are in
regex substitution (14x faster), field splitting (8x faster), and
read-line throughput (3.8x faster).  scsh wins on startup time
(Scheme48's image-based startup is faster than Chez's library loading)
and temp file creation (dominated by startup overhead -- hafod's
per-operation time is actually faster).

The benchmark suite is in `bench/`.  Run `perl bench/run-benchmarks.pl`
to reproduce.

## Relationship to scsh

The original [scsh](https://scsh.net/) was written by Olin Shivers and
runs on Scheme48.  hafod is a clean-room re-implementation of the scsh
API for Chez Scheme, using the original scsh documentation and source as
a specification.  The scsh test suite has been ported and passes in full for
all implemented features.

## Licence

ISC licence.  See [LICENCE](LICENCE) for details.

Portions derived from scsh are under the BSD 3-Clause licence (see
LICENCE for the full text of both licences).
