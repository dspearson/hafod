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
- **SRFI-1 and SRFI-13** -- the full SRFI-1 list library (97 procedures)
  and SRFI-13 string library (67 procedures) are available at the top
  level via `(import (hafod))`, matching scsh's environment.  All 35
  SRFIs that scsh bundled via Scheme 48 are available as
  `(import (hafod srfi-N))`
- **Full scsh compatibility** -- 1:1 coverage of the scsh public API
  (1,349 exported symbols); `(import (scsh))` works as an alias for
  `(import (hafod))`; all scsh accessor names, predicates, char-sets,
  file-options, RE ADT layer, and version aliases

## Requirements

- [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.x or later
- A POSIX operating system (Linux, macOS, *BSD)
- C library with standard POSIX interfaces (glibc, musl, etc.)

## Building

```sh
make                    # build the native binary (libraries are compiled first)
make test               # run the full test suite
```

If `scheme` is not on your PATH or is named differently:

```sh
make SCHEME=/path/to/chez-scheme
```

If you have [`just`](https://github.com/casey/just) installed (it ships in the
Nix dev shell), the common workflows are available as recipes:

```sh
just                    # list all recipes
just build              # build the native binary
just test               # run the test suite, hang-proofed (per-test timeout)
just install            # build and install to /usr/local (uses sudo)
just version            # print the version resolved from git tags
```

### Build modes

hafod supports three build modes:

| Mode | Command | Output | Size | Startup | Runtime dependencies |
|------|---------|--------|------|---------|---------------------|
| Native (default) | `make` | `bin/hafod-native` | ~850KB | ~85ms | boot files + compiled libs (copied in on install) |
| Library | `make compile` | `bin/hafod` | n/a | ~74ms† | `scheme` on PATH |
| Standalone | `make standalone` | `bin/hafod-standalone` | ~5.1MB | ~62ms | none |

† Since 1.9 the library launcher's image is whole-program-merged: every `(hafod)`
library is inlined into `bin/hafod.so`, so the launcher loads one image instead of
some sixty.  On the development host that cut its startup by ~15% (164ms → 140ms,
best of seven); the figures in the table are indicative and host-dependent.

**Native** (default) links against Chez's `libkernel.a` for a small native
binary.  In the build tree it loads `petite.boot`/`scheme.boot` and the
compiled `.so` libraries from the install directory; `make install` *copies*
those in (see [Installation](#installation)), so the installed binary is
self-contained and needs no Chez Scheme on the target.

**Library** compiles all libraries and uses the `bin/hafod` shell wrapper that
invokes `scheme`.  No C compiler needed; handy for development.

**Standalone** embeds LZ4-compressed vfasl boot files and the launcher
program into a single self-contained binary.  No external files needed
at runtime — recommended for single-file distribution.

### Nix

A `flake.nix` is provided for development.  The dev shell includes Chez Scheme
and [`just`](https://github.com/casey/just):

```sh
nix develop             # enter a shell with Chez Scheme + just on PATH
make && make test       # or: just build && just test
```

> Build and install from inside the dev shell (or otherwise with a single
> `SCHEME`) so every artefact uses the same Chez version.  Mixing Chez
> versions across the binary, boot files, and compiled `.so`s causes
> "incompatible fasl-object version" errors at load time.  The build records
> the Chez version it used and the launcher asserts it, refusing a mismatched,
> Rosetta, or cross build with an actionable message rather than a raw fasl
> error.

### Versioning

The version has a single source of truth — **git tags** — so there is nothing
to hand-edit per release.  `tools/gen-version.sh` runs at build time and
resolves the version from `git describe --tags`, falling back to the tracked
`VERSION` file for builds without a `.git` (release tarballs, Nix sources).
To cut a release:

```sh
git tag v1.9               # what `hafod --version` and the man page report
printf '1.9\n' >| VERSION  # keep the fallback in step (>| ignores set -o noclobber)
```

## Installation

The native install is self-contained: `make install` copies `petite.boot`,
`scheme.boot`, and the compiled libraries into the install directory, so the
installed `hafod` needs no Chez Scheme on the target system.

```sh
make                                  # build the native binary
sudo make install                     # installs to /usr/local
```

For a single-file binary instead, build standalone first
(`make standalone && make install`).

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

## Configuration

hafod loads `~/.config/hafod/init.ss` on startup in interactive mode.
The config directory follows the XDG Base Directory specification: if
`XDG_CONFIG_HOME` is set, hafod loads `$XDG_CONFIG_HOME/hafod/init.ss`
instead.

Config files are plain Scheme, evaluated in the interaction environment.
Any definitions or side effects take effect before the first REPL prompt:

```scheme
;; ~/.config/hafod/init.ss
(set-prompt! "hafod> ")
(bind-key! "C-x C-e" cmd-open-below)
(enable-paredit!)

;; Feature toggles (all default to #t)
(shell-mode? #f)           ; disable shell-compat mode (pure Scheme REPL)
(rainbow-identifiers? #f)  ; disable rainbow identifier colouring
(rainbow-parens? #f)       ; disable depth-coloured parentheses
(syntax-highlight? #f)     ; disable string/comment/number/boolean colours
(fuzzy-finder? #f)         ; disable Ctrl-R/Ctrl-T/Alt-C fzf pickers
(tab-completions? #f)      ; disable tab completion (Tab inserts literal tab)
(history-expansion? #f)    ; disable !! !$ !n history expansion
```

Pass `--no-config` (or `--norc`) to skip config loading.

If the config file contains an error, hafod displays the filename and
error message, then continues to the REPL normally -- config errors are
never fatal.

Config can be split across files using `(load "other-file.ss")` from
within `init.ss`.

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

### Shell Mode

In the interactive REPL, commands can be typed directly without
`(run ...)` wrapping.  If the first token matches a PATH executable or
a built-in command, the input is executed as a shell command:

```
> ls -la
> cat file.txt | grep pattern
> ls *.ss > files.txt
> echo $HOME
> mkdir -p foo && touch foo/bar
> make clean || echo "no Makefile"
> sleep 10 &
> jobs
> fg %1
> cd /tmp
> pushd /var/log
> export EDITOR=vim
> echo file.{c,h,o}
> !!
```

Input starting with `(`, `'`, `` ` ``, `#`, or `,` is always treated as
Scheme.  Scheme keywords (such as `define`, `lambda`, `if`) override
PATH executables with the same name.  Ambiguous input defaults to Scheme
for safety.

Built-in commands: `cd` (with `cd -` to return to the previous
directory), `pushd`/`popd` (directory stack), `export VAR=value`,
`jobs`, `fg`, and `bg`.

Shell mode is only active in the interactive REPL -- it does not apply
to scripts or `-c` expressions.

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
| `(hafod config)` | Config API (set-prompt!, bind-key!, XDG config loading) |
| `(hafod shell classifier)` | Shell mode input classifier |
| `(hafod shell parser)` | Shell command parser (pipes, redirects, globs) |
| `(hafod shell builtins)` | Shell builtins (cd, pushd, popd, export, jobs, fg, bg) |
| `(hafod shell history-expand)` | History expansion (!!, !$, !n, !prefix) |
| `(hafod shell jobs)` | Job control (job table, fg/bg, process groups, signals) |
| `(hafod shell completers)` | Programmable completions (git, ssh, kill, make) |
| `(hafod fuzzy)` | Fuzzy matching (Smith-Waterman DP, search syntax) |
| `(hafod finder)` | Full-screen fuzzy finder (fzf-style interactive picker) |
| `(hafod dot-locking)` | Dot-file locking (obtain-dot-lock, with-dot-lock) |
| `(hafod lib-dirs)` | Library directory search |
| `(hafod srfi-1)` | SRFI-1 list library (re-exported by `(hafod)`) |
| `(hafod srfi-13)` | SRFI-13 string library (re-exported by `(hafod)`) |
| `(hafod srfi-N)` | All 35 SRFIs bundled by scsh (2, 4--9, 11, 16--17, 19, 23, 25--28, 31, 34, 37, 39--40, 42--43, 45, 60--61, 63, 66--67, 71, 74, 78, 95) |
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
| `(hafod internal platform-constants)` | Platform-specific struct offsets and constants |
| `(hafod editor input-decode)` | Terminal input decoder, wcwidth display width |
| `(hafod editor keymap)` | Trie-based keymap with composable layers |
| `(hafod editor render)` | Line editor rendering with syntax colouring and feature toggles |
| `(hafod editor history)` | SQLite-backed persistent history with mode tracking |
| `(hafod editor vi)` | Full vim emulation (motions, operators, text objects, visual, search) |
| `(hafod editor help)` | Keybinding reference and interactive tutorial |
| `(hafod editor editor)` | Gap-buffer line editor with paredit |

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

The test suite comprises 122 Scheme test suites (3,800+ assertions) and a
108-test shell-based launcher test:

```sh
make test                       # run all Scheme tests + launcher + umbrella
sh test/test-launcher.sh        # run launcher/CLI tests

# Run a single test suite:
make test-re                    # regex tests
make test-syntax                # process notation tests
make test-awk                   # AWK macro tests
```

The suites are discovered by a glob, so `make test` and `just test` run a
wiring audit (`tools/check-test-wiring.sh`) before the suites themselves.  It
fails the run if a test file is neither wired in nor deliberately excluded, and
if a harness library has leaked into the suite list -- either would otherwise be
silent, since a suite that is never run reports nothing, and Chez accepts a
`(library ...)` form handed to `--script` and exits 0 having asserted nothing.

Some suites exercise the interactive editor, PTYs, and signals.  Both `make
test` and `just test` redirect every suite's stdin from `/dev/null`, so an
interactive suite can never block waiting for keystrokes.  `just test`
additionally gives each suite a per-test timeout, so even a genuine hang or
infinite loop can't stall the run:

```sh
just test                       # hang-proofed full run (recommended)
just test-one editor            # a single suite by name
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
- **Whole-program optimisation** -- both the umbrella library and the
  launcher image are whole-program-merged (`compile-whole-library` for the
  libraries, and `compile-whole-program` inlining every library into
  `bin/hafod.so`) so the library-mode launcher loads a single image for
  faster startup
- **Exit hooks** -- `(hafod exit-hooks)` for registering cleanup actions
- **Dot-file locking** -- `(hafod dot-locking)` for advisory file locks
- **Library directory search** -- `(hafod lib-dirs)` for finding files
  along a search path
- **Enhanced launcher** -- `-l` preload files, `-e` entry point,
  `--` REPL mode, `|` preprocessing, and `--batch` (or the `HAFOD_BATCH`
  environment variable) to force the plain non-editor REPL path
- **Shell mode** -- bare command execution in the REPL, input
  classifier, pipes, redirects, globs, env vars, builtins (cd,
  pushd, popd, export), `&&`/`||`/`;` chaining, `&` background,
  brace expansion (`{a,b,c}`, `{1..5}`, nested cross-product),
  history expansion (`!!`, `!$`, `!n`, `!prefix`)
- **Job control** -- job table, `fg`/`bg`/`jobs` builtins, process
  groups, SIGTSTP/SIGTTIN/SIGTTOU handling, background job
  notifications at prompt
- **Interactive editor** -- gap-buffer line editor with syntax
  colouring (rainbow parens, rainbow identifiers, strings,
  comments, numbers, booleans), smart enter, bracketed paste,
  prefix-filtered Up/Down, undo/redo (C-/ and M-/), fish-style
  auto-suggestions, command timing display, terminal-wrap-aware
  multiline rendering, `(show-keybindings)` reference,
  `(run-tutorial)` interactive walkthrough
- **Full vim emulation** -- vi normal mode with motions (w/W/b/B/e/E,
  f/F/t/T, 0/^/$, gg/G, %), operators (d/c/y with text objects),
  visual mode (v/V), search (/pattern, n/N, */#), registers
  ("\{reg}), marks (m{char}, '{char}), count prefixes ({n}{cmd}),
  and dot-repeat.  State encapsulated in a single record for clean
  session management
- **fzf-style fuzzy finder** -- full-screen fuzzy picker on
  alternate screen buffer with real-time filtering:
  - Ctrl-R: history search with mode-aware colouring (Scheme
    entries get rainbow parens/identifiers, shell entries render
    plain), numbered candidates, history mode stored in SQLite
  - Ctrl-T: file picker (`git ls-files` in repos, recursive
    walk outside); auto-inserts space before filename when needed
  - Alt-C: directory picker with `cd` on selection
  - Extended search syntax: `!negation`, `^prefix`, `.suffix`,
    `'exact`, terms separated by spaces (AND)
- **Fish-style tab completion** -- multi-column grid layout with
  arrow key navigation, coloured directories (blue with `/`),
  muted indigo selection highlight, scrolling pager with row
  indicator; falls back to single-column when descriptions are
  present
- **Fuzzy matching** -- Smith-Waterman DP scoring for completions
  and search, extended search syntax (`!prefix` negation, `^exact`
  anchoring, `.suffix`), Unicode normalisation (é→e, ñ→n),
  tiebreak by length then first-match position
- **Programmable completions** -- command-specific completers for
  git (subcommands, branches, modified files), ssh (hosts from
  `~/.ssh/config` and `known_hosts`), kill (PIDs with process
  names), make (targets from Makefile), with description display
  in completion menu; user-extensible via `register-completer!`
- **Paredit** -- structural editing with auto-pairing, toggleable
  at runtime via `toggle-paredit!`
- **Feature toggles** -- all non-core features can be individually
  enabled/disabled via parameters: `shell-mode?`,
  `rainbow-identifiers?`, `rainbow-parens?`, `syntax-highlight?`,
  `fuzzy-finder?`, `tab-completions?`, `history-expansion?`
- **Config system** -- XDG-compliant `~/.config/hafod/init.ss`,
  `set-prompt!`, `bind-key!` with Emacs-style key descriptions,
  `--no-config` flag
- **Terminal & environment awareness** -- the interactive UI degrades
  gracefully when the output is not a capable terminal: colour, cursor
  control, editor escapes, the paren flash, completion menus and the
  keybinding cheatsheet are each gated on the actual target, `NO_COLOR`
  and `TERM=dumb` are honoured, colour can be forced or suppressed with
  `--color=auto|always|never` or the `CLICOLOR_FORCE` environment
  variable (`NO_COLOR` takes precedence over `CLICOLOR_FORCE`), and the
  line editor engages only when
  *both* stdin and stdout are terminals -- so `hafod | cat`,
  `hafod > log`, and `hafod < script` produce clean, escape-free output
  (a broken downstream pipe exits quietly, matching a normal filter).
  `--batch` / `HAFOD_BATCH` forces the plain non-editor path.  The
  terminal is always restored to cooked mode on exit, on an uncaught
  error, on a fatal signal, and across a Ctrl-Z suspend, so a crash or
  `^Z` never leaves the shell wedged in raw mode.

### Quick migration checklist

To port a scsh script to hafod:

1. Change the shebang: `#!/usr/bin/env hafod`
2. Change imports: `(open scsh)` → `(import (hafod))` (or keep
   `(import (scsh))` which also works)
3. `|` works as-is in scripts run via `hafod -s`; use `pipe` only if
   loading as an R6RS library in bare Chez
4. Change error handlers: `with-handler` → `guard`
5. Most scripts work unchanged -- 1,349 scsh-compatible symbols are
   exported, including SRFI-1 and SRFI-13 at the top level

## Performance

hafod 1.4 (Chez Scheme 10.0) vs scsh 0.7 (Scheme48 1.9.2) on
x86_64 Linux.  In-process timing (startup excluded) — each runtime
is started once and all operations are measured within the process.
Ratio < 1.0 means hafod is faster.

| Benchmark | N | hafod (ms) | scsh (ms) | Ratio | Winner |
|-----------|---|-----------|-----------|-------|--------|
| fork-exec | 500 | 1750 | 892 | 1.96x | scsh |
| pipeline | 200 | 1726 | 1275 | 1.35x | scsh |
| string I/O | 200 | 302 | 839 | 0.36x | hafod |
| regex match | 10k | 5 | 237 | 0.02x | hafod |
| file ops | 500 | 8 | 62 | 0.12x | hafod |
| computation (fib 35) | 1 | 766 | 1406 | 0.54x | hafod |
| env ops | 50k | 30 | 279 | 0.11x | hafod |
| glob | 100 | 659 | 7849 | 0.08x | hafod |
| read-line | 500 | 760 | 2494 | 0.30x | hafod |
| field split | 5k | 22 | 1815 | 0.01x | hafod |
| AWK | 200 | 302 | 990 | 0.31x | hafod |
| regex subst | 5k | 11 | 430 | 0.03x | hafod |
| output capture | 500 | 3029 | 1597 | 1.90x | scsh |
| temp file | 2k | 22 | 204 | 0.11x | hafod |
| with-cwd | 50k | 115 | 819 | 0.14x | hafod |

**Summary:** hafod wins 12 of 15 benchmarks.  The largest gains are
in field splitting (82x), regex matching (51x), regex substitution
(39x), and glob (12x).  scsh wins fork-exec, pipeline, and output
capture — all process-creation-heavy workloads where Scheme48's
smaller address space makes `fork(2)` cheaper (less copy-on-write
overhead).

Startup is not included in these numbers.  Chez Scheme's library-based
startup (~84ms) is slower than Scheme48's image resume (~13ms); for
short-lived scripts, use the standalone build (`make standalone`) which
reduces startup via embedded boot files.

hafod green threads (no scsh equivalent):

| Benchmark | Description | Time (ms) |
|-----------|-------------|----------|
| threads-10k | spawn+join 10,000 threads | 24 |
| channel-50k | 50,000 messages through buffered channel | 6 |
| preempt-100x10k | 100 preempted threads, 10k work each | 40 |
| ring-50x5k | 50-thread ring, 5,000 token passes | 314 |

The in-process benchmark scripts are in `bench/all-hafod.ss` and
`bench/all-scsh.scm`.  The process-level runner (including startup)
is `perl bench/run-benchmarks.pl`.

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
