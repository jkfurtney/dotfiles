# dotfiles

Personal dotfiles: shell configs (`.bashrc`, `.zshrc`, `bashrc_windows`),
Emacs (`.emacs` + supporting `.el` files), assorted scripts (`.py`, `.ahk`,
`.sh`), and small standalone Emacs Lisp packages (`npy-view.el`,
`doctest-mode.el`, `blacken.el`, etc). Used across Linux, macOS, and
Windows machines from the same checkout.

## .emacs

Single monolithic init file (`~/src/dotfiles/.emacs` on Linux/macOS,
`c:/src/dotfiles/.emacs` on Windows). Platform branching is done at the top
via `jkf/windows-p` (checks `system-type`) and, further down, a
`(pcase system-name ...)` block for per-machine tweaks (frame size, fonts,
etc). Most custom functions/variables are prefixed `jkf/`.

Packages are managed with plain `package.el`: the `my-packages` list
(~line 103) is installed via `package-install-selected-packages` near the
top of the file, before most other configuration runs. Anything that needs
to exist on `exec-path`/`PATH` *before* that call (e.g. a compiler needed to
build a package's native module) has to be set up earlier still, in the
`jkf/windows-p` branch near the top of the file (~line 86), not in the
later "windows specific setup" block (~line 441) where most other Windows
PATH additions live — that block runs too late for anything package
installation depends on.

### Spell checking setup

Two spell-checkers are configured and coexist:

- **jinx** (default) — auto-enabled via `text-mode-hook`, `rst-mode-hook`,
  `org-mode-hook`, `python-mode-hook`.
- **flyspell** + hunspell/ispell (fallback) — not auto-enabled, but fully
  configured and available.

Toggle either explicitly in a buffer with `C-c s j` (jinx) / `C-c s f`
(flyspell), via `jkf/spell-checker-jinx` / `jkf/spell-checker-flyspell`.

**Why two exist**: flyspell/ispell.el drive hunspell as a long-lived
subprocess over a pipe. The first spawn (parsing the affix/dictionary
files) is slow and used to stall Emacs on first use. Two fixes were
layered in, in order:

1. Pre-warm hunspell in the background. In the Windows `ispell` setup
   block (search for `ispell-init-process`), an idle timer calls
   `ispell-init-process` a couple seconds after startup so the slow spawn
   happens before the user's first interactive spell-check. This alone
   fixes the flyspell path and needs no extra tooling.
2. Adopt **jinx** as the default. jinx binds directly to libenchant's C
   API via a compiled dynamic module instead of spawning a subprocess at
   all, so there's no per-check IPC and no startup stall to hide.

**Windows-specific build chain for jinx** (this is the part with sharp
edges — worth reading before touching it again):

jinx compiles its own native module (`jinx-mod.c` → `jinx-mod.dll`) the
first time `jinx-mode` runs, by shelling out to a C compiler and
`pkg-config --cflags --libs enchant-2`. None of that ships with Windows or
with Emacs, so the machine needed:

- **MSYS2** installed at `C:\msys64`.
- The `mingw-w64-x86_64-{gcc,pkgconf,enchant,hunspell,hunspell-en}`
  packages, all from MSYS2's `mingw64` repo — deliberately *not* mixed
  with the unrelated MinGW-w64 gcc that ships with Strawberry Perl
  (`C:\Strawberry\c\bin\gcc.exe`), to avoid linking a compiler and a
  libenchant build from two different toolchains against each other.
- `C:\msys64\mingw64\bin` added to `exec-path`/`PATH` early in `.emacs`
  (before package installation) — this is what lets jinx's `call-process`
  find `gcc`/`pkg-config` to *compile* the module.
- `C:\msys64\mingw64\bin` **also** added to the Windows **user-level PATH
  environment variable** (outside Emacs — via registry/`setx`, not
  `setenv` in `.emacs`). This is the non-obvious part: the compiled
  `jinx-mod.dll` is loaded with `module-load`, which does a Windows
  `LoadLibrary` that needs `libenchant-2-2.dll` on the **process's PATH at
  the moment Emacs started**. Setting `PATH` from inside `.emacs` via
  `setenv` is too late for this — it was tested directly and confirmed to
  fail; only a PATH set before the Emacs process launches works. Concretely:
  **after any change to this PATH entry, Emacs must be fully restarted**
  (a running Emacs, or one launched from an already-running shell that
  predates the change, won't pick it up).

**Known-benign warning**: enchant's `nuspell` provider plugin
(`enchant_nuspell.dll`) fails to load with a "specified procedure could
not be found" warning on this setup. Harmless — enchant falls back to the
`hunspell` provider (from `mingw-w64-x86_64-hunspell` +
`mingw-w64-x86_64-hunspell-en`), which is what's actually used and works
correctly for `en_US`.

**Gotchas hit during first install** (both one-time, both resolved):

- `package-install jinx` can fail with `...tar: Not found` if the local
  MELPA archive index is stale — MELPA only keeps the latest build of a
  package, so an old cached `archive-contents` can point at a filename
  that's since been pruned. Fix: `M-x package-refresh-contents` before
  retrying the install.
- The very first native-module compile can fail inside Emacs
  (`*jinx module compilation*` buffer shows the `gcc` command and
  "Compilation ... failed" with *no* compiler diagnostics at all — exit
  code nonzero, zero output). Running the exact same `gcc` command by hand
  in the package's ELPA directory succeeded immediately and produced a
  working `jinx-mod.dll` — this looked like a one-off (possibly
  antivirus/Defender scanning a freshly-written, unsigned `.dll` on first
  sight). Since jinx only recompiles when `jinx-mod.dll` is missing,
  building it once by hand is enough to unblock `jinx-mode` for good.

The local hunspell dictionary (`c:/src/dotfiles/dict-en-20260101`, tracked
alongside `dict-en-20260101.zip`) is still what both spell-checkers use for
`en_US` — flyspell/ispell via `ispell-hunspell-dict-paths-alist`/`DICPATH`,
jinx via enchant's own hunspell provider picking up the same dictionary
files through MSYS2's `mingw-w64-x86_64-hunspell-en` package.
