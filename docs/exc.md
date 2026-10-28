# Spreadsheets with exc.el

The `.emacs` configuration follows the [upstream Straight instructions](https://github.com/yibie/exc.el#with-straight-doom-or-elpaca).
Straight fetches and byte-compiles `ecss`, `ebox`, `etaf`, and `exc`, including
its plugins. Emacs 29.1 or later is required.

## Use

- Open or create `data.grid` with `C-x C-f`.
- Open CSV with `M-x exc-find-file-csv`; `C-x C-s` saves CSV.
- Open XLSX with `M-x exc-xlsx-find-file`.
- Export XLSX with `M-x exc-xlsx-export`.
- In a grid, type to replace a cell, or use `RET`/`F2` to edit it.
  Formulas such as `=SUM(A1:A3)` work directly.

XLSX imports are not tied to the workbook: `C-x C-s` saves a `.grid`
file. Use the export command to write XLSX.

## Linux XLSX module

Straight compiles the Lisp, but not the Rust XLSX module. Emacs must have
dynamic module support. With Cargo and a C linker available, run
`M-x exc-xlsx-build-module` and wait for compilation to finish.

On this NixOS machine, this command supplies the missing C linker:

```sh
nix shell nixpkgs#gcc --command bash -c \
  'cd "$HOME/.emacs.d/straight/build/exc/xlsx" && cargo build --release'
```

The module must be built under Straight's **build** directory, where the
loaded plugin searches for it. If you build in the repository instead,
set `exc-xlsx-module-file` to that library's absolute path.

Rebuild the module after updating exc.el, or if Straight rebuilds the
package and removes the module. A missing Linux module reports
“No prebuilt xlsx module”; loading `exc-xlsx` alone does not test it.

## Verification

The Straight installation and Linux dynamic module load were tested with
Emacs 30.2: 438 of 441 upstream tests passed, including grid editing, CSV
saving, and XLSX import/export. Two financial-formula tests also fail when
run directly from upstream source without Straight. The plugin-directory test assumes
upstream's directory layout rather than Straight's flattened plugin files.

The test suite needs `zip` and `unzip`. Use Bash as Emacs's
`shell-file-name` when testing in a temporary Nix shell: this machine's Zsh
startup drops the temporary tools from `PATH`.

The complete `.emacs` also loaded successfully in a fresh batch Emacs,
opened a new `.grid` buffer in `exc-mode`, and loaded the Linux XLSX
module. A graphical check needs an available display output.

## Activate the configuration

Launch this checkout directly without Home Manager or a NixOS rebuild:

```sh
emacs -q \
  --eval '(setq user-init-file "/home/lucas/git/luckysori/dotemacs/.emacs")' \
  -l /home/lucas/git/luckysori/dotemacs/early-init.el \
  -l /home/lucas/git/luckysori/dotemacs/.emacs
```

Setting `user-init-file` lets the configuration find its adjacent `priv/`
directory. Alternatively, evaluate the `Spreadsheets` section in an
existing Emacs, or deploy through the normal Home Manager workflow.
