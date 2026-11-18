# Python

Python uses Tree-sitter, lsp-mode with basedpyright and Ruff, Corfu, and
Apheleia. Existing `C-c l` LSP commands apply; `C-x C-r` renames and
`C-x C-a` offers code actions. Saving sorts imports and formats with Ruff,
using the project's Ruff configuration. `C-c C-p` starts Python via `uv run python`.

Provide `basedpyright-langserver`, `ruff`, and `uv` on PATH. For Zenvi these
come from the private metarepo `flake.nix`, loaded by `.envrc` with
`use flake path:.`. Existing envrc/inheritenv integration supplies that
buffer-local environment inside sub-repos too, unless a nearer `.envrc`
overrides it. A child `.envrc` can use `source_up` to compose with the parent.

Run `uv sync` in each Python repo (with its normal dependency groups and
workspace flags). basedpyright discovers the nearest `.venv`; it runs
separately per LSP project so different repos do not share an interpreter.
Choose the sub-repo, not the metarepo, if LSP asks for a project root.

After changing the flake, run `M-x envrc-reload-all`; restart existing Python
workspaces with `M-x lsp-workspace-restart`. After initially installing this
configuration, restart Emacs. If dependencies were synced after starting
LSP, restart that workspace to rediscover its interpreter.
