# bash-ts-mode

[![CI](https://github.com/konomanoasa/bash-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/bash-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
GNU Bourne Again SHell 5.3.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/bash-ts-mode")
```

## Automatic Activation

Enabled for `.bash` files, `.bashrc`, `.bash_profile`, `.bash_login`, and
`.bash_logout` without a shebang, and scripts with a `bash` shebang.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                                                                         |
| ----- | --------------------------------------------------------------------------------- |
| 1     | Comments                                                                          |
| 2     | Keywords, function definitions, command calls, and strings                        |
| 3     | Numbers, constants, variables, and escapes outside shell patterns and static EREs |
| 4     | Operators, punctuation, brackets, shell patterns, and static EREs                 |

## Parsing Conditions

Set parsing conditions for matching files in `.editorconfig`.

```ini
[*.bash]
extglob = false
extquote = true
```

Both properties accept `true` and `false`.
The defaults are `extglob = false` and `extquote = true`.
Restart the mode to apply changes.

## Grammar

[tree-sitter-bash](https://github.com/konomanoasa/tree-sitter-bash)

## License

[MIT](LICENSE)
