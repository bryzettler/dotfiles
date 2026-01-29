# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Nix-darwin + home-manager dotfiles for macOS (Apple Silicon). Declarative system configuration using Nix Flakes.

## Key Commands

```bash
rebuild                    # Apply all changes (alias for darwin-rebuild switch --flake ~/.dotfiles)
nix-collect-garbage -d     # Clean old generations
```

After modifying:
- `.nix` files: run `rebuild`
- `config/emacs/init.el`: restart Emacs
- `config/ghostty/config`: restart terminal
- Fish config: reload shell (`exec fish`)

## Architecture

```
flake.nix                 # Entry point - nixpkgs-unstable, nix-darwin, home-manager inputs
scripts/bootstrap.sh      # Fresh machine setup script
hosts/zyrbbook/
  ├── default.nix        # Darwin system config (imports darwin modules)
  └── home.nix           # Home-manager config (imports home modules)
modules/
  ├── darwin/
  │   ├── homebrew.nix   # Brew packages (casks, formulae)
  │   └── system.nix     # macOS defaults (Dock, Finder, keyboard)
  └── home/
      ├── shell/         # Fish + Starship
      ├── terminal/      # Ghostty + tmux
      ├── editors/       # Emacs (terminal-only, straight.el)
      ├── tools/         # ripgrep, fd, yazi, htop
      ├── packages/      # fnm, node setup
      ├── theme/         # Catppuccin Mocha palette
      ├── git.nix        # Git + delta config
      └── fonts.nix      # iA Writer Quattro
config/
  ├── emacs/init.el      # Elisp config (straight.el + use-package)
  ├── ghostty/config     # Terminal emulator settings
  └── obsidian/          # Vault settings
```

## Patterns

**Module Organization**: One `.nix` file per tool. Host-specific overrides in `hosts/`.

**File Linking**: Non-Nix configs linked via `home.file`:
```nix
home.file.".emacs.d/init.el".source = ../../../config/emacs/init.el;
```

**Adding Packages**:
- GUI apps: `modules/darwin/homebrew.nix` → `casks`
- CLI tools: `modules/darwin/homebrew.nix` → `brews` or add to relevant home module
- Nix packages: add to relevant module's `home.packages`

## Key Configurations

- **Shell**: Fish + Starship, auto-starts tmux "main" session
- **Terminal**: Ghostty (90% opacity, VS Code Dark+ colors)
- **tmux**: Prefix `Ctrl+\`, sessions persist via continuum
- **Emacs**: Terminal-only (`emacs -nw`), 24-bit color, LSP via eglot
- **Git**: delta pager, rerere enabled, editor is `emacs -nw`

## Environment

- Node: fnm (auto-switch via `.nvmrc`)
- Ruby: rbenv
- Rust: cargo in PATH
- direnv: `.envrc` support enabled
