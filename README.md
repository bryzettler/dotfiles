# dotfiles

Nix-darwin configuration for macOS with home-manager.

## Stack

- **Shell**: Fish + Starship
- **Terminal**: Ghostty + tmux
- **Editor**: Emacs (terminal mode)
- **Theme**: Catppuccin Mocha

## Prerequisites

Install Nix:

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

## Installation

1. Clone this repo:

```bash
git clone https://github.com/bzettler/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
```

2. Build and switch:

```bash
nix run nix-darwin -- switch --flake .
```

## Usage

Rebuild after changes:

```bash
darwin-rebuild switch --flake ~/.dotfiles
```

Or use the fish alias:

```bash
rebuild
```

## Structure

```
.
├── flake.nix                 # Nix flake entry point
├── config/
│   ├── emacs/init.el         # Emacs configuration
│   └── ghostty/config        # Ghostty terminal config
├── hosts/
│   └── zyrbbook/             # Host-specific config
│       ├── default.nix       # Darwin system config
│       └── home.nix          # Home-manager config
└── modules/
    ├── darwin/               # macOS system modules
    │   ├── homebrew.nix      # Brew packages
    │   └── system.nix        # macOS defaults
    └── home/                 # Home-manager modules
        ├── editors/          # Editor configs
        ├── git.nix           # Git config
        ├── packages/         # Language packages
        ├── shell/            # Shell configs
        ├── terminal/         # Terminal configs
        └── tools/            # Dev tools
```
