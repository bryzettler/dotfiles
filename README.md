# dotfiles

Nix-darwin configuration for macOS with home-manager.

## Stack

- **Shell**: Fish + Starship
- **Terminal**: Ghostty + tmux
- **Editor**: Emacs (terminal mode)
- **Theme**: Catppuccin Mocha

## Installation

Run the bootstrap script:

```bash
curl -fsSL https://raw.githubusercontent.com/bryzettler/dotfiles/master/scripts/bootstrap.sh | bash
```

This will install Nix, clone the dotfiles, and configure your system.

## Usage

Rebuild after changes:

```bash
rebuild
```

## Structure

```
.
├── flake.nix                 # Nix flake entry point
├── scripts/
│   └── bootstrap.sh          # Fresh machine setup
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
