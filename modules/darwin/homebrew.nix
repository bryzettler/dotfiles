{ pkgs, ... }:

{
  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "none"; # change to "zap" after verifying all packages
      autoUpdate = true;
      upgrade = true;
    };

    caskArgs.no_quarantine = true;

    taps = [
      "homebrew/cask-fonts"
      "heroku/brew"
      "txtx/taps"
      "xcodesorg/made"
    ];

    brews = [
      # core
      "bash"
      "fish"
      "starship"
      "tmux"

      # git & vcs
      "gh"
      "git-delta"

      # search & navigation
      "ripgrep"
      "zoxide"
      "yazi"

      # dev tools
      "pandoc"
      "jq"
      "tree"
      "wget"
      "curl"
      "gnupg"
      "zizmor"
      "cargo-audit"

      # languages
      "fnm"
      "rbenv"
      "ruby-build"

      # databases
      "postgresql@14"
      "redis"

      # lsp servers
      "taplo"
      "yaml-language-server"

      # build deps
      "openssl@3"
      "readline"
      "enchant"
      "pkg-config"

      # tapped (bootstrap.sh trusts these formulae before brew bundle runs)
      "heroku/brew/heroku"
      "txtx/taps/surfpool"
      "xcodesorg/made/xcodes"
    ];

    casks = [
      # fonts
      "font-hack-nerd-font"
      "font-symbols-only-nerd-font"

      # terminals & editors
      "ghostty"

      # dev tools
      "docker"
      "android-studio"
      "bruno"
      "tableplus"
      "reactotron"

      # productivity
      "alfred"
      "rectangle"
      "only-switch"
      "obsidian"

      # communication
      "slack"
      "telegram"
      "zoom"

      # media
      "spotify"
      "iina"
      "beardedspice"

      # security & network
      "1password"
      "tailscale"
      "tunnelbear"

      # crypto
      "ledger-live"

      # peripherals
      "logi-options+"

      # gaming/streaming
      "shadow"
    ];
  };
}
