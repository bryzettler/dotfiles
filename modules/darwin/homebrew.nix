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
      "jq"
      "tree"
      "wget"
      "curl"
      "gnupg"

      # languages
      "fnm"
      "rbenv"
      "ruby-build"

      # databases
      "postgresql@14"
      "redis"

      # build deps
      "openssl@3"
      "readline"
    ];

    casks = [
      # fonts
      "font-hack-nerd-font"
      "font-symbols-only-nerd-font"

      # terminals & editors
      "ghostty"
      "cursor"
      "visual-studio-code"

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
