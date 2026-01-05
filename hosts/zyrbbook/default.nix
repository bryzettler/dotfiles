{ pkgs, username, ... }:

{
  imports = [
    ../../modules/darwin/homebrew.nix
    ../../modules/darwin/system.nix
  ];

  # nix settings
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      warn-dirty = false;
    };
    # garbage collection
    gc = {
      automatic = true;
      interval = { Weekday = 0; Hour = 2; Minute = 0; };
      options = "--delete-older-than 30d";
    };
  };

  # allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # fish shell
  programs.fish.enable = true;

  # networking
  networking = {
    hostName = "zyrbbook";
    computerName = "zyrbbook";

    # Steven Black's Unified hosts + fakenews + porn blocklist
    # Cloned to ~/.hosts-blocklist by bootstrap.sh
    hostFiles = [
      (/. + "/Users/${username}/.hosts-blocklist/alternates/fakenews-porn/hosts")
    ];
  };

  # user
  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
    shell = pkgs.fish;
  };

  # required for nix-darwin
  system.stateVersion = 5;

  # set primary user for nix-darwin
  system.primaryUser = username;
}
