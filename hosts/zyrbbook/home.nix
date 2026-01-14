{ config, pkgs, lib, username, ... }:

{
  imports = [
    ../../modules/home/shell/fish.nix
    ../../modules/home/shell/starship.nix
    ../../modules/home/git.nix
    ../../modules/home/terminal/tmux.nix
    ../../modules/home/terminal/ghostty.nix
    ../../modules/home/editors/emacs.nix
    ../../modules/home/tools/bat.nix
    ../../modules/home/tools/dev.nix
    ../../modules/home/packages/node.nix
    ../../modules/home/fonts.nix
    ../../modules/home/apps/obsidian.nix
  ];

  home = {
    username = username;
    homeDirectory = "/Users/${username}";
    stateVersion = "24.11";

    # session variables
    sessionVariables = {
      EDITOR = "emacs -nw";
      VISUAL = "emacs -nw";
      PAGER = "bat";
    };
  };

  # let home-manager manage itself
  programs.home-manager.enable = true;
}
