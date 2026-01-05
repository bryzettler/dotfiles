{ config, pkgs, ... }:

{
  # Node.js related packages managed via fnm in fish.nix
  # This module just ensures npm global packages path is set

  home.sessionVariables = {
    NPM_CONFIG_PREFIX = "$HOME/.npm-global";
  };

  home.sessionPath = [
    "$HOME/.npm-global/bin"
  ];
}
