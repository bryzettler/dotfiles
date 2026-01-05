{ config, pkgs, ... }:

{
  programs.bat = {
    enable = true;
    config = {
      theme = "Catppuccin Mocha";
      style = "numbers,changes,header";
    };
  };
}
