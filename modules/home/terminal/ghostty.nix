{ config, pkgs, lib, ... }:

{
  # ghostty config via symlink
  home.file.".config/ghostty/config".source = ../../../config/ghostty/config;
}
