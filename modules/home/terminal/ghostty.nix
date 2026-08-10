{ config, ... }:

{
  # ghostty config via out-of-store symlink — repo edits apply on ghostty restart
  home.file.".config/ghostty/config".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/config/ghostty/config";
}
