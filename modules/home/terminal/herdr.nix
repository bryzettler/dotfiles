{ config, ... }:

{
  # herdr config via out-of-store symlink — apply with `herdr server reload-config`
  home.file.".config/herdr/config.toml".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/config/herdr/config.toml";
}
