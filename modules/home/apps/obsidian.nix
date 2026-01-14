{ config, ... }:

let
  # Change this each year when creating a new vault
  vaultYear = "2026";
  obsidianConfigDir = "${config.home.homeDirectory}/Documents/Personal/Vaults/${vaultYear}/.obsidian";
in
{
  home.file = {
    "${obsidianConfigDir}/appearance.json".source = ../../../config/obsidian/appearance.json;
    "${obsidianConfigDir}/community-plugins.json".source = ../../../config/obsidian/community-plugins.json;
    "${obsidianConfigDir}/core-plugins.json".source = ../../../config/obsidian/core-plugins.json;
    "${obsidianConfigDir}/hotkeys.json".source = ../../../config/obsidian/hotkeys.json;
    "${obsidianConfigDir}/daily-notes.json".source = ../../../config/obsidian/daily-notes.json;
    "${obsidianConfigDir}/templates.json".source = ../../../config/obsidian/templates.json;
    "${obsidianConfigDir}/snippets".source = ../../../config/obsidian/snippets;
  };
}
