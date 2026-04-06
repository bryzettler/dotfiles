{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
  };

  home.packages = with pkgs; [
    enchant2
    hunspell
    hunspellDicts.en_US

    # LSP servers
    nodePackages.typescript-language-server
    vscode-langservers-extracted
    yaml-language-server
    bash-language-server
    nil
    sqls
    taplo
    dockerfile-language-server-nodejs
  ];

  # link emacs config
  home.file.".emacs.d/init.el".source = ../../../config/emacs/init.el;
  home.file.".emacs.d/early-init.el".source = ../../../config/emacs/early-init.el;
}
