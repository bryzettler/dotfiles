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
  ];

  # link emacs config
  home.file.".emacs.d/init.el".source = ../../../config/emacs/init.el;
  home.file.".emacs.d/early-init.el".text = ''
    ;; -*- lexical-binding: t; -*-

    ;; Reduce garbage collection during startup
    (setq gc-cons-threshold most-positive-fixnum)

    ;; Disable package.el in favor of straight.el
    (setq package-enable-at-startup nil)

    ;; Prevent unwanted runtime compilation
    (setq native-comp-deferred-compilation nil)
    (setq native-comp-jit-compilation nil)

    ;; GUI frame defaults (applied before frame creation)
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist)
    (push '(internal-border-width . 12) default-frame-alist)
    (push '(alpha-background . 90) default-frame-alist)
    (push '(font . "Hack Nerd Font-12") default-frame-alist)

    ;; Faster startup
    (setq frame-inhibit-implied-resize t)
  '';
}
