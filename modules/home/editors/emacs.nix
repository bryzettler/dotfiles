{ config, pkgs, lib, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

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

    ;; Disable UI elements early
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist)

    ;; Faster startup
    (setq frame-inhibit-implied-resize t)
  '';
}
