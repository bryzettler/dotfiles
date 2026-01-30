{ pkgs, ... }:

let
  catppuccin = import ../theme/catppuccin.nix;
in
{
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      # tmux auto-start: attach to "main" or create it (skip inside Emacs vterm)
      if type -q tmux; and not set -q TMUX; and not set -q INSIDE_EMACS
        tmux attach -t main; or tmux new -s main
      end

      # emacs key bindings (default)
      fish_default_key_bindings

      # homebrew
      eval "$(/opt/homebrew/bin/brew shellenv)"

      # fnm (node version manager)
      fnm env --use-on-cd --shell fish | source

      # rbenv (ruby version manager)
      status --is-interactive; and rbenv init - fish | source

      # cargo
      fish_add_path $HOME/.cargo/bin

      # solana (if installed)
      fish_add_path $HOME/.local/share/solana/install/active_release/bin
    '';

    shellAliases = {
      # editor
      e = "emacs -nw";
      fishrc = "emacs -nw ~/.config/fish/config.fish";
      reload = "source ~/.config/fish/config.fish";

      # file listing (eza)
      ls = "eza --icons";
      ll = "eza -l -g --git --icons";
      la = "eza -la -g --git --icons";
      lt = "eza -1 --git --tree --git-ignore --icons";

      # navigation
      cdw = "mkdir -p ~/Documents/Work && cd ~/Documents/Work";
      cdp = "mkdir -p ~/Documents/Personal && cd ~/Documents/Personal";

      # package managers
      yarnclean = "rm -rf node_modules/ && yarn install";
      npmclean = "rm -rf node_modules/ && npm install";

      # git
      g = "git";

      # misc
      top = "htop";

      # rebuild nix
      rebuild = "darwin-rebuild switch --flake ~/.dotfiles";
    };

    functions = {
      fish_greeting = "";
      # Rename tmux window on directory change only (reduces flashing)
      cd = ''
        builtin cd $argv
        if set -q TMUX
          tmux rename-window (basename $PWD)
        end
      '';
    };
  };

  # zoxide (better cd)
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  # fzf
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    defaultOptions = [
      "--height=40%"
      "--layout=reverse"
      "--border"
      "--color=bg:${catppuccin.base},bg+:${catppuccin.surface0},fg:${catppuccin.text},fg+:${catppuccin.text},hl:${catppuccin.red},hl+:${catppuccin.red},pointer:${catppuccin.rosewater},prompt:${catppuccin.mauve},info:${catppuccin.blue}"
    ];
  };

  # eza (better ls)
  programs.eza = {
    enable = true;
    enableFishIntegration = true;
    git = true;
    icons = "auto";
  };

  # direnv
  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
    nix-direnv.enable = true;
  };
}
