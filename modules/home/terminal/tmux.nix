{ config, pkgs, lib, ... }:

let
  # catppuccin mocha colors
  colors = {
    bg = "#1e1e2e";
    fg = "#cdd6f4";
    fg_dim = "#6c7086";
    accent = "#cba6f7";    # mauve
    blue = "#89b4fa";
    green = "#a6e3a1";
  };
in
{
  programs.tmux = {
    enable = true;
    shell = "${pkgs.fish}/bin/fish";
    terminal = "xterm-256color";
    baseIndex = 1;
    escapeTime = 0;
    mouse = true;
    historyLimit = 1000000;

    plugins = with pkgs.tmuxPlugins; [
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '15'
        '';
      }
    ];

    extraConfig = ''
      # true color support
      set -ga terminal-overrides ",xterm-256color:Tc"

      # pane base index
      setw -g pane-base-index 1

      # renumber windows
      set -g renumber-windows on

      # auto-rename window to current directory
      set -g automatic-rename on
      set -g automatic-rename-format '#{b:pane_current_path}'

      # ============================================
      # Keybindings (simple, iTerm2-like)
      # ============================================

      # Shift+Left/Right to switch windows
      bind -n S-Left previous-window
      bind -n S-Right next-window

      # Alt+T - new window
      bind -n M-t new-window -c "#{pane_current_path}"

      # Alt+D/Shift+D - splits (like iTerm2)
      bind -n M-d split-window -h -c "#{pane_current_path}"
      bind -n M-D split-window -v -c "#{pane_current_path}"

      # Alt+Arrow - navigate panes
      bind -n M-Left select-pane -L
      bind -n M-Right select-pane -R
      bind -n M-Up select-pane -U
      bind -n M-Down select-pane -D

      # Alt+W - close pane
      bind -n M-w kill-pane

      # Alt+, - rename window
      bind -n M-, command-prompt -I "#W" "rename-window '%%'"

      # Ctrl+Tab - switch windows
      bind -n C-Tab next-window
      bind -n C-S-Tab previous-window

      # Reload config
      bind r source-file ~/.config/tmux/tmux.conf \; display "Config reloaded!"

      # ============================================
      # Status Bar - Catppuccin Mocha
      # ============================================
      set -g status on
      set -g status-position bottom
      set -g status-style "bg=${colors.bg},fg=${colors.fg}"
      set -g status-left "#[fg=${colors.bg},bg=${colors.blue},bold] #S #[fg=${colors.blue},bg=${colors.bg}] "
      set -g status-left-length 30
      set -g status-right "#[fg=${colors.fg_dim}]%H:%M #[fg=${colors.blue}]│ #[fg=${colors.accent}]%b %d "
      set -g status-right-length 50

      # Window status
      setw -g window-status-format "#[fg=${colors.fg_dim}] #I:#W "
      setw -g window-status-current-format "#[fg=${colors.bg},bg=${colors.accent},bold] #I:#W #[fg=${colors.accent},bg=${colors.bg}]"

      # Pane borders
      set -g pane-border-style "fg=${colors.fg_dim}"
      set -g pane-active-border-style "fg=${colors.blue}"

      # Message style
      set -g message-style "bg=${colors.blue},fg=${colors.bg},bold"

      # No bells
      set -g visual-activity off
      set -g visual-bell off
      set -g visual-silence off
      setw -g monitor-activity off
      set -g bell-action none
    '';
  };
}
