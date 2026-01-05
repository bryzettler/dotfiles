{ pkgs, ... }:

let
  catppuccin = import ../theme/catppuccin.nix;
  colors = {
    bg = catppuccin.base;
    fg = catppuccin.text;
    fg_dim = catppuccin.overlay0;
    accent = catppuccin.mauve;
    blue = catppuccin.blue;
    green = catppuccin.green;
  };
in
{
  programs.tmux = {
    enable = true;
    shell = "${pkgs.fish}/bin/fish";
    terminal = "tmux-256color";  # Better for Emacs -nw color support
    baseIndex = 1;
    escapeTime = 0;
    mouse = true;
    historyLimit = 50000;
    focusEvents = true;
    prefix = "C-\\";  # Ergonomic, avoids Ctrl+b repeat lag

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
      # True color support for Emacs -nw and other apps
      set -ag terminal-overrides ",xterm-256color:RGB"

      # Allow C-\ to pass through (press twice to send literal)
      bind 'C-\' send-prefix

      # pane base index
      setw -g pane-base-index 1

      # renumber windows
      set -g renumber-windows on

      # Window renaming handled by shell chpwd hook (reduces flashing)
      set -g automatic-rename off
      set -g allow-rename on

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
