{ pkgs, ... }:

let
  # VS Code Dark+ colors
  colors = {
    bg = "#1e1e1e";
    fg = "#d4d4d4";
    fg_dim = "#666666";
    accent = "#569cd6";
    blue = "#3b8eea";
    green = "#4ec9b0";
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
      # True color (24-bit) support for Emacs -nw and other apps
      set -ag terminal-overrides ",xterm-256color:Tc"
      set -ag terminal-overrides ",tmux-256color:Tc"
      set -ag terminal-overrides ",xterm-ghostty:Tc"
      set -g default-terminal "tmux-256color"

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
      # Emacs-style C-x bindings (smart passthrough)
      # ============================================

      # C-x: if in Emacs, send C-x; otherwise enter cx-keys table
      # Uses tmux format comparison (-F) with case-insensitive match (/i) for macOS Emacs-arm64-11
      bind -n C-x if-shell -F "#{m/i:*emacs*,#{pane_current_command}}" "send-keys C-x" "switch-client -T cx-keys"

      # Key table for C-x prefix (when NOT in Emacs)
      bind -T cx-keys 2 split-window -h -c "#{pane_current_path}"  # side-by-side (matches your Emacs)
      bind -T cx-keys 3 split-window -v -c "#{pane_current_path}"  # stacked (matches your Emacs)
      bind -T cx-keys 0 kill-pane                                  # C-x 0 = close pane
      bind -T cx-keys 1 resize-pane -Z                             # C-x 1 = zoom (maximize) pane
      bind -T cx-keys k kill-pane                                  # C-x k = close (Emacs kill-buffer style)

      # C-x o = visual pane selection with numbers + letter keys
      # display-panes runs in background (-b) showing numbers
      # simultaneously switch to pane-select table for letter input
      bind -T cx-keys o run-shell -b "tmux display-panes -d 2000" \; switch-client -T pane-select

      # Pane select key table - home row letters map to pane indices
      # Mapping: a=1, s=2, d=3, f=4, g=5, h=6, j=7, k=8, l=9
      bind -T pane-select a select-pane -t :.1
      bind -T pane-select s select-pane -t :.2
      bind -T pane-select d select-pane -t :.3
      bind -T pane-select f select-pane -t :.4
      bind -T pane-select g select-pane -t :.5
      bind -T pane-select h select-pane -t :.6
      bind -T pane-select j select-pane -t :.7
      bind -T pane-select k select-pane -t :.8
      bind -T pane-select l select-pane -t :.9
      # Also allow digit keys for flexibility
      bind -T pane-select 1 select-pane -t :.1
      bind -T pane-select 2 select-pane -t :.2
      bind -T pane-select 3 select-pane -t :.3
      bind -T pane-select 4 select-pane -t :.4
      bind -T pane-select 5 select-pane -t :.5
      bind -T pane-select 6 select-pane -t :.6
      bind -T pane-select 7 select-pane -t :.7
      bind -T pane-select 8 select-pane -t :.8
      bind -T pane-select 9 select-pane -t :.9

      # Escape from key tables
      bind -T cx-keys Escape switch-client -T root
      bind -T pane-select Escape switch-client -T root

      # Pass through C-x C-x to shell (readline)
      bind -T cx-keys C-x send-keys C-x

      # Style pane number display
      set -g display-panes-colour "${colors.fg_dim}"
      set -g display-panes-active-colour "${colors.blue}"
      set -g display-panes-time 2000

      # ============================================
      # Status Bar - VS Code Dark+
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
