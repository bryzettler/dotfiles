{ config, ... }:

let
  dots = "${config.home.homeDirectory}/.dotfiles";
in
{
  # Claude Code global config via out-of-store symlinks — repo edits apply live.
  # settings.json is intentionally NOT linked: Claude Code and herdr rewrite it
  # (plugin toggles, /config, integration updates) and would clobber a symlink.
  # Sync it into the repo with the `claude-sync` fish function.
  home.file.".claude/CLAUDE.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/CLAUDE.md";
  home.file.".claude/RTK.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/RTK.md";
  home.file.".claude/agents/implementer.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/implementer.md";
  home.file.".claude/agents/implementer-deep.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/implementer-deep.md";
  home.file.".claude/agents/fixer.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/fixer.md";
  home.file.".claude/skills/review".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/skills/review";
  home.file.".claude/skills/implement-tickets".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/skills/implement-tickets";
  home.file.".claude/hooks/block-destructive-git.sh".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/hooks/block-destructive-git.sh";
  home.file.".claude/hooks/check-comments.py".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/hooks/check-comments.py";
}
