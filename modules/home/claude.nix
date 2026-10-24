{ config, ... }:

let
  dots = "${config.home.homeDirectory}/.dotfiles";
in
{
  # Claude Code global config via out-of-store symlinks — repo edits apply live.
  # settings.json is linked too: Claude Code rewrites (plugin toggles, /config)
  # land in the repo, so review them in `git diff` before committing.
  home.file.".claude/settings.json".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/settings.json";
  home.file.".claude/CLAUDE.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/CLAUDE.md";
  home.file.".claude/RTK.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/RTK.md";
  home.file.".claude/CODING_STANDARDS.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/CODING_STANDARDS.md";
  home.file.".claude/MODEL_ROUTING.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/MODEL_ROUTING.md";
  home.file.".claude/statusline.mjs".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/statusline.mjs";
  home.file.".claude/AGENT_SKILLS_SETUP.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/AGENT_SKILLS_SETUP.md";
  home.file.".claude/agents/implementer.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/implementer.md";
  home.file.".claude/agents/implementer-deep.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/implementer-deep.md";
  home.file.".claude/agents/fixer.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/fixer.md";
  home.file.".claude/agents/tracer.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/tracer.md";
  home.file.".claude/agents/verifier.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/verifier.md";
  home.file.".claude/agents/scout.md".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/agents/scout.md";
  home.file.".claude/skills/review".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/skills/review";
  home.file.".claude/skills/implement-tickets".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/skills/implement-tickets";
  home.file.".claude/skills/address-review".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/skills/address-review";
  home.file.".claude/skills/recipe-note".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/skills/recipe-note";
  home.file.".claude/hooks/block-destructive-git.sh".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/hooks/block-destructive-git.sh";
  home.file.".claude/hooks/format-prettier.sh".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/hooks/format-prettier.sh";
  home.file.".claude/hooks/check-comments.py".source = config.lib.file.mkOutOfStoreSymlink "${dots}/config/claude/hooks/check-comments.py";
}
