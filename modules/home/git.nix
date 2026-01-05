{ config, pkgs, lib, ... }:

{
  programs.git = {
    enable = true;

    settings = {
      user = {
        name = "Bryan Zettler";
        email = "bryanzettler@gmail.com";
      };

      github.user = "bzettler";

      init.defaultBranch = "main";

      core = {
        editor = "emacs -nw";
        excludesfile = "~/.gitignore_global";
        quotepath = false;
        autocrlf = "input";
        safecrlf = "warn";
      };

      color = {
        ui = "auto";
        branch = {
          current = "yellow reverse";
          local = "yellow";
          remote = "green";
        };
        diff = {
          meta = "yellow bold";
          frag = "magenta bold";
          old = "red bold";
          new = "green bold";
        };
        status = {
          added = "yellow";
          changed = "green";
          untracked = "cyan";
        };
      };

      merge = {
        conflictStyle = "diff3";
        stat = true;
      };

      push = {
        autoSetupRemote = true;
        default = "current";
        followTags = true;
      };

      pull = {
        rebase = true;
      };

      rebase = {
        autoStash = true;
      };

      diff = {
        algorithm = "histogram";
        colorMoved = "default";
      };

      rerere.enabled = true;

      alias = {
        co = "checkout";
        d = "diff --color-words";
        cam = "commit -a -m";
        up = "pull --rebase --autostash";
        upm = "!git fetch upstream && git merge upstream/master";
        st = "status";
        a = "add";
        hist = "log --pretty=format:\"%Cgreen%h %Creset%cd %Cblue[%cn] %Creset%s%C(yellow)%d%C(reset)\" --graph --date=relative --decorate --all";
        llog = "log --graph --name-status --pretty=format:\"%C(red)%h %C(reset)(%cd) %C(green)%an %Creset%s %C(yellow)%d%Creset\" --date=relative";
        ps = "!git push origin $(git rev-parse --abbrev-ref HEAD)";
        pl = "!git pull origin $(git rev-parse --abbrev-ref HEAD)";
        open = "!gh browse";
        nuke = "!git restore . && git clean -fd";
      };
    };

    ignores = [
      ".DS_Store"
      "._*"
      ".idea/"
      ".vscode/"
      "*.swp"
      "*.swo"
      "*~"
      ".env"
      ".env.local"
      ".env*.local"
      "node_modules/"
      "__pycache__/"
      "*.pyc"
      ".venv/"
      "venv/"
      ".direnv/"
      "result"
    ];
  };

  # delta (git pager)
  programs.git-delta = {
    enable = true;
    options = {
      navigate = true;
      side-by-side = false;
      line-numbers = true;
    };
  };
}
