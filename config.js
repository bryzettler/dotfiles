module.exports = {
  brew: [
    // http://conqueringthecommandline.com/book/ack_ag
    "ack",
    "ag",
    "asdf",
    // https://github.com/wting/autojump
    "autojump",
    // alternative to `cat`: https://github.com/sharkdp/bat
    "bat",
    // Install GNU core utilities (those that come with macOS are outdated)
    // Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
    "coreutils",
    "automake",
    "autoconf",
    "openssl",
    "libyaml",
    "libxslt",
    "libtool",
    "unixodbc",
    "dos2unix",
    // Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed
    "findutils",
    // 'fortune',
    "fzf",
    "readline", // ensure gawk gets good readline
    "gawk",
    // http://www.lcdf.org/gifsicle/ (because I'm a gif junky)
    "gifsicle",
    "gnupg",
    "svn",
    // Install GNU `sed`, overwriting the built-in `sed`
    // so we can do "sed -i 's/foo/bar/' file" instead of "sed -i '' 's/foo/bar/' file"
    "gnu-sed --with-default-names",
    // upgrade grep so we can get things like inverted match (-v)
    "grep --with-default-names",
    // better, more recent grep
    "homebrew/dupes/grep",
    // https://github.com/jkbrzt/httpie
    "httpie",
    // jq is a sort of JSON grep
    "jq",
    // Mac App Store CLI: https://github.com/mas-cli/mas
    "mas",
    // Install some other useful utilities like `sponge`
    "moreutils",
    "nmap",
    "openconnect",
    "reattach-to-user-namespace",
    // better/more recent version of screen
    "homebrew/dupes/screen",
    "tmux",
    "todo-txt",
    "tree",
    "ttyrec",
    "watch",
    "redis",
    "postgres",
    "postgis",
    "gcc",
    // Install wget with IRI support
    "wget --enable-iri",
  ],
  cask: [
    "java8",
    // better, more recent emacs
    "emacs --no-quarantine",
    //'adium',
    //'amazon-cloud-drive',
    //'atom',
    // 'box-sync',
    //'comicbooklover',
    //'diffmerge',
    "docker", // docker for mac
    "dropbox",
    "graphql-playground",
    //'evernote',
    //"flux",
    //"gpg-suite",
    //'ireadfast',
    //"iterm2",
    //'hyper',
    //"little-snitch",
    //"macbreakz",
    //"micro-snitch",
    //"signal",
    //'macvim',
    "spectacle",
    //'sketchup',
    //'slack',
    "ferdi",
    "alfred",
    "spotify",
    "the-unarchiver",
    //'torbrowser',
    //'transmission',
    //"visual-studio-code",
    //'vlc',
    "xquartz",
    "brave-browser",
    "transmission",
    "1password",
    "tableplus",
    "numi",
    "beardedspice",
    "ripgrep",
  ],
  gem: ["pry", "pry-doc", "colorls"],
  npm: [
    "antic",
    "buzzphrase",
    "eslint",
    "eslint-config-airbnb",
    "eslint-plugin-flowtype",
    "eslint-plugin-import",
    "eslint-plugin-jsx-ally",
    "eslint-plugin-react",
    "eslint_d",
    "typescript",
    "typescript-language-server",
    "tslint",
    "instant-markdown-d",
    // 'generator-dockerize',
    // 'gulp',
    "npm-check-updates",
    "prettyjson",
    "trash",
    // "vtop",
    "htop",
    "yarn",
    "tern",
    "eslint_d",
    "prettier",
    // ,'yo'
  ],
  mas: [
    //com.apple.dt.Xcode (10.2.1)
    //"497799835",
    //com.if.Amphetamine (4.1.6)
    //'937984704',
    //net.shinyfrog.bear (1.6.15)
    //'1091189122',
    //com.monosnap.monosnap (3.5.8)
    //'540348655',
    //com.app77.pwsafemac (4.17)
    //'520993579',
  ],
};
