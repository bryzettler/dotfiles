fpath=($fpath $HOME/.zsh/func)
typeset -U fpath

if [[ -z $LANG ]]; then export LANG=en_US.UTF-8; fi
if [[ -z $LC_COLLATE ]]; then export LC_COLLATE=en_US.UTF-8; fi
if [[ -z $LC_CTYPE ]]; then export LC_CTYPE=en_US.UTF-8; fi
if [[ -z $LC_MESSAGES ]]; then export LC_MESSAGES=en_US.UTF-8; fi
if [[ -z $LC_MONETARY ]]; then export LC_MONETARY=en_US.UTF-8; fi
if [[ -z $LC_NUMERIC ]]; then export LC_NUMERIC=en_US.UTF-8; fi
if [[ -z $LC_TIME ]]; then export LC_TIME=en_US.UTF-8; fi
