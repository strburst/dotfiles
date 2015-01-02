export ZSH=$HOME/.oh-my-zsh

# ZSH_THEME="agnoster"
# ZSH_THEME="jonathan"
ZSH_THEME="nanotech"
# ZSH_THEME="ys"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
PATH+=":/usr/games:/usr/local/games:/usr/lib/jvm/java-8-oracle/bin"
PATH+=":/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin"

export EDITOR='vim'

# Solarized ls colors; see https://github.com/seebi/dircolors-solarized
eval `dircolors ~/dircolors.ansi-universal`

alias ack="ack-grep"

# Set the TERM variable before launching tmux
alias tmux="TERM=screen-256color tmux"

# Start a new tmux session with every terminal
if [[ ! $TERM =~ screen ]]; then
    export TERM=screen-256color
    tmux
fi
