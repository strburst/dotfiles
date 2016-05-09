# OH-MY-ZSH CONFIGURATION
export ZSH=$HOME/.oh-my-zsh

# Other cool themes: agnoster, blinks, jonathan, nanotech
ZSH_THEME="bira"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# Don't share history between terminals
unsetopt SHARE_HISTORY
# Get rid of duplicate history entries first
setopt HIST_EXPIRE_DUPS_FIRST

# Save only the past 1000 or so lines of history
export SAVEHIST=1000

# OTHER STUFF
set -o vi   # vi keyboard shortcuts

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
PATH+=":/usr/games:/usr/local/games:/usr/lib/jvm/java-8-oracle/bin"
PATH+=":/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin"
PATH+=":/usr/bin/core_perl:$HOME/.gem/ruby/2.2.0/bin:$HOME/.gem/ruby/2.3.0/bin"

export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"

export CLASSPATH=".:/usr/local/bin/junit-4.12.jar:/usr/local/bin/hamcrest-core"
CLASSPATH+="-1.3.jar:/usr/local/bin/checkstyle-6.9.jar"

export NODE_PATH="/usr/lib/node_modules"

export EDITOR='vim'

# Solarized ls colors; see https://github.com/seebi/dircolors-solarized
eval `dircolors ~/.dircolors.ansi-universal`

# Search repos for a command if not installed
source /usr/share/doc/pkgfile/command-not-found.zsh

alias hist='history | less'
alias sl='ls'
alias sqlite3='sqlite3 -column -header -nullvalue "<NULL>"'
