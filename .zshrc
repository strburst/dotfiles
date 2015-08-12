# OH-MY-ZSH CONFIGURATION
export ZSH=$HOME/.oh-my-zsh

# ZSH_THEME="jonathan"
ZSH_THEME="nanotech"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# OTHER STUFF
set -o vi   # vi keyboard shortcuts

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
PATH+=":/usr/games:/usr/local/games:/usr/lib/jvm/java-8-oracle/bin"
PATH+=":/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin"

export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"

export CLASSPATH=".:/usr/local/bin/junit-4.12.jar:/usr/local/bin/hamcrest-core"
CLASSPATH+="-1.3.jar:/usr/local/bin/checkstyle-6.2.jar"

export EDITOR='vim'

# Solarized ls colors; see https://github.com/seebi/dircolors-solarized
eval `dircolors ~/.dircolors.ansi-universal`

# Search repos for a command if not installed
source /usr/share/doc/pkgfile/command-not-found.zsh
