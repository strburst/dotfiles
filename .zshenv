export CLASSPATH=".:/usr/local/bin/junit-4.12.jar:/usr/local/bin/hamcrest-core"
CLASSPATH+="-1.3.jar:/usr/local/bin/checkstyle-6.9.jar"

export EDITOR='vim'

export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"

export NODE_PATH="/usr/lib/node_modules"

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
PATH+=":/usr/games:/usr/local/games:/usr/lib/jvm/java-8-oracle/bin"
PATH+=":/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin"
PATH+=":/usr/bin/core_perl:$HOME/.gem/ruby/2.2.0/bin:$HOME/.gem/ruby/2.3.0/bin"

# Also include ~/bin in path if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi
