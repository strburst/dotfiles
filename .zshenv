export EDITOR='vim'
export JAVA_HOME="/usr/lib/jvm/java-8-openjdk"
export NODE_PATH="/usr/lib/node_modules"

# Also include ~/bin in path if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

# File for machine-specific settings
if [ -f "$HOME/.zshenv.local" ]; then
    source "$HOME/.zshenv.local"
fi
