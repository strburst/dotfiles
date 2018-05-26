export EDITOR='vim'
export MANPATH="$MANPATH:$HOME/.node/share/man"
export NODE_PATH="$HOME/.local/share/node/lib/node_modules:$NODE_PATH"

typeset -U PATH path  # Discard any duplicate entries
export PATH="$PATH:$HOME/.local/bin:$HOME/.local/share/node/bin"
