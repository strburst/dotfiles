export EDITOR='vim'
export NODE_PATH='/usr/lib/node_modules'

typeset -U PATH path  # Discard any duplicate entries
export PATH="$PATH:$HOME/.local/bin"
