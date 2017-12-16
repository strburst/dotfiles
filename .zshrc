## ZSH OPTIONS {{{

autoload -U colors && colors      # Built-in colors
autoload -U compinit && compinit  # Built-in completions
autoload -U promptinit && promptinit && prompt walters

# Save 1000 lines of history
export HISTSIZE=1000
export SAVEHIST=1000

export HISTFILE=~/.zsh_history

export KEYTIMEOUT=1  # Only wait 0.1 seconds when switching modes

# Headers in this section are based on categories in zshoptions(1)

## Changing directories {{{
setopt auto_pushd         # cd pushes onto directory stack
setopt pushd_ignore_dups  # Don't add duplicates to the directory stack
setopt pushd_minus        # Invert pushd's understanding of +/-

## }}} Completion {{{
setopt always_to_end     # Move to end when completion finishes
setopt auto_menu         # Show zsh's trademark completion menu on tab
setopt complete_in_word  # Don't move the cursor when completing

## }}} History {{{
setopt append_history          # Append to (not overwrite) history when exiting
setopt extended_history        # Also save timestamp and execution time
setopt hist_expire_dups_first  # Get rid of duplicate history entries first
setopt hist_ignore_all_dups    # Remove older instances of a new command
setopt hist_ignore_space       # Don't record commands starting with space
setopt hist_no_store           # Don't store the history command in history
setopt hist_verify             # Show result first when expanding history
setopt inc_append_history      # Write to history continuously

## }}} Input/output {{{
setopt interactivecomments  # Allow comments in interactive shells

## }}} Job control {{{
setopt long_list_jobs  # Always list jobs in long format

## }}} Prompting {{{
setopt prompt_subst  # Fill out variables in the prompt

unsetopt menu_complete  # Don't autoselect the first menu entry

## }}} Flow control {{{
unsetopt flow_control  # Don't output flow control characters

## }}}

## }}} KEYBINDINGS {{{

bindkey -v  # vi keybindings

function bindall() {
    bindkey -M emacs $1 $2
    bindkey -M vicmd $1 $2
    bindkey -M viins $1 $2
}

# Make home/end/delete keys work like they're supposed to
bindall "${terminfo[khome]}" beginning-of-line
bindall "${terminfo[kend]}" end-of-line
bindall "${terminfo[kdch1]}" delete-char

# Bind <C-left> and <C-right>
bindall "^[Od" vi-backward-word
bindall "^[Oc" vi-forward-word

bindall '^q' push-line         # Put the buffer into the buffer stack
bindall '^f' insert-last-word  # Insert last argument to the previous command

# Scroll up/down history
bindkey '^p' up-history
bindkey '^n' down-history
bindkey '^r' history-incremental-search-backward

# ^Z to foreground the last suspended job
foreground-current-job() { fg; }
zle -N foreground-current-job
bindall '^z' foreground-current-job

## }}} OTHER STUFF {{{

zstyle ':completion:*' menu select  # Enable select-box on current completion
# Completion menu uses ls colors when selecting files
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Search repos for a command if not installed (both Arch and Ubuntu)
if [ -f "/usr/share/doc/pkgfile/command-not-found.zsh" ]; then
    source /usr/share/doc/pkgfile/command-not-found.zsh
elif [ -x /usr/lib/command-not-found ]; then
    function command_not_found_handler {
        /usr/lib/command-not-found -- "$1"
        return $?
    }
fi

## }}} ALIASES {{{

alias duck='links www.duckduckgo.com'
alias feh='feh --scale-down'
alias grep='grep --color=auto'
alias hist='history 0 | less'
alias ll='ls -Al'
alias ls='ls --color=auto --human-readable'
alias ppp='ping -c 3 8.8.8.8'
alias sbcl='rlwrap sbcl'
alias sl='ls'
alias sqlite3='sqlite3 -column -header -nullvalue "<NULL>"'

## }}}

# File for machine-specific settings
if [ -f "$HOME/.zshrc.local" ]; then
    source "$HOME/.zshrc.local"
fi
