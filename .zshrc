## ZPLUG CONFIGURATION {{{

if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

source ~/.zplug/init.zsh

zplug "Peeja/ctrl-zsh"               # ^Z switches between current job and shell
zplug "themes/bira", from:oh-my-zsh  # Fancy shell prompt

autoload colors && colors

# Install plugins if there are plugins that have not been installed
zplug check --verbose || zplug install

zplug load

## }}} ZSH_OPTIONS {{{

# Headers are based on zshoptions(1)

## Changind directories {{{
setopt auto_pushd         # cd pushes onto directory stack
setopt pushd_ignore_dups  # Don't add duplicates to the directory stack
setopt pushd_minus        # Invert pushd's understanding of +/-

## }}} Completion {{{
setopt always_to_end     # Move to end when completion finishes
setopt auto_menu         # Show zsh's trademark completion menu on tab
setopt complete_in_word  # Don't move the cursor when completing

## }}} Historu {{{
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

## }}} OTHER STUFF {{{

# Save 1000 lines of history
export HISTSIZE=1000
export SAVEHIST=1000

export HISTFILE=~/.zsh_history

bindkey -v           # vi keybindings
export KEYTIMEOUT=1  # Only wait 0.1 seconds when switching modes

bindkey '^p' up-history
bindkey '^n' down-history
bindkey '^r' history-incremental-search-backward

# Make home/end keys work like they're supposed to
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

# Solarized ls colors; see https://github.com/seebi/dircolors-solarized
eval `dircolors ~/.dircolors.ansi-universal`

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

alias hist='history | less'
alias ll='ls -l'
alias ls='ls --color=auto'
alias packeru='packer -Syu --auronly --noconfirm'
alias ppp='ping 8.8.8.8'
alias sl='ls'
alias sqlite3='sqlite3 -column -header -nullvalue "<NULL>"'

## }}}
