# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# Warning - NOT POSIX-compliant

# If not running interactively, don't do anything
[[ -z "$PS1" ]] && return

# don't put duplicate lines in the history. See bash(1) for more options
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

if [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [[ -f /etc/bash_completion && -r /etc/bash_completion ]] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# If a personal bin exists, use it
if [[ -d "$HOME/bin" ]] ; then
    export PATH="$HOME/bin:$PATH"
fi
# If a cabal bin exists, use it
if [[ -d "$HOME/.cabal/bin" ]] ; then
    export PATH="$PATH:$HOME/.cabal/bin"
fi

# Use vi mode, with Ctrl-L used to clear screen
set -o vi
bind -m vi-insert "\C-l":clear-screen

# I like vim, mmmkay?
export EDITOR='vim'
export VISUAL='vim'

# less is a handy pager
export PAGER=less
 
# Handy color code definitions:
export BLACK="\033[00;30m"
export RED="\033[00;31m"
export GREEN="\033[00;32m"
export BROWN="\033[00;33m"
export BLUE="\033[00;34m"
export PURPLE="\033[00;35m"
export CYAN="\033[00;36m"
export LIGHT_GRAY="\033[00;37m"
export DARK_GRAY="\033[01;30m"
export LIGHT_RED="\033[01;31m"
export LIGHT_GREEN="\033[01;32m"
export YELLOW="\033[01;33m"
export LIGHT_BLUE="\033[01;34m"
export LIGHT_PURPLE="\033[01;35m"
export LIGHT_CYAN="\033[01;36m"
export WHITE="\033[01;37m"
export NO_COLOUR="\033[0m"

# Set the prompt
export PS1="\[$LIGHT_GREEN\]\u\[$CYAN\]@\h:\[$LIGHT_BLUE\]\w\[$NO_COLOUR\]$ "

# Alias definitions.
if [[ -f "$HOME/.bash_aliases" && -r "$HOME/.bash_aliases" ]]; then
    . "$HOME/.bash_aliases"
fi

# Function defintions.
if [[ -f "$HOME/.bash_funcs" && -r "$HOME/.bash_funcs" ]]; then
    . "$HOME/.bash_funcs"
fi

# Define a pretty greeting
export GREETING="${CYAN}This is BASH ${RED}${BASH_VERSION%.*}\
${CYAN} - on DISPLAY: ${RED}$DISPLAY${NO_COLOUR}\n"

echo -e "$GREETING"
# defined in .bash_funcs, clears screen and prints greeting
# clg

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
