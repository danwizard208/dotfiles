# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# POWERLEVEL9K {{{
export POWERLEVEL9K_STATUS_VERBOSE=false
export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir vcs)
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status vi_mode time)
export POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
export POWERLEVEL9K_MODE='awesome-fontconfig'
POWERLEVEL9K_VI_MODE_INSERT_BACKGROUND=8
POWERLEVEL9K_VI_MODE_NORMAL_BACKGROUND=8
POWERLEVEL9K_CONTEXT_DEFAULT_BACKGROUND=8
# export ZLE_RPROMPT_INDENT=0
# }}}

# Antigen {{{

source ~/dotfiles/antigen/antigen.zsh
antigen use oh-my-zsh

antigen bundle git
antigen bundle vi-mode
antigen bundle last-working-dir
antigen bundle copydir
antigen bundle copyfile
antigen bundle extract
antigen bundle history
antigen bundle web-search

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Theme {{{
if (( $(echotc Co) >= 256 )); then
    if [[ ! ($(tty) =~ ^/dev/tty) ]]; then
        antigen theme bhilburn/powerlevel9k powerlevel9k
    fi
fi
# }}}

# Tell antigen that you're done.
antigen apply

# }}}

setopt KSH_AUTOLOAD
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"

# Dircolor settings {{{
eval `dircolors ~/dotfiles/dircolors-solarized/dircolors.ansi-universal`
# }}}

# Default editor {{{
if (type nvim &> /dev/null); then
    export EDITOR='nvim'
    export VISUAL='nvim'
else if (type vim &> /dev/null); then
    export EDITOR='vim'
    export VISUAL='vim'
else
    export EDITOR='vi'
    export VISUAL='vi'
fi
fi
# }}}

# ssh {{{
export SSH_KEY_PATH="~/.ssh/id_rsa"
# }}}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# todir-tools {{{
fpath+=("/home/daniel/dotfiles/todir-tools/")
export TODIR_ROOT="$HOME/todir"
autoload oh
# }}}

typeset -U path

path+=("$HOME/bin")
path+=("$HOME/utils/bin")
KEYTIMEOUT=2
# vim:foldmethod=marker
alias path='echo ${(F)path}'
;
