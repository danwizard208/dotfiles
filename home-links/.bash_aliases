#Silly personal aliases
alias q=exit
alias woman=man

#Interactive mode for rm: Less mistakes
alias rm='rm -I'

#ls family:
#
#ld: Detailed information about all files, in human readable format
alias ld='ls -Alh'
#la: List all files but '.' and '..'
alias la='ls -A' 
#l.: List only hidden files
alias l.='ls -d .*'

#Prompt before clobbering
alias cp='cp -i'
alias mv='mv -i'

#Recursive mkdir
alias mkdir='mkdir -p'

#Handy abbreviations
alias h='history'
alias j='jobs -l'
alias ..='cd ..'
alias ...='cd ../..'
alias path='echo -e ${PATH//:/\\n}'

#More understandble info from du
alias du='du -kh'

#Installing should take only 2 characters!
alias ai='sudo apt-get install'

#I prefer to edit in multiple tabs, not multiple buffers
alias gvim='gvim -p'
alias vim='vim -p'

alias ldmstart='sudo /etc/init.d/lightdm start'
alias ldmstop='sudo /etc/init.d/lightdm stop'
