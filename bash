# pretty prompt
if [[ ${EUID} == 0 ]] ; then
	export PS1='\[\033[01;31m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
else
	export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
fi
# pretty ls
alias ls='ls --color=auto'
