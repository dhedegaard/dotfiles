# pretty prompt
if [[ ${EUID} == 0 ]] ; then
	export PS1='\[\033[01;31m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
else
	export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
fi

# pretty ls
alias ls='ls --color=auto'

# xterm title
export PROMPT_COMMAND="echo -ne \"\033]0;$USER@$HOSTNAME\007\""

# add ~/.bin to the front of PATH (if it exists).
if [ -d $HOME/bin ]; then
  export PATH=$HOME/bin:$PATH
fi