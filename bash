# pretty prompt
if [[ ${EUID} == 0 ]] ; then
  # root/admin prompt
  export PS1='\[\033[1;31m\]\h\[\033[1;37m\]:\[\033[0;34m\]\w \[\033[1;31m\]${?##0}\[\033[0;36m\]# \[\033[0m\]'
else
  # ordinary user prompt
  export PS1='\[\033[1;32m\]\u\[\033[0m\]@\[\033[1;31m\]\h\[\033[1;37m\]:\[\033[0;35m\]\w \[\033[1;31m\]${?##0}\[\033[0;36m\]\$ \[\033[0m\]'
fi

# pretty ls
ls --color=auto >/dev/null 2>&1  #test
if [ $? == 0 ]; then
  alias ls='ls --color=auto'
fi

# pretty grep
grep --color=auto "" /dev/null 2>&1  # test
if [ $? == 0 ] || [ $? == 1 ]; then
  alias grep='grep --color=auto'
fi

# fix window resize
shopt -s checkwinsize

# xterm title
export PROMPT_COMMAND="echo -ne \"\033]0;$USER@$HOSTNAME\007\""

# Some nice aliases
alias s="ssh"

# add ~/.bin to the front of PATH (if it exists).
if [ -d $HOME/bin ]; then
  export PATH=$HOME/bin:$PATH
fi

alias drm="docker ps -qa | xargs docker rm ; docker images -qa | xargs docker rmi"
