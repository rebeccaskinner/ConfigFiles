alias ls='ls --color=auto --classify'
alias ll='ls --color=auto --classify -l -a'
alias less='less -r'
alias ack='ack-grep --pager="less -r"'
alias gitk='gitk --all &'
alias tig='tig --all'
alias cmakea-debug='cmake -DCMAKE_INSTALL_PREFIX=/usr/ -DTESTS=on -DCMAKE_BUILD_TYPE=Debug -DBINARY=on -DHEADERS=on -DPKGCONFIG=on ..'
alias cmakea-release='cmake -DCMAKE_INSTALL_PREFIX=/usr/ -DTESTS=on -DCMAKE_BUILD_TYPE=Release -DBINARY=on -DHEADERS=on -DPKGCONFIG=on ..'
alias qiv='qiv -t -I'
alias gvalgrind='G_SLICE=alway-malloc G_DEBUG=gc-friendly'
alias screen='screen -T screen-256color'
alias grep='egrep --color=auto'
alias gp='git push origin head'

if [[ $(uname) == "Darwin" ]]; then
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
fi
