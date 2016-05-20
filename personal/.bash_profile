#!/bin/bash

. ~/.bashrc
source ~/.bash_funs
source ~/.git_tab_completion
source /Users/rebecca/erlangs/17.4/activate
if [ -e /Users/rebecca/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/rebecca/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
if [ -z $DOCKER_HOST ]; then eval $(docker-machine env default); fi

# Start the emacs deamon if it's not running
ps -wef | grep Emacs | grep -- '--daemon'
if [[ 0 -ne $? ]]; then
  /Applications/Emacs.app/Contents/MacOS/Emacs --daemon
fi
