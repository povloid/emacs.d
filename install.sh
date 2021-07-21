#!/bin/sh

# CONFDIR=$HOME/.emacs.d
# if [ -e $CONFDIR ]
# then
#     test -L $CONFDIR || mv $CONFDIR ${CONFDIR}.backup
# fi
# ln -Tsf `pwd` $HOME/.emacs.d

# mkdir -p $CONFDIR/elpa

brew install ispell
brew install alacritty
brew install showkey
brew install tmux
brew install emacs

brew install font-jetbrains-mono
brew install font-fira-mono
brew install font-fira-mono-for-powerline
brew install font-fira-mono-nerd-font
brew install font-awesome
brew install font-awesome-terminal-fonts
brew install font-fontawesome
brew install font-material-icons

###############################################################################
#                                    python                                   #
###############################################################################

pip3 install 'python-language-server[all]'
pip3 install -U setuptools
pip3 install virtualenvwrapper flake8 pep8 importmagic autopep8 yapf nose
pip3 install -U virtualenvwrapper flake8 pep8 importmagic autopep8 yapf nose
pip3 install sqlparse
pip3 install awscli --upgrade --user

###############################################################################
#                                    nodejs                                   #
###############################################################################

brew install node@14

npm install -g npm

###############################################################################
#                                     yarn                                    #
###############################################################################

yarn global add bash-language-server
yarn global add js-beautify
yarn global add preact-cli
yarn global add prettier
yarn global add sql-formatter-cli
yarn global add typescript
yarn global add typescript-language-server
