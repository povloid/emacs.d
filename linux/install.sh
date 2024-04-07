#!/bin/bash

cd

mkdir -p bin
mkdir -p global/npm/bin
mkdir -p global/yarn/bin

export PATH="bin/:${PATH}"
export PATH="global/npm/bin/:${PATH}"
export PATH="global/yarn/bin/:${PATH}"

ln -sf git/emacs.d .emacs.dl
ln -sf git/emacs.d/linux/zshrc .zshrc
ln -sf git/emacs.d/linux/tmux.conf .tmux.confl
ln -sf git/emacs.d/linux/alacritty.yml .alacritty.yml

sudo apt update
sudo apt upgrade

# console
sudo apt install -y \
     git htop ncal mc tree\
     wget nano vim htop locate p7zip p7zip-full unzip \
     apt-xapian-index \
     ispell \
     tmux \
     zsh \
     build-essential \
     emacs-nox silversearcher-ag

# GUI
sudo apt install -y \
     alacritty stellarium\
     ttf-anonymous-pro\
     fonts-jetbrains-mono \
     fonts-monoid \
     chromium \
     fonts-league-mono \
     fonts-paratype \
     libavcodec-extra vlc \
     gparted transmission-gtk vlc pavucontrol \
     geany gimp inkscape audacity filezilla

###############################################################################
#                                    python                                   #
###############################################################################

# pip3 install 'python-language-server[all]'
# pip3 install -U setuptools
# pip3 install virtualenvwrapper flake8 pep8 importmagic autopep8 yapf nose
# pip3 install -U virtualenvwrapper flake8 pep8 importmagic autopep8 yapf nose
# pip3 install sqlparse
# pip3 install awscli --upgrade --user

###############################################################################
#                                    nodejs                                   #
###############################################################################

curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo bash -&&\
    sudo apt install -y nodejs

###############################################################################
#                                     yarn                                    #
###############################################################################

export NPM_CONFIG_PREFIX=~/global/npm

npm install --global yarn

yarn config set prefix ~/global/yarn/ \
    && yarn global add bash-language-server \
    && yarn global add js-beautify \
    && yarn global add preact-cli \
    && yarn global add prettier \
    && yarn global add sql-formatter-cli \
    && yarn global add typescript \
    && yarn global add typescript-language-server \
