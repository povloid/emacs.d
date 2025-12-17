#!/bin/bash

cd

mkdir -p bin
#mkdir -p global/npm/bin
#mkdir -p global/yarn/bin

export PATH="bin/:${PATH}"
#export PATH="global/npm/bin/:${PATH}"
#export PATH="global/yarn/bin/:${PATH}"

ln -sf git/emacs.d .emacs.d
ln -sf git/emacs.d/linux/zshrc .zshrc
ln -sf git/emacs.d/linux/tmux.conf .tmux.conf
ln -sf git/emacs.d/linux/alacritty.yml .alacritty.yml

sudo apt update
sudo apt upgrade

# console
sudo apt install -y \
     git htop ncal mc tree \
     wget nano vim htop locate zip p7zip p7zip-full unzip \
     ispell \
     tmux zsh \
     build-essential \
     smartmontools \
     emacs-nox \
     ufw \
     rsync \
     sshfs \
     silversearcher-ag

# GUI
sudo apt install -y \
     alacritty \
     atril  \
     audacious \
     audacity \
     chromium  \
     filezilla \
     fonts-jetbrains-mono  \
     fonts-league-mono  \
     fonts-monoid  \
     fonts-paratype  \
     fonts-roboto \
     fonts-roboto-fontface \
     fonts-roboto-slab \
     fonts-roboto-unhinted \
     gimp \
     gnome-shell-extension-appindicator \
     gparted \
     gufw \
     inkscape \
     libavcodec-extra \
     memtest86+ \
     menulibre \
     mpv \
     remmina \
     stellarium \
     transmission \
     transmission-gtk   \
     vlc \
     xclip


# Radeon Video Driver
sudo apt install firmware-amd-graphics mesa-vulkan-drivers libgl1-mesa-dri xserver-xorg-video-all

# KVM

sudo apt install qemu-kvm libvirt-clients libvirt-daemon-system bridge-utils virtinst libvirt-daemon virt-manager -y

sudo usermod -aG kvm ${USER}
sudo usermod -aG libvirt ${USER}

# docker

sudo apt install docker.io docker-compose containerd runc

sudo gpasswd --add ${USER} docker

# rust

sudo apt install rustc cargo


# QT 6
# sudo apt install git cmake g++ gdebi synaptic git ninja-build
# sudo apt install qtcreator qml6-module-* qt6-*-dev

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

sudo apt install -y nodejs

###############################################################################
#                                     yarn                                    #
###############################################################################

sudo apt install -y yarnpkg

# export NPM_CONFIG_PREFIX=~/global/npm

# npm install --global yarn

# yarn config set prefix ~/global/yarn/ \
#    && yarn global add bash-language-server \
#    && yarn global add js-beautify \
#    && yarn global add preact-cli \
#    && yarn global add prettier \
#    && yarn global add sql-formatter-cli \
#    && yarn global add typescript \
#    && yarn global add typescript-language-server

###############################################################################
#                                  ssh-agent                                  #
###############################################################################

# mkdir -p ~/.config/plasma-workspace/env
# cp start-ssh-agent.sh ~/.config/plasma-workspace/env/start-ssh-agent.sh
