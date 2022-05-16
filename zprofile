export LC_ALL=en_US.UTF-8

# emacs

export PATH="/Applications/Emacs.app/Contents/MacOS/bin/":${PATH}

export EDITOR='ec'
export TTT=".zprofile"

# iTerm 2

# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1

# Set colors to match iTerm2 Terminal Colors
#export TERM=xterm-256color
export TERM=xterm-24bit

# path

export PATH="/Users/pacman/bin/:${PATH}"

export PATH="/usr/local/opt/krb5/bin:$PATH"
export PATH="/usr/local/opt/libpq/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/usr/local/opt/ncurses/bin:$PATH"
export PATH="/usr/local/opt/qt/bin:$PATH"

# python

export PATH="/usr/local/opt/python@3.9/bin:$PATH"

# ruby

export PATH="/Users/pacman/gem_modules/bin/:${PATH}"
export GEM_HOME="/Users/pacman/gem_modules/"

# node

export PATH="/usr/local/opt/node@14/bin:$PATH"
export NPM_CONFIG_PREFIX=~/global/npm
export PATH="/Users/pacman/global/npm/bin/:${PATH}"
export PATH="/Users/pacman/global/yarn/bin/:${PATH}"

export LDFLAGS="-L/usr/local/opt/node@14/lib"
export CPPFLAGS="-I/usr/local/opt/node@14/include"

# overtok default some

source ~/git/overtok/qa/overtok/release/set_qa_env.sh

# configuration envs

export LDFLAGS="-L/usr/local/opt/icu4c/lib"
export CPPFLAGS="-I/usr/local/opt/icu4c/include"

# java

#export PATH="/usr/local/opt/openjdk/bin:$PATH"
#export PATH="/opt/jdk/bin:$PATH"
#export JAVA_HOME="/opt/jdk"
#export JDK_HOME="/opt/jdk"

# Visual Studio Code (code)

export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

# AWS

export AWS_PAGER=""

# google cloud

export PATH="/Users/pacman/opt/google-cloud-sdk/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/pacman/opt/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/pacman/opt/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/pacman/opt/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/pacman/opt/google-cloud-sdk/completion.zsh.inc'; fi
