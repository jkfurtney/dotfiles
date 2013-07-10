# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.


#ZSH_THEME="intheloop"
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)


# disable ctrl-D logout
setopt IGNORE_EOF

source $ZSH/oh-my-zsh.sh

unsetopt correct

# Customize to your needs...
export PATH=$PATH:/opt/ThirdParty-2.2.0/platforms/linux64Gcc/gperftools-svn/bin:/home/jkf/OpenFOAM/jkf-2.2.0/platforms/linux64GccDPOpt/bin:/opt/site/2.2.0/platforms/linux64GccDPOpt/bin:/opt/openfoam220/platforms/linux64GccDPOpt/bin:/opt/openfoam220/bin:/opt/openfoam220/wmake:.:/home/jkf/bin/:/home/jkf/.local/bin/:/home/jkf/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games


alias conk='firefox --app ~/src/conkeror/application.ini &> /dev/null &'
alias gs='git status'
alias gk='gitk --all'
alias fcpp='find . -name "*.cpp"'
#alias ip='ipython -i'
alias ipp='~/.local/bin/ipython -i'
#set -o ignoreeof
alias rm='rm -i'
alias mv='mv -i'
alias gd='git diff'
alias gba='git branch -a'
alias gut='git ls-files --other --exclude-standard'
PATH=.:~/bin/:~/.local/bin/:$PATH
alias ipnb='~/.local/bin/ipython notebook --pylab inline'
alias kp='~/.local/bin/kernprof.py'
alias ptest='python tests/test_fmm.py'
alias pbuild='python setup.py install --user --record install.log && ptest'
alias nt='nosetests -v'
LD_LIBRARY_PATH="~/dist/lib"
