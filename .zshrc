# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/chrisn/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


bundles=(
    "plugins/docker, from:oh-my-zsh" # Completion for docker
    "plugins/fasd, from:oh-my-zsh" # Quickly jump to directories
    "plugins/lein, from:oh-my-zsh" # Completion for lein
    "plugins/sudo, from:oh-my-zsh" # Quickly re-run commands with sudo
    "plugins/tmuxinator, from:oh-my-zsh" # Completion for tmuxinator
    "plugins/vault, from:oh-my-zsh" # Completion for vault
    "plugins/wd, from:oh-my-zsh" # Quickly jump to directories
    "zsh-users/zsh-autosuggestions" # Autosuggestions from history
)

################################################################################
#### Define bundle list management functions

# Usage: add_bundle <zplug-args>
#
# Adds a bundle to $bundles. Word splitting will be performed on
# zplug-args to determine the arguments that will be passed to zplug.
function add_bundle() {
    if ! (( ${bundles[(I)$1]} )); then
        bundles+=($1)
    fi
}

# Usage: remove_bundle <zplug-args>
#
# Removes a bundle from $bundles by name. The name should be exactly
# the same as it appears in $bundles, with spaces if necessary.
function remove_bundle() {
    bundles=("${(@)bundles:#$1}")
}


################################################################################
#### zplug

export ZPLUG_HOME=$HOME/.zplug
export ZSH=$ZPLUG_HOME/repos/robbyrussell/oh-my-zsh
export ZSH_CACHE_DIR=$ZSH/cache

if [[ -f $ZPLUG_HOME/init.zsh ]]; then
    source $ZPLUG_HOME/init.zsh

    for bundle in $bundles; do
	zplug $=bundle
    done

    if ! zplug check; then
	zplug install
    fi

    zplug load
fi


################################################################################
#### Prompt

# Enable parameter expansion and other substitutions in the $PROMPT.
setopt prompt_subst

# Here we define a prompt that displays the current directory and git
# branch, and turns red on a nonzero exit code. Adapted heavily from
# [1], with supporting functions extracted from Oh My Zsh [2] so that
# we don't have to load the latter as a dependency.
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/master/themes/mgutz.zsh-theme
# [2]: https://github.com/robbyrussell/oh-my-zsh/blob/3705d47bb3f3229234cba992320eadc97a221caf/lib/git.zsh

# Function that prints the branch or revision of the current HEAD,
# surrounded by square brackets and followed by an asterisk if the
# working directory is dirty, if the user is inside a Git repository.
function radian_prompt_git_info() {
    local ref
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
        ref=$(command git rev-parse --short HEAD 2> /dev/null) || \
        return 0
    echo "[${ref#refs/heads/}$(radian_prompt_git_dirty)]"
}

# Function that prints an asterisk if the working directory is dirty.
# If $RADIAN_PROMPT_IGNORE_UNTRACKED_FILES is true, then untracked
# files are not counted as dirty.
function radian_prompt_git_dirty() {
    local FLAGS
    FLAGS=('--porcelain' '--ignore-submodules=dirty')
    if [[ $RADIAN_PROMPT_IGNORE_UNTRACKED_FILES == true ]]; then
        FLAGS+='--untracked-files=no'
    fi
    if [[ $(command git status ${FLAGS} 2> /dev/null | tail -n1) ]]; then
        echo "*"
    fi
}

# Define the actual prompt format.
PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(radian_prompt_git_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'


################################################################################
#### Completion

# For ambiguous completions, use an interactive menu (which can be
# escaped with C-g) instead of overwriting the current command.
zstyle ':completion:*' menu select

# Allow usage of shift-tab (backtab) to go backward in the completion
# menu.
bindkey '^[[Z' reverse-menu-complete

# Substring completions. Not fuzzy. Sometimes they have weird
# behavior. This is the best I can manage for now, since I've been
# working on completions literally all day. See [1]. (Why is zsh so
# hard? Sigh.)
#
# [1]: http://unix.stackexchange.com/q/330481/176805
zstyle ':completion:*' matcher-list 'l:|=* r:|=* m:{a-z\-}={A-Z\_}'



################################################################################
#### Globbing

# This makes globs case-insensitive.
unsetopt case_glob

# This makes globbing regexes case-insensitive.
unsetopt case_match

# Allow globs to match dotfiles.
setopt glob_dots

# Sort numeric filenames numerically, instead of lexicographically.
setopt numeric_glob_sort


################################################################################
#### Man

# By default, run-help is an alias to man. We want to turn that off so
# that we can access the function definition of run-help (by default,
# aliases take precedence over functions). But if you re-source this
# file, then the alias might already be removed, so we suppress any
# error that this might throw.
unalias run-help 2>/dev/null || true

# Now we tell Zsh to autoload the run-help function, meaning that when
# it is invoked, Zsh will load the function from the file where it is
# defined. (That file comes with Zsh.) There are additional functions
# that we can autoload that will increase the functionality of
# run-help, but unfortunately they have a serious bug that causes them
# to crash when there is an alias defined for the function that you
# are requesting help for. (For example, loading run-help-git causes
# an error when requesting help for git because we later alias
# git=hub.) So we don't bother with those.
autoload -Uz run-help

# We define a function that wraps man to provide some basic
# highlighting for man pages. This makes them a little easier on the
# eyes. (This is done by binding some environment variables that less
# looks at.) See [1].
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/3ebbb40b31fa1ce9f10040742cdb06ea04fa7c41/plugins/colored-man-pages/colored-man-pages.plugin.zsh
function man() {
    env \
	LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man $@
}


export TERM=xterm-256color
export S3_CACHE_DIR=/home/chrisn/dev/s3cache\n
export VAULT_ADDR=https://thinktopic.com:8200
export EDITOR='emacs -nw'
alias emacs-nw='emacs -nw'

# added by travis gem
[ -f /home/chrisn/.travis/travis.sh ] && source /home/chrisn/.travis/travis.sh
