# vim:foldmethod=marker:foldlevel=0
EDITOR='vi'
bindkey -e
zle -N newtab

### Shows state of the Versioning Control System (e.g. Git, Subversion, Mercurial
autoload -Uz vcs_info

### Needed for a pretty prompt
setopt prompt_subst # Enables additional prompt extentions
autoload -U colors && colors # Enables colours

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
# Vi style:
# zle -N edit-command-line
# bindkey -M vicmd v edit-command-line

autoload -U zmv

autoload -U history-search-end

# Settings # {{{
# ============================================================

REPORTTIME=10 # print elapsed time when more than 10 seconds

setopt NO_BG_NICE # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS # allow functions to have local traps
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

# history:
setopt inc_append_history   # append history list to the history file (important for multiple parallel zsh sessions!)
setopt share_history        # import new commands from the history file also in other zsh-session
setopt extended_history     # save each command's beginning timestamp and the duration to the history file
setopt hist_ignore_all_dups # If  a  new  command  line being added to the history
                            # list duplicates an older one, the older command is removed from the list
setopt hist_ignore_space    # remove command lines from the history list when
                            # the first character on the line is a space

HISTFILE=$HOME/.zsh_history
HISTSIZE=2000
SAVEHIST=3000              # useful for setopt append_history

setopt auto_cd              # if a command is issued that can't be executed as a normal command,
                            # and the command is the name of a directory, perform the cd command to that directory
setopt extended_glob        # in order to use #, ~ and ^ for filename generation
                            # grep word *~(*.gz|*.bz|*.bz2|*.zip|*.Z) ->
                            # -> searches for word not in compressed files
                            # don't forget to quote '^', '~' and '#'!
setopt longlistjobs         # display PID when suspending processes as well
setopt notify               # report the status of backgrounds jobs immediately
setopt hash_list_all        # Whenever a command completion is attempted, make sure
                            # the entire command path is hashed first.
# setopt completeinword       # not just at the end
setopt nohup                # and don't kill them, either
# setopt auto_pushd         # make cd push the old directory onto the directory stack.
setopt nonomatch            # try to avoid the 'zsh: no matches found...'
setopt nobeep               # avoid "beep"ing
setopt pushd_ignore_dups    # don't push the same dir twice.
setopt noglobdots           # * shouldn't match dotfiles. ever.
setopt long_list_jobs       # List jobs in long format

setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY

setopt MENUCOMPLETE
# Set/unset  shell options
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash

HOSTNAME="`hostname`"
PAGER='less -R'

# }}}
# Binds # {{{
# ============================================================

#bindkey -v
#bindkey '\e[3~'  delete-char

bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
bindkey '^r' history-incremental-search-backward
bindkey '^R' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^[[H" beginning-of-line
bindkey "^[[1~" beginning-of-line
bindkey "^[[F"  end-of-line
bindkey "^[[4~" end-of-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
#bindkey -e
# }}}
# Completion # {{{
# ============================================================

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:processes' command 'ps -aU$USER'
# Completion Styles
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
#
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}') 
# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
zstyle '*' hosts $hosts

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

# }}}
# Aliases # {{{
# ============================================================
# xterm
#alias xterm="xterm -bg #002b36 -fg #657b83"

# cd
alias ..='cd ..'

# ls
alias l="ls -lAhF"

# git
alias gl='git pull'
alias gp='git push'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gs='git status -sb'
alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"
alias changelog='git log `git log -1 --format=%H -- CHANGELOG*`..; cat CHANGELOG*'
#alias git='hub'

# commands starting with % for pasting from web
alias %=' '
# }}}
# Custom Functions # {{{
# ============================================================
# Convert $HOME to ~
function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}
# Shows VIRTUAL_ENV info
function virtualenv_info {
    if [ "$VIRTUAL_ENV" ]; then
        echo '[ '`basename $VIRTUAL_ENV`' ] '
    fi
}
# HG Versioning Info
function hg_prompt_info {
    hg prompt --angle-brackets "\
< on %{$fg[magenta]%}<branch>%{$reset_color%}>\
< at %{$fg[yellow]%}<tags|%{$reset_color%}, %{$fg[yellow]%}>%{$reset_color%}>\
%{$fg[green]%}<status|modified|unknown><update>%{$reset_color%}<
patches: <patches|join( → )|pre_applied(%{$fg[yellow]%})|post_applied(%{$reset_color%})|pre_unapplied(%{$fg_bold[black]%})|post_unapplied(%{$reset_color%})>>" 2>/dev/null
}

# Get the status of the working svn tree
# Copied from the git function, reworked for svn
function svn_prompt_status() {
  INDEX=$(svn status 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | grep '^? ' &> /dev/null); then
    STATUS="$ZSH_THEME_SVN_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$ZSH_THEME_SVN_PROMPT_ADDED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^M ' &> /dev/null); then
    STATUS="$ZSH_THEME_SVN_PROMPT_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^D ' &> /dev/null); then
    STATUS="$ZSH_THEME_SVN_PROMPT_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^! ' &> /dev/null); then
    STATUS="$ZSH_THEME_SVN_PROMPT_MISSING$STATUS"
  fi
  echo $STATUS
}
# }}}
# VCS # {{{
# ============================================================
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' check-for-changes true

### Git Icons
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[green]%} ✚"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[blue]%} ✹"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%} ✖"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[magenta]%} ➜"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[yellow]%} ═"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[cyan]%} ✭"

### Svn Icons
ZSH_THEME_SVN_PROMPT_DIRTY=" %{$fg[yellow]%}✭"
ZSH_THEME_SVN_PROMPT_CLEAN=""
ZSH_THEME_SVN_PROMPT_ADDED="%{$fg[green]%} ✚"
ZSH_THEME_SVN_PROMPT_MODIFIED="%{$fg[blue]%} ✹"
ZSH_THEME_SVN_PROMPT_DELETED="%{$fg[red]%} ✖"
ZSH_THEME_SVN_PROMPT_MISSING="%{$fg[magenta]%} !"
ZSH_THEME_SVN_PROMPT_UNTRACKED="%{$fg[cyan]%} ✭"

### VCS formating
FMT_BRANCH=" on %F{blue}%r:%F{cyan}%b %c%u%f%f${PR_RST}"
FMT_ACTION="(%{$fg[green]%}%a${PR_RST})"
zstyle ':vcs_info:*' unstagedstr " %{$fg[yellow]%}✭"
zstyle ':vcs_info:*' stagedstr " %{$fg[green]%}✓"
zstyle ':vcs_info:*' actionformats "${FMT_BRANCH}${FMT_ACTION}"
zstyle ':vcs_info:*' formats "${FMT_BRANCH}"
zstyle ':vcs_info:*' nvcsformats ""
zstyle ':vcs_info:*' branchformat '%b@%r'

precmd () {
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:git*:*' formats ' on %F{blue}%r:%F{cyan}%b@%.5i%c%u%f%f'
    } else {
        zstyle ':vcs_info:git*:*' formats ' on %F{blue}%r:%F{cyan}%b@%.5i %F{red}✗%f%f'
    }
    vcs_info
}

### Detects the VCS and shows the appropriate sign
function prompt_char {
git branch >/dev/null 2>/dev/null && echo '± → ' && return
hg root >/dev/null 2>/dev/null && echo '☿ → ' && return
svn info >/dev/null 2>/dev/null && echo '⚡ → ' && return
echo '○ → '
}
# }}}
# Prompt Formatting # {{{
# ============================================================
MODE_INDICATOR="%{$fg_bold[red]%}❮%{$reset_color%}%{$fg[red]%}❮❮%{$reset_color%}"
local return_status="%{$fg[red]%}%(?..⏎)%{$reset_color%}"

PROMPT='
%{$fg[green]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[white]%}$(collapse_pwd)%{$reset_color%}${vcs_info_msg_0_}%{$reset_color%}
%{$fg[magenta]%}$(virtualenv_info)%{$fg[green]%}$(prompt_char)%{$reset_color%} '

### Detailed VCS actions
RPROMPT='${return_status}$(git_prompt_status)$(svn_prompt_status)%{$reset_color%}'
