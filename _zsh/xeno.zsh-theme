# vim:foldmethod=marker:foldlevel=0
### Shows state of the Versioning Control System (e.g. Git, Subversion, Mercurial
autoload -Uz vcs_info

### Needed for a pretty prompt
setopt prompt_subst # Enables additional prompt extentions
autoload -U colors && colors # Enables colours

# Custom Functions # {{{
# ============================================================
# Convert $HOME to ~
function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}
# Shows VIRTUAL_ENV info
function virtualenv_info {
     [ $VIRTUAL_ENV ] && echo '[ '`basename $VIRTUAL_ENV`' ] '
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
%{$fg[green]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[white]%}$(collapse_pwd)%{$reset_color%}${vcs_info_msg_0_}$(svn_dirty)%{$reset_color%}
%{$fg[magenta]%}$(virtualenv_info)%{$fg[green]%}$(prompt_char)%{$reset_color%} '

### Detailed VCS actions
RPROMPT='${return_status}$(git_prompt_status)$(svn_prompt_status)%{$reset_color%}'

# Change terminal cursor color to reflect vi mode
# http://andreasbwagner.tumblr.com/post/804629866/zsh-cursor-color-vi-mode
bindkey -v
zle-keymap-select () {
  if [ $TERM = "screen" ]; then
    if [ $KEYMAP = vicmd ]; then
      echo -ne '\033P\033]12;#b58900\007\033\\'
    else
      echo -ne '\033P\033]12;#eee8d5\007\033\\'
    fi
  elif [ $TERM != "linux" ]; then
    if [ $KEYMAP = vicmd ]; then
      echo -ne "\033]12;#b58900\007"
    else
      echo -ne "\033]12;#eee8d5\007"
    fi
  fi
}; zle -N zle-keymap-select
zle-line-init () {
  zle -K viins
  if [ $TERM = "screen" ]; then
    echo -ne '\033P\033]12;grey\007\033\\'
  elif [ $TERM != "linux" ]; then
    echo -ne "\033]12;grey\007"
  fi
}; zle -N zle-line-init
# }}}

