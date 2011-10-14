USERNAME     = $(shell whoami)
  
# COMMANDS
ECHO         = echo -n $(BOLD)
CAT          = cat

# SOURCE FILES
dZSH         = `pwd`/zsh
dVIM         = `pwd`/vim
dEMACS       = `pwd`/emacs

# SYSTEM FILES
ZSH          = ~/.zsh
ZSHRC        = ~/.zshrc
OMZ          = ~/.oh-my-zsh
VIM          = ~/.vim
VIMRC        = ~/.vimrc
EMACS        = ~/.emacs.d

# OUTPUT
NO_COLOR     = \033[1;0m
ERROR_COLOR  = \033[0;31m
OK_COLOR     = \033[0;32m
WARN_COLOR   = \033[0;33m
BOLD         = `tput bold`
NORMAL       = `tput sgr0`

OK_STRING    = $(OK_COLOR)[OK]\n$(NO_COLOR)
ERROR_STRING = $(ERROR_COLOR)[ERRORS]\n$(NO_COLOR)
WARN_STRING  = $(WARN_COLOR)[WARNINGS]\n$(NO_COLOR)

symlink: symlink-zsh symlink-vim symlink-emacs
install: install-omz install-vim install-emacs

define RESULT
	@if test -e temp.errors; \
		then $(ECHO) "$(ERROR_STRING)" && $(CAT) temp.log && rm -f temp.* && exit 1; \
	elif test -s temp.log; then $(ECHO) "$(WARN_STRING)" && $(CAT) temp.log; \
	else $(ECHO) "$(OK_STRING)"; fi;
	@-rm -f temp.errors temp.log
endef

symlink-zsh: symlink-zshrc symlink-omz

symlink-zshrc: prepare-zshrc
	@$(ECHO) "Symlinking $(ZSHRC)..."
	@ln -s $(dZSH)/zshrc $(ZSHRC) 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

symlink-omz: prepare-omz
	@$(ECHO) "Symlinking OH-MY-ZSH theme..."
	@ln -s $(dZSH)/swift.zsh-theme $(OMZ)/themes/swift.zsh-theme 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

symlink-vim: symlink-vimdir symlink-vimrc

symlink-vimdir: prepare-vimdir
	@$(ECHO) "Symlinking $(VIM)..."
	@ln -s $(dVIM) $(VIM) 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

symlink-vimrc: prepare-vimrc
	@$(ECHO) "Symlinking $(VIMRC)..."
	@ln -s $(dVIM)/vimrc $(VIMRC) 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

symlink-emacs: symlink-emacs-init symlink-emacs-username

symlink-emacs-init: prepare-emacs-init
	@$(ECHO) "Symlinking $(EMACS)/init.el..."
	@ln -s $(dEMACS)/init.el $(EMACS)/init.el 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

symlink-emacs-username: prepare-emacs-username
	@$(ECHO) "Symlinking $(EMACS)/$(USERNAME).el..."
	@ln -s $(dEMACS)/emacs.el $(EMACS)/$(USERNAME).el 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

prepare-zshrc:
	@$(ECHO) 'Backing up $(ZSHRC)...'
	@if test -f $(ZSHRC); then rm -f $(ZSHRC).bak; mv $(ZSHRC) $(ZSHRC).bak; fi 1> /dev/null 2> temp.log || touch temp.errors
	@if test -h $(ZSHRC); then rm $(ZSHRC); fi 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

prepare-omz:
	@$(ECHO) 'Removing old OH-MY-ZSH theme...'
	@if test -h $(OMZ)/themes/swift.zsh-theme; then rm $(OMZ)/themes/swift.zsh-theme; fi 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

prepare-vimdir:
	@$(ECHO) "Building Vim plugins..."
	@cd $(dVIM)/bundle/command-t; rake make 1> /dev/null 2> ../../temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) 'Backing up $(VIM)...'
	@if test -d $(VIM); then rm -rf $(VIM).bak; mv $(VIM) $(VIM).bak; fi 1> /dev/null 2> temp.log || touch temp.errors
	@if test -h $(VIM); then rm -f $(VIM); fi 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

prepare-vimrc:
	@$(ECHO) 'Backing up $(VIMRC)...'
	@if test -f $(VIMRC); then rm -f $(VIMRC).bak; mv $(VIMRC) $(VIMRC).bak; fi 1> /dev/null 2> temp.log || touch temp.errors
	@if test -h $(VIMRC); then rm -f $(VIMRC); fi 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

prepare-emacs-init:
	@$(ECHO) 'Backing up $(EMACS)/init.el...'
	@if test -f $(EMACS)/init.el; then rm -f $(EMACS)/init.el.bak; mv $(EMACS)/init.el $(EMACS)/init.el.bak; fi 1> /dev/null 2> temp.log || touch temp.errors
	@if test -h $(EMACS)/init.el; then rm -f $(EMACS)/init.el; fi 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

prepare-emacs-username:
	@$(ECHO) "Creating $(EdMACS)/$(USERNAME)..."
	@test -d $(EMACS)/$(USERNAME) || mkdir -p $(EMACS)/$(USERNAME) 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) 'Backing up $(EMACS)/$(USERNAME).el...'
	@if test -f $(EMACS)/$(USERNAME).el; then rm -f $(EMACS)/$(USERNAME).el.bak; mv $(EMACS)/$(USERNAME).el $(EMACS)/$(USERNAME).el.bak; fi 1> /dev/null 2> temp.log || touch temp.errors
	@if test -h $(EMACS)/$(USERNAME).el; then rm -f ~/.emacs.d/$(USERNAME).el; fi 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) "Installing Solarized Emacs Theme..."
	@test -d $(EMACS)/$(USERNAME)/color-theme-solarized || git clone git://github.com/sellout/emacs-color-theme-solarized.git $(EMACS)/$(USERNAME)/color-theme-solarized 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-zsh:
	@$(ECHO) 'Installing Zsh...'
	@apt-get -y install zsh 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) 'Setting Zsh as default shell...'
	@chsh -s /bin/zsh 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-omz: install-zsh
	@$(ECHO) 'Installing Oh-My-Zsh...'
	@if ! test -d ~/.oh-my-zsh/; then wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | sh; fi
	$(RESULT)

install-vim: install-python-modules install-ruby
	@$(ECHO) 'Installing Vim...'
	@apt-get -y install vim vim-gnome ack exuberant-ctags vim-nox 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-emacs: install-emacs-sources
	@$(ECHO) 'Installing Emacs24 Snapshot...'
	@apt-get -y install emacs-snapshot-common 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-emacs-sources:
	@$(ECHO) 'Adding Emacs24 apt-key...'
	@wget -q -O - http://emacs.naquadah.org/key.gpg | sudo apt-key add - 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) 'Adding Emacs24 sources to sources.list...'
	@echo 'deb http://emacs.naquadah.org/ stable/\ndeb-src http://emacs.naquadah.org/ stable/' >> /etc/apt/sources.list 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) 'Updating Sources...'
	@apt-get update 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-python-modules: install-python
	@$(ECHO) 'Installing Python modules...'
	@pip install pep8 pyflakes nose django-nose virtualenv virtualenvwrapper 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-python:
	@$(ECHO) 'Installing Python dependencies...'
	@apt-get -y install python python-dev python-pip 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)
	@$(ECHO) 'Updating PIP...'
	@pip install pip -U 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-ruby:
	@$(ECHO) 'Installing Ruby dependencies...'
	@apt-get -y install ruby ruby-dev rake 1> /dev/null 2> temp.log || touch temp.errors
	$(RESULT)

install-lein:
	@$(ECHO) 'Installing Leiningen...'
	@mkdir -p ~/bin/
	@if test -f ~/bin/lein; then rm ~/bin/lein; fi 1> /dev/null 2> temp.log || touch temp.errors
	@wget https://raw.github.com/technomancy/leiningen/stable/bin/lein -O - > ~/bin/lein 1> /dev/null 2> temp.log || touch temp.errors
	@chmod +x ~/bin/lein
	$(RESULT)
