all: build install
install: install-zsh install-vim install-emacs
build: build-zsh build-vim build-emacs build-python build-ruby build-clojure

install-zsh: build-zsh
	if test -f ~/.zshrc; then mv ~/.zshrc ~/.zshrc.bak; fi
	if test -f ~/.oh-my-zsh/themes/swift.zsh-theme; then rm ~/.oh-my-zsh/themes/swift.zsh-theme; fi
	ln -s `pwd`/zsh/zshrc ~/.zshrc
	ln -s `pwd`/zsh/swift.zsh-theme ~/.oh-my-zsh/themes/swift.zsh-theme

install-vim: build-vim
	if test -d ~/.vim; then mv ~/.vim ~/.vim.bak; fi
	if test -f ~/.vimrc; then mv ~/.vimrc ~/.vimrc.bak; fi
	ln -s `pwd`/vim ~/.vim
	ln -s `pwd`/vim/vimrc ~/.vimrc
	cd vim/bundle/command-t; rake make

install-emacs:
	USERNAME=$(shell whoami)
	mkdir -p ~/.emacs.d/$(USERNAME)
	test -d ~/.emacs.d/$(USERNAME)/color-theme-solarized || git clone git://github.com/sellout/emacs-color-theme-solarized.git ~/.emacs.d/$(USERNAME)/color-theme-solarized
	if test -f ~/.emacs.d/init.el; then mv ~/.emacs.d/init.el ~/.emacs.d/init.el.bak; fi
	if test -f ~/.emacs.d/$(USERNAME).el; then mv ~/.emacs.d/$(USERNAME).el ~/.emacs.d/$(USERNAME).el.bak; fi
	ln -s `pwd`/emacs/init.el ~/.emacs.d/init.el
	ln -s `pwd`/emacs/emacs.el ~/.emacs.d/$(USERNAME).el

build-zsh:
	apt-get install zsh
	if ! test -d ~/.oh-my-zsh/; then wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | sh; fi
	chsh -s /bin/zsh

build-vim:
	apt-get install vim vim-gnome ack exuberant-ctags vim-nox

build-emacs:
	wget -q -O - http://emacs.naquadah.org/key.gpg | sudo apt-key add -
	echo 'deb http://emacs.naquadah.org/ stable/\ndeb-src http://emacs.naquadah.org/ stable/' >> /etc/apt/sources.list
	apt-get update && apt-get install emacs-snapshot-common

build-python:
	apt-get install python python-dev python-pip
	pip install pip -U
	pip install pep8 pyflakes nose django-nose virtualenv virtualenvwrapper -U

build-ruby:
	apt-get install ruby ruby-dev rake

build-clojure:
	mkdir -p ~/bin/
	if test -f ~/bin/lein; then rm ~/bin/lein; fi
	wget https://raw.github.com/technomancy/leiningen/stable/bin/lein -O - > ~/bin/lein
