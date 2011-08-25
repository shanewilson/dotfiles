install: install-zsh install-vim install-xterm

install-zsh:
	if test -f ~/.zshrc; then mv ~/.zshrc ~/.zshrc.bak; fi
	if test -f ~/.oh-my-zsh/themes/swift.zsh-theme; then rm ~/.oh-my-zsh/themes/swift.zsh-theme; fi
	ln -s `pwd`/zsh/zshrc ~/.zshrc
	ln -s `pwd`/zsh/swift.zsh-theme ~/.oh-my-zsh/themes/swift.zsh-theme

install-vim:
	if test -d ~/.vim; then mv ~/.vim ~/.vim.bak; fi
	if test -f ~/.vimrc; then mv ~/.vimrc ~/.vimrc.bak; fi
	ln -s `pwd`/vim ~/.vim
	ln -s `pwd`/vim/vimrc ~/.vimrc
	git submodule init
	git submodule update
	cd vim/bundle/command-t; rake make

install-xterm:
	if test -f ~/.Xdefaults; then mv ~/.Xdefaults ~/.Xdefaults.bak; fi
	ln -s `pwd`/.Xdefaults ~/.Xdefaults
