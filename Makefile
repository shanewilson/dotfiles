install: install-vim install-zsh

install-vim:
	mv ~/.vim ~/.vim.bak 
	mv ~/.vimrc ~/.vimrc.bak
	ln -s `pwd`/vim ~/.vim
	ln -s ~/vim/vimrc ~/.vimrc

install-bash:
	mv ~/.zshrc ~/.zshrc.bak
	ln -s `pwd`/zsh/zshrc ~/.zshrc
