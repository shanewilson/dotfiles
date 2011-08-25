# Dot Files
Collection of config files to setup my system

## Installation

    git clone --recurse-submodules git://github.com/shanewilson/dotfiles.git

### Dependences
#### zsh
If you plan on using zsh settings you will need [oh-my-zsh][]

#### vim
To get the most of vim you will need to install the following:

[Python][] and [PIP][]

    apt-get python python-setuptools
    easy_install pip

[Pep8][], [Pyflakes][], [Nose][] and [Django Nose][]

    sudo pip install pep8 pyflakes nose django-nose

[Ruby][], [Rake][] and [Ack][]

    apt-get install ruby ruby-dev rake ack

If your vim does not have ruby support

    apt-get install vim-nox

Ctags support for TagList

    apt-get install exuberant-ctags

[oh-my-zsh]: https://github.com/robbyrussell/oh-my-zsh
[Python]: http://www.python.org
[PIP]: http://pypi.python.org/pypi/pip
[Pep8]: http://pypi.python.org/pypi/pep8
[Pyflakes]: http://pypi.python.org/pypi/pyflakes/0.4.0
[Nose]: http://pypi.python.org/pypi/nose/1.0.0
[Django Nose]: http://pypi.python.org/pypi/django-nose/0.1.3
[Ruby]: http://www.ruby-lang.org/
[Rake]: http://rake.rubyforge.org/
[Ack]: http://betterthangrep.com/

### Install Script

    make install

### Manual

**Backup any config files before you change them.**

#### zsh
This assumes you have *oh-my-zsh* installed and working.

Symlink *~/.zsh* and *~/.zshrc* to */zsh* and */zsh/zshrc* respectively

    ln -s path/to/dotfiles/zsh ~/.zsh
    ln -s path/to/dotfiles/zsh/zshrc ~/.zshrc
    ln -s path/to/dotfiles/zsh/swift.zsh-theme ~/.oh-my-zsh/themes/swift.zsh-theme

#### vim
Symlink *~/.vim* and *~/.vimrc* to */vim* and */vim/vimrc* respectively

    ln -s path/to/dotfiles/vim ~/.vim
    ln -s path/to/dotfiles/vim/vimrc ~/.vimrc

Setup Command-T

    cd path/to/dotfiles/vim/bundle/command-t
    rake make

#### git
Symlink *~/.gitconfig* to *\_gitconfig*

    ln -s path/to/dotfiles/_gitconfig ~/.gitconfig

#### Solarized Terminal
Everything was put together while using the [solarized][] colour scheme, so you should probably make sure to use it too. It is already included with vim, but to get the most out of the zsh theme you will need a [solarized genome terminal][] as well.

[solarized]: http://ethanschoonover.com/solarized
[solarized genome terminal]: http://xorcode.com/guides/solarized-vim-eclipse-ubuntu/

## Credits
https://github.com/ryanb/dotfiles  
http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/  
http://matija.suklje.name/?q=node/207  
