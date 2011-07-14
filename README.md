# Dot Files
Collection of config files to setup my system

## Installation
### Dependences
#### zsh
If you plan on using zsh settings you will need [oh-my-zsh][]

#### vim
To get the most of vim you will need to install the following:

[Python][] and [PIP][]

    apt-get python pip

[Pep8][], [Pyflakes][], [Nose][] and [Django Nose][]

    pip install pep8 pyflakes nose django-nose

[Ruby][], [Rake][] and [Ack][]

    apt-get install ruby ruby-dev rake ack

If your vim does not have ruby support

    apt-get install vim-nox


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

### Script
At some point.

### Manual

    git clone --recurse-submodules git://github.com/shanewilson/dotfiles.git

#### zsh
This assumes you have *oh-my-zsh* installed and working.

Backup your current *~/.zsh* and *~/.zshrc*

Symlink *~/.zsh* and *~/.zshrc* to *\_zsh* and *\_zshrc* respectively

    ln -s path/to/dotfiles/_zsh ~/.zsh
    ln -s path/to/dotfiles/_zshrc ~/.zshrc

Symlink to the *oh-my-zsh* theme

    ln -s path/to/dotfiles/_zsh/xeno.zsh-theme ~/.oh-my-zsh/themes/xeno.zsh-theme

#### vim
Backup your current *~/.vim* and *~/.vimrc*

Symlink *~/.vim* and *~/.vimrc* to *\_vim* and *\_vimrc* respectively

    ln -s path/to/dotfiles/_vim ~/.vim
    ln -s path/to/dotfiles/_vimrc ~/.vimrc

Setup Command-T

    cd path/to/dotfiles/_vim/bundle/command-t
    rake make

#### Solarized Terminal
Everything was put together while using the [solarized][] colour scheme, so you should probably make sure to use it. It is already included with vim, but the get the most out of the zsh theme you will have to configure your terminal as well.  
[solarized genome terminal]: http://xorcode.com/guides/solarized-vim-eclipse-ubuntu/
