# Dot Files
Collection of config files to setup my system
# Installation
## Dependences
#### zsh
If you plan on using zsh settings you will need [oh-my-zsh][]
#### vim
To get the most of vim you will need to install the following:  

* [Python][] and [PIP][]  

    apt-get python pip

* [Pep8][], [Pyflakes][], [Nose][] and [Django Nose][]  

    pip install pep8 pyflakes nose django-nose

* [Ruby][], [Rake][] and [Ack][]  

    apt-get install rake ruby ruby-dev ack

* If your vim does not have ruby support  

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

## Script

## Manual
symlink zsh: _zshrc, xeno.zsh-theme
symlink vim: _vimrc, _vim/
git submodule init && git submodule update && git submodule foreach git submodule init && git submodule foreach git submodule update
dl from bitbucket:
    hg clone https://bitbucket.org/ns9tks/vim-autocomplpop
    hg clone https://bitbucket.org/ns9tks/vim-l9
apt-get install vim-nox ruby rake ruby-dev
run rake make in command-t
solarized genome terminal: http://xorcode.com/guides/solarized-vim-eclipse-ubuntu/

