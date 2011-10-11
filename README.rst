=========
Dot Files
=========

Collection of config files to setup my system

Installation
------------

Clone from github::

   git clone --recurse-submodules git://github.com/shanewilson/dotfiles.git

Dependences
~~~~~~~~~~~

zsh
'''

If you plan on using zsh settings you will need oh-my-zsh_

vim
'''

To get the most of vim you will need to install the following:

Python_ and PIP_::

    sudo apt-get install python-pip
    sudo pip install pip -U

Pep8_, Pyflakes_, Nose_, `Django Nose`_ and Virtualenv_::

    sudo pip install pep8 pyflakes nose django-nose virtualenv virtualenvwrapper

Ruby_, Rake_, Ack_, Ctags_ and Vim Ruby support::

    sudo apt-get install ruby ruby-dev rake ack exuberant-ctags vim-nox

.. _oh-my-zsh: https://github.com/robbyrussell/oh-my-zsh
.. _Python: http://www.python.org
.. _PIP: http://pypi.python.org/pypi/pip
.. _Pep8: http://pypi.python.org/pypi/pep8
.. _Pyflakes: http://pypi.python.org/pypi/pyflakes/0.4.0
.. _Nose: http://pypi.python.org/pypi/nose/1.0.0
.. _Virtualenv: http://pypi.python.org/pypi/virtualenv
.. _Django Nose: http://pypi.python.org/pypi/django-nose/0.1.3
.. _Ruby: http://www.ruby-lang.org/
.. _Rake: http://rake.rubyforge.org/
.. _Ack: http://betterthangrep.com/
.. _Ctags: http://ctags.sourceforge.net/

Install Script
~~~~~~~~~~~~~~

Type the following in the dotfiles directory::

    make install

Manual
~~~~~~

**Backup any config files before you change them.**

zsh
'''

This assumes you have *oh-my-zsh* installed and working.

Symlink *~/.zsh* and *~/.zshrc* to */zsh* and */zsh/zshrc* respectively::

    ln -s path/to/dotfiles/zsh ~/.zsh
    ln -s path/to/dotfiles/zsh/zshrc ~/.zshrc
    ln -s path/to/dotfiles/zsh/swift.zsh-theme ~/.oh-my-zsh/themes/swift.zsh-theme

vim
'''

Symlink *~/.vim* and *~/.vimrc* to */vim* and */vim/vimrc* respectively::

    ln -s path/to/dotfiles/vim ~/.vim
    ln -s path/to/dotfiles/vim/vimrc ~/.vimrc

Setup Command-T::

    cd path/to/dotfiles/vim/bundle/command-t
    rake make

git
'''

Copy any lines you want from */gitconfig* to your *~/.gitconfig* files

Solarized Terminal
''''''''''''''''''

Everything was put together while using the solarized_ colour scheme, so you should probably make sure to use it too. It is already included with vim, but to get the most out of the zsh theme you will need a `solarized genome terminal`_ as well.

.. _solarized: http://ethanschoonover.com/solarized
.. _solarized genome terminal: http://xorcode.com/guides/solarized-vim-eclipse-ubuntu/

Credits
-------

https://github.com/ryanb/dotfiles

http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/

http://matija.suklje.name/?q=node/207
