# Install Git
# TODO

# Install Heroku Toolbelt
# TODO

# Install rbenv
# TODO

# Clone repos
mkdir ~/dev_repos
# TODO Ask for a list of repos, iterate through each and clone it

# Emacs configuration
ln -s $HOME/dev_repos/dotfiles/emacs/ ~/.emacs.d
ln -s $HOME/.bash_profile $HOME/.emacs_bash
ln -s "$(pwd)/git/gitconfig.symlink" $HOME/.gitconfig
ln -s "$(pwd)/osx/bashrc.symlink" $HOME/.bashrc
ln -s "$(pwd)/osx/bash_profile.symlink" $HOME/.bash_profile

# Establish emacs binary as the Emacs.app binary
sudo mv /usr/bin/{,old}emacs
sudo ln -s "$(pwd)/bin/emacs" /usr/bin/emacs

sudo mv /usr/bin/{,old}emacsclient
sudo ln -s "$(pwd)/bin/emacsclient" /usr/bin/emacsclient

# Node Version Manager (NVM)
brew install node
# Install brew
# ..

# Python
brew install python
pip install virtualenv
