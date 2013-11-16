# Install Git
# TODO

# Install Heroku Toolbelt
# TODO

# Clone repos
mkdir ~/dev_repos
# TODO Ask for a list of repos, iterate through each and clone it

# Emacs configuration
ln -s $HOME/dev_repos/dotfiles/emacs/ ~/.emacs.d
ln -s $HOME/.bash_profile $HOME/.emacs_bash
ln -s "$(pwd)/git/gitconfig.symlink" $HOME/.gitconfig

# Establish emacs binary as the Emacs.app binary
sudo mv /usr/bin/{,old}emacs
sudo ln -s "$(pwd)/bin/emacs" /usr/bin/emacs

sudo mv /usr/bin/{,old}emacsclient
sudo ln -s "$(pwd)/bin/emacsclient" /usr/bin/emacsclient

# Node Version Manager (NVM)
curl https://raw.github.com/creationix/nvm/master/install.sh | sh
# TODO: Install some node version inside nvm and also install something to uglify
# https://github.com/creationix/nvm/

# Install brew
# ..
# Install postgresql with brew
brew install postgresql
