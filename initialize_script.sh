# Install Git
# TODO

# Clone repos
mkdir ~/dev_repos
# TODO Ask for a list of repos, iterate through each and clone it

# Emacs configuration
ln -s /Users/sebastian/dev_repos/dotfiles/emacs/ ~/.emacs.d
ln -s /Users/sebastian/.bash_profile /Users/sebastian/.emacs_bash
ln -s git/gitconfig.symlink ${current dir??}/.gitconfig

# Establish emacs binary as the Emacs.app binary
sudo mv /usr/bin/{,old}emacs
sudo cp ./bin/emacs /usr/bin/emacs

# Node Version Manager (NVM)
curl https://raw.github.com/creationix/nvm/master/install.sh | sh
# TODO: Install some node version inside nvm and also install something to uglify
# https://github.com/creationix/nvm/
