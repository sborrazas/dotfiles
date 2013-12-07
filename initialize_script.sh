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

# Brew - postgresql
brew install postgresql
# Initialize the location where postgres will store its data
initdb /usr/local/var/postgres
# Start the postgres server in the background
pg_ctl -D /usr/local/var/pg_data -l /tmp/postgres_logfile start
# Copy the launch agent to start postgres on login
cp /usr/local/Cellar/postgresql/9.3.1/homebrew.mxcl.postgresql.plist ~/Library/LaunchAgents/
launchctl load -w ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist

# Python
brew install python
pip install virtualenv
