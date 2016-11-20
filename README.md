# Provision OSX with Ansible

*Template for prosvisioning local OSX machine for web apps development*

This is inspired by [@roolo's provisioning scripts](roolo/provision-ansible).

## Previous steps

* Go to "Sharing" and change the machine's name.
* Update the `group_vars/all.yml` accordingly.

## Local provision

* Install latest Xcode from App Store.
* Make sure latest command line tools are installed (run
  `xcode-select --install`).
* Make sure Python is installed (usually bundled with OSX).
* Install pip `sudo easy_install pip`.
* Install Ansible `sudo pip install ansible`.
* Get this repo directory into the machine somehow.
* Run `ansible-playbook -v -i local provision.yml --ask-sudo-pass` in this
  directory.

## Remote provision

* Add local machine to remote machines `.ssh/authorized_keys`.
* Install latest Xcode from App Store on remote machine.
* Make sure latest command line tools are installed from the XCode app (usually
  bundled with the latest) on remote machine.
* Create `remote` file with the IP of the remote machine (see `remote.example`).
* Make sure remote connections are allowed on the remote machine. To enable
  this, go to "System Preferences", "Sharing", "Remote Login" and allow
  "Administrators". Make sure you turn this off after the provisioning is over.
* Also, make sure there's no need for a password prompt for sudo commands. To do
  this add `%{{ user }} ALL=(ALL) NOPASSWD:ALL` through `sudo visudo`.
* Run `ansible-playbook -v -i remote provision.yml`  in this directory.

### Notes

If the remote host has a password to connect through SSH you should also append
the following parameters to the `ansible-playbook` command:
`--ask-pass --ask-sudo-pass -c paramiko`.

## Post provision

* Import the terminal config file located in the Desktop and set it as the
  default profile.
* Go into iTunes, on the menu "Store", "Authorize this computer" so you're
  able to download the songs.
