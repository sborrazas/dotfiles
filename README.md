# Provision local machine with Ansible

*Template for provisioning local OSX/Ubuntu machine for software development*

## Previous steps

* Go to "Sharing" and change the machine's name.
* Update the `group_vars/all.yml` accordingly.

## OSX Local provision

* Install latest Xcode from App Store.
* Make sure latest command line tools are installed (run
  `xcode-select --install`).
* Make sure Python is installed (usually bundled with OSX).
* Install pip `sudo easy_install pip`.
* Install Ansible `sudo pip install ansible`.
* Get this repo directory into the machine somehow.
* Run `ansible-playbook -v --ask-become-pass --skip-tags=osx -i local provision.yml` in this directory.

## OSX Remote provision

0. Make sure you copied the priv/pub keys to the backup HD, together with all
   the other home files that you'll need.
1. Install XCode from the AppStore.
2. Install Chrome, Docker, Brave, Emacs and Spotify using the official sites.
3. Install all other AppStore apps that you'll need.
4. In OSX, open terminal and change default shell to `/bin/bash`.
5. Once XCode is installed run `xcode-select --install`.
6. Copy files from the backup HD.
7. Clone this repo into `~/work`.
8. Copy SSH keys.
9. [Import GPG keys](http://irtfweb.ifa.hawaii.edu/~lockhart/gpg/)
10. Build docker image to provision with ansible:
     ```
     docker build -t ansible_provisioner -f Dockerfile.ansible .
     ```
11. Create a container with this image and create a new ssh keypair.
    ```
    docker run -it --rm -v=$(pwd):/app --workdir=/app ansible_provisioner /bin/bash
    # ...
    keygen
    ```
12. Copy this key to the host `.ssh/authorized_keys`.
13. Run the ansible playbook:
    ```
    ansible-playbook --skip-tags=ubuntu -i remote provision.yml
    ```

## Ubuntu Remote provision

0. Make sure you copied all backup files that you'll need to the home directory, including SSH keys.
1. [Install
   Docker](https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository)
   and make sure it starts on boot.
2. Instal the SSH server in the Ubuntu host machine:
   ```
   sudo apt install -y ssh
   sudo systemctl enable --now ssh
   ```
3. Build docker image to provision with ansible:
     ```
     docker build -t ansible_provisioner -f Dockerfile.ansible .
     ```
4. Create a container with this image and create a new ssh keypair.
    ```
    docker run -it --rm -v=$(pwd):/app --workdir=/app ansible_provisioner /bin/bash
    # ...
    ssh-keygen
    ```
5. Copy this key to the host `.ssh/authorized_keys`.
6. Run the ansible playbook:
   ```
   ansible-playbook --skip-tags=osx -i remote provision.yml
   ```
7. Install the following Gnome extensions:
   * WinTile
   * Application Menu
8. Set the `numix` theme under settings.
9. Add the remapping keys script to the startup apps:
   * Name: remap keys
   * Command: `bash -c "xkbcomp /home/sborrazas/.xkbmap $DISPLAY"`
10. Change the local Wifi DNS to be `8.8.8.8,8.8.4.4`.
11. Run bleachbit cleanups (and continue to do so every couple of months).

### Notes

If the remote host has a password to connect through SSH you should also append
the following parameters to the `ansible-playbook` command: `--ask-become`.

## Post provision

* Import the terminal config file located in the Desktop and set it as the
  default profile.
* Go into iTunes, on the menu "Store", "Authorize this computer" so you're
  able to download the songs.
