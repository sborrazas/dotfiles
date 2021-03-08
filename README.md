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
* Run `ansible-playbook -v --ask-become-pass --skip-tags=osx -i local provision.yml` in this directory.

## Remote provision

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
     docker build -t ansible_provisioner -f Dockerfile.ansible
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

### Notes

If the remote host has a password to connect through SSH you should also append
the following parameters to the `ansible-playbook` command: `--ask-become`.

## Post provision

* Import the terminal config file located in the Desktop and set it as the
  default profile.
* Go into iTunes, on the menu "Store", "Authorize this computer" so you're
  able to download the songs.
