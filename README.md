# Installation Ubuntu Single User Server
```
sudo apt-get update
sudo apt-get upgrade
sudo apt-get r-base
apt-get install libcurl4-gnutls-dev
sudo apt-get install openjdk-7-*
# https://stackoverflow.com/questions/16438073/unable-to-install-rjava-in-r-3-0-in-ubuntu-13-04
sudo R CMD javareconf
sudo apt-get libssl-dev
# Now we have all requirements; launch R
sudo R
install.packages('devtools')
devtools::install_github('paulklemm/regression-heatmap-r-package')
install.packages('opencpu')
# Launch OpenCPU on Port 5003
opencpu::opencpu$start(5003)
```

For large computations it might be necessary to replace increase the timeout (usually set to 90 seconds) under `~/.opencpu.conf` (`"timelimit.post": 90`).

## Create a fixed IP
Edit `/etc/network/interfaces` ([http://wiki.ubuntuusers.de/interfaces](http://wiki.ubuntuusers.de/interfaces))

### Example
```
# This file describes the network interfaces available on your system
# and how to activate them. For more information, see interfaces(5).

# The loopback network interface
auto lo
iface lo inet loopback

# The primary network interface
auto eth0
# iface eth0 inet dhcp

iface eth0 inet static
address 172.27.212.10
netmask 255.255.0.0
network 172.27.0.0
broadcast 172.27.0.255
gateway 172.27.100.100
dns-nameservers 172.27.0.12 141.44.23.23
dns-search isg.intern
```

## Optional: Install `SSH` and `VNC`

- `sudo apt-get install openssh-server`
- `sudo apt-get install x11vnc`

Old keys for the machine in the ssh client need to be replaced by running `ssh-keygen -R hostname ` and replace the keys under `~/.ssh/known_host`.

I had trouble running x11vnc under Gnome, so I've installed XFCE through `sudo apt-get install xubuntu-desktop gksu leafpad synaptic` and chose the `xfce` session at login (through the ubuntu logo).

**This repository is licensed under [Attribution-NonCommercial-NoDerivatives 4.0 International](https://creativecommons.org/licenses/by-nc-nd/4.0/)** (see LICENCE.md)

I will change the licence to [http://choosealicense.com/licenses/mit/](MIT) as soon as the associated paper gets accepted.

## Workaround for Java Loading Error

`rJava` and `OpenCPU` currently [don't play nice together](https://github.com/jeroenooms/opencpu/issues/85). We start `OpenCPU` in single user mode to bypass this problem. To do this, simply create a `R` file starting `OpenCPU` on the correct port and then adding a `CRON` job for it, so it is automatically loaded on startup.

### Create the Script

```
touch startOpenCPU.R
nano startOpenCPU.R
  # Add following code without the `#`
  # opencpu::opencpu$start(5003)
  # opencpu::opencpu$stop()
  # opencpu::opencpu$start(5003)
sudo crontab -e
  # Add `sudo R CMD BATCH [path-to-script]/startOpenCPU.R`
```

`OpenCPU` should start in single user mode on startup now. If its not, look into the log for troubleshooting using `grep CRON /var/log/syslog`.

*Note that this is a quick-fix until the issue is resolved!*
