# Requirements
- Java interpreter (for RWeka)
  - for Ubuntu: `sudo apt-get install openjdk-7-*`
  - then `sudo R CMD javareconf`
  - Source: [https://stackoverflow.com/questions/16438073/unable-to-install-rjava-in-r-3-0-in-ubuntu-13-04](https://stackoverflow.com/questions/16438073/unable-to-install-rjava-in-r-3-0-in-ubuntu-13-04)

# Installation Ubuntu Single User Server
```
sudo apt-get update
sudo apt-get upgrade
sudo apt-get r-base
apt-get install libcurl4-gnutls-dev
sudo apt-get install openjdk-7-*
sudo R CMD javareconf
# Now we have all requirements; launch R
sudo R
install.packages('devtools')
devtools::install_github('Powernap/regression-cube-r-package')
install.packages('opencpu')
# Launch OpenCPU on Port 5003
opencpu::opencpu$start(5003)
```

## Create a fixed IP
Edit `/etc/network/interfaces` ([http://wiki.ubuntuusers.de/interfaces](http://wiki.ubuntuusers.de/interfaces))

### Example:
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

# Installation
- install devtools - `install.packages('devtools')`
  - Under Ubuntu, devtools might need an additional package for `RCurl`: `apt-get install libcurl4-gnutls-dev`
- install this package from github - `install_github('Powernap/regression-cube-r-package')`

**This repository is licensed under [Attribution-NonCommercial-NoDerivatives 4.0 International](https://creativecommons.org/licenses/by-nc-nd/4.0/)** (see LICENCE.md)
