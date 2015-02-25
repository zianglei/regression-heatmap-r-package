# Requirements
- Java interpreter (for RWeka)
  - for Ubuntu: `sudo apt-get install openjdk-7-*`
  - then `sudo R CMD javareconf`
  - Source: [https://stackoverflow.com/questions/16438073/unable-to-install-rjava-in-r-3-0-in-ubuntu-13-04](https://stackoverflow.com/questions/16438073/unable-to-install-rjava-in-r-3-0-in-ubuntu-13-04)

# Installation
- install devtools (`install.packages('devtools')`)
- install required packages (`install.packages(c('rms', 'rjson'))`)
- install this package from github (`install_github('Powernap/regression-cube-r-package')`)

**This repository is licensed under [Attribution-NonCommercial-NoDerivatives 4.0 International](https://creativecommons.org/licenses/by-nc-nd/4.0/)** (see LICENCE.md)
