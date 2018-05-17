<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/PublicHealthEngland/hcaidcs.svg?branch=master)](https://travis-ci.org/) [![codecov](https://codecov.io/gh/PublicHealthEngland/hcaidcs/branch/master/graph/badge.svg)](https://codecov.io/gh/PublicHealthEngland/hcaidcs)

This is a collection of functions to make working with data from the [HCAI DCS](https://hcaidcs.phe.org.uk/) easier.

Installation
------------

The quickest way to install the package is to use `install_git` from the devtools package.

``` r
# install devtools if necessary
install.packages("devtools")

# then install the hcaidcs package
devtools::install_git('https://github.com/publichealthengland/hcaidcs.git')
```

Alternatively, one can download the zip file from github and do the following:

1.  Save the zip file to `H:\` but do not unzip
2.  Open RStudio
3.  Run the following line: `install.packages(file.choose(), repos=NULL)`
4.  This will launch a file chooser where you can select the zip file you have just downloaded.

This should install the package to the R package library.

If installation is successful you will then be able to load the package with `library(hcaidcs)`

Use
---

Functions that begin `aec_` are intended for producing tables or plots for the annual epidemiologic commentary.

Functions beginning `ann_tab_` are intended for the production of the annual tables.

Functions prefixed `nice_` indicate that they format values for nice printing in text, such as financial year, or an estimate with its' 95% confidence interval.

Functions prefixed `kh03_` indicate functions for use in the preparation of the kh03 denominator data.

Functions prefixed `mf_` indicate functions for use in the preparation of the HCAI monthly factsheet for the department of health.

Copyright notice
----------------

This package was developed at Public Health England and caries the Open Government Licence as a result. However, it also includes geographic data from Office for National Statistics (ONS) and so the following copyright statement also applies. Contains National Statistics data © Crown copyright and database right 2018

Contains OS data © Crown copyright and database right 2018

Contributions
-------------

Contributions to this package are welcome. Please see the [Contribution guidelines](http://bioinformatics-git.phe.gov.uk/Simon.Thelwall/hcaidcs/blob/master/CONTRIBUTING.md).
