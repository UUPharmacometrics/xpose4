## Changes

Changes in this version of xpose4 are:

* 

## Test environments
* local OS X (10.12.5) install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0 and development version
* win-builder (devel and release)

## R CMD check results
For macOS and ubuntu there were no ERRORs, WARNINGs or NOTEs.

For win-builder release and devel versions there was one note:

* checking CRAN incoming feasibility ... NOTE
    + Maintainer: 'Andrew C. Hooker <andrew.hooker@farmbio.uu.se>'
    + Possibly mis-spelled words in DESCRIPTION:
        + NONMEM (15:11)
        + covariate (16:49)

The first portion is just stating that I am the maintainer.  

The second portion does not recognize a software called "NONMEM" or 
a the word "covariate", both of which are spelled correctly.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of xpose4. 
All packages passed.
