## Test environments
1. local macOS (10.13.3) install, R 3.4.3
2. ubuntu 14.04.5 LTS (on travis-ci) R 3.4.3 and development version
3. win-builder (devel and release)

## R CMD check results

### 1. macOS
For macOS there was a warning when building the tar file:

* building ‘xpose4_4.6.0.9002.tar.gz’ 
Warning: invalid uid value replaced by that for user 'nobody'
Warning: invalid gid value replaced by that for user 'nobody'

Which seems to be because my UID on macOS is too large.  
I am not sure how to change my UID without messing lots of other things up.
I assume this Warning can be ignored.

Otherwise we have:

R CMD check results
0 errors | 0 warnings | 0 notes
R CMD check succeeded

### 2. ubuntu
For ubuntu there were no ERRORs, WARNINGs or NOTEs.

### 3. win-builder
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
No ERRORs or WARNINGs were found.
