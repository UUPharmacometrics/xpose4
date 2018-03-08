## Test environments
1. local macOS (10.13.3) install, R 3.4.3
2. ubuntu 14.04.5 LTS (on travis-ci) R 3.4.3 and development version
3. win-builder (devel and release)

## R CMD check results

### 1. macOS
For macOS there was a WARNING when building the tar file:

* building ‘xpose4_4.6.1.tar.gz’ 
Warning: invalid uid value replaced by that for user 'nobody'
Warning: invalid gid value replaced by that for user 'nobody'

Which seems to be because my uid on macOS is too large.  
I am not sure how to change my uid without messing lots of other things up.
I assume this WARNING can be ignored.

From R CMD check:

R CMD check results
0 errors | 0 warnings | 0 notes
R CMD check succeeded

### 2. ubuntu
For ubuntu there were no ERRORs, WARNINGs or NOTEs.

### 3. win-builder
For win-builder there were no ERRORs, WARNINGs or NOTEs on the 
development branch.

For the current release version there was one note:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Andrew C. Hooker <andrew.hooker@farmbio.uu.se>'

Possibly mis-spelled words in DESCRIPTION:
  NONMEM (15:11)

* The first portion is just stating my name
* The second portion is worried about the word NONMEM
  which is a computer program: https://en.wikipedia.org/wiki/NONMEM.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of xpose4. 
No ERRORs or WARNINGs were found.
