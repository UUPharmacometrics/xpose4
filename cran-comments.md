## Test environments
* local macOS (10.15.3) install, R release version
* Ubuntu 16.04.6 LTS (on travis-ci) R release and devel version
* Windows Server 2012 R2 x64 (on AppVeyor), R release version
* win-builder, R release version

## R CMD check results
For macOS, ubuntu, windows server and win-builder (release) there were no ERRORs, WARNINGs or NOTEs. 

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
