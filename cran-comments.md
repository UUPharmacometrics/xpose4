## Test environments
* local macOS (10.15.3) install, R release version
* Ubuntu 16.04.6 LTS (on travis-ci) R release and devel version
* Windows Server 2012 R2 x64 (on AppVeyor), R release version
* win-builder, R release version
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub: Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of xpose4. 
No ERRORs or WARNINGs were found.

## This is a resubmission
In the previous submission (2020-02-15), the following problems were found:

* Found the following (possibly) invalid URLs:
     URL: http://www.r-project.org
       From: README.md
       Status: 200
       Message: OK
       R-project URL not in canonical form
     Canonical www.R-project.org URLs use https.

* Is there some reference about the method you can add in the Description 
  field in the form Authors (year) <doi:.....>?

* Please soncider to omit "Tools for" from the title.

* Please omit "A collection of functions to be used" from the Description 
  field.
  
These have been fixed in the current resubmission.

