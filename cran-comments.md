## Test environments
* local macOS (10.15.3) install, R release version
* Ubuntu 16.04.6 LTS (on travis-ci) R release and devel versions
* Windows Server 2012 R2 x64 (on AppVeyor), R release version
* win-builder, R devel and release versions
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub: Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. Except those noted below.

* For win-builder release and devel, and
  "Windows Server 2008 R2 SP1, R-devel, 32/64 bit" 
  there was 1 note:

  Maintainer: 'Andrew C. Hooker <andrew.hooker@farmbio.uu.se>'

  Possibly mis-spelled words in DESCRIPTION:
    Jonsson (17:60)
    Keizer (17:8)
    al (17:18, 17:71)
    et (17:15, 17:68)
  
  - The first is my name
  - The second is a reference to papers on our methods as requested by CRAN.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of xpose4. 
No ERRORs or WARNINGs were found.


## Reason for submission
A number of platforms on CRAN check were giving warnings about 'Documented arguments not in
\usage' in the r-devel checks.  These are from a recent bug fix
(PR#16223, see
<https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16223>)

* This has been fixed in the current version by removing the
documentation for argument '...'.


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

* Please consider to omit "Tools for" from the title.

* Please omit "A collection of functions to be used" from the Description 
  field.
  
These have been fixed in the current resubmission.

