## Test environments
* local macOS (10.15.7) install, R release version
* Ubuntu 16.04.6 LTS (on travis-ci) R release and devel versions
* Windows Server 2012 R2 x64 (on AppVeyor), R release version
* win-builder, R devel and release versions

## R CMD check results

There were no ERRORs or WARNINGs. 

* For win-builder there was 1 NOTE:

  Maintainer: 'Andrew C. Hooker <andrew.hooker@farmaci.uu.se>'

  Possibly mis-spelled words in DESCRIPTION:
    Jonsson (17:60)
    Keizer (17:8)
    al (17:18, 17:71)
    et (17:15, 17:68)
  
  - The first is my name
  - The second is a reference to papers on our methods as requested by CRAN.
  
* win-builder release
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Andrew C. Hooker <andrew.hooker@farmaci.uu.se>'

New submission

Package was archived on CRAN

Version contains large components (4.7.0.9001)

Possibly mis-spelled words in DESCRIPTION:
  Jonsson (17:60)
  Keizer (17:8)
  NONMEM (14:39)
  al (17:18, 17:71)
  et (17:15, 17:68)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-12-10 as check problems were not
    corrected in time.
    
* win builder devel

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Andrew C. Hooker <andrew.hooker@farmaci.uu.se>'

New submission

Package was archived on CRAN

Version contains large components (4.7.0.9001)

Possibly mis-spelled words in DESCRIPTION:
  Jonsson (17:60)
  Keizer (17:8)
  NONMEM (14:39)
  al (17:18, 17:71)
  et (17:15, 17:68)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-12-10 as check problems were not
    corrected in time.

Found the following (possibly) invalid URLs:
  URL: http://link.springer.com/article/10.1007%2Fs11095-007-9361-x (moved to https://link.springer.com/article/10.1007/s11095-007-9361-x)
    From: man/compute.cwres.Rd
    Status: 200
    Message: OK
  URL: http://xpose.sf.net (moved to http://xpose.sourceforge.net/)
    From: README.md
    Status: 200
    Message: OK

** running examples for arch 'i386' ... [11s] ERROR
Running examples in 'xpose4-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: cwres_wres_vs_x
> ### Title: Weighted residuals (WRES) and conditional WRES (CWRES) plotted
> ###   against the independent variable (IDV) or the population predictions
> ###   (PRED) for Xpose 4
> ### Aliases: cwres_wres_vs_x cwres.wres.vs.idv cwres.wres.vs.pred
> 
> ### ** Examples
> 
> ## Here we load the example xpose database 
> xpdb <- simpraz.xpdb
> 
> cwres.wres.vs.idv(xpdb)
Error in get(y) : first argument has length > 1
Calls: cwres.wres.vs.idv ... xpose.plot.default -> subset -> subset.data.frame -> eval -> eval -> get
Execution halted
** running examples for arch 'x64' ... [12s] ERROR
Running examples in 'xpose4-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: cwres_wres_vs_x
> ### Title: Weighted residuals (WRES) and conditional WRES (CWRES) plotted
> ###   against the independent variable (IDV) or the population predictions
> ###   (PRED) for Xpose 4
> ### Aliases: cwres_wres_vs_x cwres.wres.vs.idv cwres.wres.vs.pred
> 
> ### ** Examples
> 
> ## Here we load the example xpose database 
> xpdb <- simpraz.xpdb
> 
> cwres.wres.vs.idv(xpdb)
Error in get(y) : first argument has length > 1
Calls: cwres.wres.vs.idv ... xpose.plot.default -> subset -> subset.data.frame -> eval -> eval -> get
Execution halted
* checking for unstated dependencies in 'tests' ... OK
* checking tests ...
** running tests for arch 'i386' ... [2s] OK
  Running 'testthat.R' [2s]
** running tests for arch 'x64' ... [2s] OK
  Running 'testthat.R' [2s]
* checking PDF version of manual ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 2 ERRORs, 1 NOTE

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

