## Test environments

* Local: macOS-latest, R release 
* GitHub Actions:
  - macOS-latest, R release
  - Windows-latest, R release 
  - Ubuntu-latest, R release, devel and oldrel-1 
* win-builder: R devel 

## R CMD check results

* There were no ERRORs, WARNINGs or NOTEs for all test environments. 
  
## Downstream dependencies - revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Reason for submission

I got an email from Kurt Hornik:

   Dear maintainer,

   Please see the problems shown on CRAN

   Specifically, please see the NOTEs about Rd file(s) with Rd \link{}
   targets missing package anchors in the "Rd cross-references" check.


These issues have been fixed. Other small updates have also occurred.

Please see the NEWS file for details.