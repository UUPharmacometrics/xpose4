## Test environments

* Local: macOS-latest, R release 
* GitHub Actions:
  - macOS-latest, R release
  - Windows-latest, R release 
  - Ubuntu-latest, R release, devel and oldrel-1 
* win-builder: R release and devel 

## R CMD check results

* There were no ERRORs, WARNINGs or NOTEs for local and GitHub environments. 

* For win-builder there was 1 NOTE:

  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Andrew C. Hooker <andrew.hooker@farmaci.uu.se>'

  Found the following (possibly) invalid DOIs:
    DOI: 10.1038/psp.2013.24
      From: DESCRIPTION
            inst/CITATION
      Status: Service Unavailable
      Message: 503
  
  - I can verify that the DOI is valid and works 
    when copying and pasting into Google
  
## Downstream dependencies

I have also run R CMD check on downstream dependencies of xpose4. 
No ERRORs or WARNINGs were found.

## Reason for submission

I got an email from Kurt Hornik:

  Please see the problems shown on
  <https://cran.r-project.org/web/checks/check_results_xpose4.html>.
  Specifically, please see the NOTEs for the "Rd files" check.
  In Rd \describe, \arguments and \value the item entries are of the form
  \item{LABEL}{DESCRIPTION}
  with a *non-empty* label: please modify your Rd files accordingly.
  Please correct before 2022-05-30 to safely retain your package on CRAN.

This issue has been fixed. Other small updates have also occurred.

