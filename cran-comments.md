## Test environments

* Local: macOS (12.4), R release 
* GitHub Actions:
  - Windows-latest, R release 
  - macOS-latest, R release 
  - Ubuntu-latest, R release, devel and olrel-1 
* win-builder: R release and devel 

## R CMD check results

There were no ERRORs or WARNINGs. 

* For win-builder there was 1 NOTE:

    > Maintainer: 'Andrew C. Hooker <andrew.hooker@farmaci.uu.se>'
    
    > New submission
    
    > Package was archived on CRAN
    
    > Possibly mis-spelled words in DESCRIPTION:
    >   Jonsson (17:60)
    >   Keizer (17:8)
    >   NONMEM (14:39)
    >   al (17:18, 17:71)
    >   et (17:15, 17:68)
    
    > CRAN repository db overrides:
    >   X-CRAN-Comment: Archived on 2020-12-10 as check problems were not
        corrected in time."

  
  - My name hasn't changed but my email address has changed.
  - I missed the deadline for updating the package set by CRAN.
  - The mis-spelled words come from referencing papers on our methods 
    as requested by CRAN.
  
* For devel R version on Travis:

    > * checking for future file timestamps ... NOTE
    > unable to verify current time (17:15, 17:68)

  - This seems do happen sporadically and seems
    like a spurious problem with the testing system.
  
  
## Downstream dependencies

I have also run R CMD check on downstream dependencies of xpose4. 
No ERRORs or WARNINGs were found.

## Reason for submission

This is an updated version of xpose4. In the previous version, 
a number of platforms on CRAN check for devel versions of R 
were giving warnings/errors about some examples that were being run in
the documentation.  The bug in the code causing these problems 
has been fixed.

