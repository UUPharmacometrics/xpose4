Xpose 4
====================

by Andrew C. Hooker, Justin J. Wilkins, Mats O. Karlsson 
and E. Niclas Jonsson

http://xpose.sourceforge.net/

## Introduction

Xpose 4 is a ground-floor rewrite of Xpose 3.1. Chief amongst the enhancements
in the new version is the migration from S-PLUS to R, a free,
multi-platform statistical environment.  We have also added direct access to Xpose
functions from the command line, and, through R, access to Xpose tools by
third-party applications.

## Installation

1. R: You need to have R installed.  Download the latest version of R from www.r-project.org.
2. The gam package: Xpose uses the gam package, install the latest version from CRAN.  Write at the command
line:
   ```
   install.packages("gam")
   ```
3. Install xpose in R using one of the following methods:
   * latest stable release -- From CRAN.  Write at the R command line:
     ```
     install.packages("xpose4")
     ```
   * Latest development version -- from Github. Note that the command below installs the "master" 
(development) branch; if you want the release branch from Github add `ref="release"` to the
`install_github()` call. The `install_github()` approach requires that you build from source, 
i.e. `make` and compilers must be installed on your system -- see the R FAQ for your operating system; 
you may also need to install dependencies manually.
     ```
     devtools::install_github("xpose4",username="andrewhooker")
     ```


R and required package installation
-----------------------------------
To install you will need:

* R version >=2.2.0
* the gam package

All of these are freely available from the Comprehensive R-Project
Archive Network (http://cran.r-project.org). To install Hmisc and gam,
you have a number of options.

  Option 1: The GUI 
  ---------------------------
  Start the R GUI, and then select "Packages" -> "Install Package(s)..."
  from the menus at the top of the screen. You will be asked for a 
  mirror - select the one that seems closest to you, geographically,
  and click OK. Select "Hmisc" from the list, and click OK. Repeat all
  steps for "gam". 

  Option 2: The R console
  -----------------------
  Start R. Enter
  
  > install.packages("Hmisc")
  > install.packages("gam")

  and the Hmisc and gam packages will be downloaded and installed from the
  default mirror.

  Type ?install.packages in R for more details.
 

Xpose 4 Installation
--------------------
Download the correct archive for your operating system

  * For Windows users xpose4_VERSION.NUMBER_win32.zip
  * For Linux users xpose4_VERSION.NUMBER.tar.gz

where VERSION.NUMBER should be replaced by the most recent version of
xposed released. Once you have downloaded the archive, you will need to
install the Xpose package bundle. 

  Option 1: The GUI (Windows)
  ---------------------------
  Install from R, by using the GUI. Choose the menu item "Packages" ->
  "Install packages from local zip files..." and select the zip file
  you have downloaded (e.g. xpose4_4.0.1_win32.zip).

  Option 2: The R console (Linux & Windows)
  -----------------------------------------
  Start R. Enter
  
  > install.packages("C:/temp/xpose4_4.0.1_win32.zip",repos=NULL)

  if you are using Windows, or

  > install.packages("/tmp/xpose4_4.0.1.tar.gz",repos=NULL)

  if you are using Linux. Note that this assumes that you have stored
  your downloaded files in "C:\temp" or "/tmp/". Substitute as 
  needed.

NOTE: for windows, ensure you have downloaded the BINARY
distribution. If you have the source code release
(e.g. xpose4_4.0.1_src.zip), you will need to either (a) build the
packages from source code, as below, or download the correct file and
start again.  

* the Hmisc package 


Building Xpose 4 from source
----------------------------
For Windows users, you will need to download the source distribution
(e.g. xpose4_4.0.1_src.zip) from the website. In addition you will
need to download some package building toolsets from the web. We
direct you to this rather useful tutorial on building R packages under
Windows: 

http://www.maths.bris.ac.uk/~maman/computerstuff/Rhelp/Rpackages.html

This description is a bit outdated but useful as well

http://faculty.chicagogsb.edu/peter.rossi/research/bayes%20book/bayesm/Making%20R%20Packages%20Under%20Windows.pdf 

In addition the R help manuals give lots of information:

http://cran.r-project.org/doc/manuals/R-admin.html#The-Windows-toolset
http://cran.r-project.org/doc/manuals/R-exts.html

Your task under Linux should be much simpler. All the tools you need ought
to be available already in almost all distributions. Simply follow the
instructions (option 2) above.  The *.tar.gz release version of xpose
4 is a compressed source distribution that can be built using the
command above.  For more help on building R packages see:

http://cran.r-project.org/doc/manuals/R-exts.html


Running Xpose 4
---------------
* Start R
* At the R command prompt, type:
  
  library(xpose4) <ENTER>
  xpose4() <ENTER>

  and use the menu system!

Each function in the bundle is now independently available from the command
line, once libraries are loaded. For example, assuming your run is called
'run5.mod', you might do the following:

* Import data

  xpdb5 <- xpose.data(5)

* Display goodness-of-fit plots

  basic.gof(xpdb5)

More help is available in the online documentation, which can be found by
typing (for example) ?basic.gof at the R command line.  Using the GUI
(Windows): From the menu system in R choose 'Help' -> 'Html help' ->
'Search Engine & Keywords' and search for "xpose4".   


Don't Panic
-----------
Andrew Hooker (andrew.hooker@farmbio.uu.se)
should be able to get you a quick answer if you run into trouble.  The
website http://xpose.sf.net  should also be of help.


Release Schedule
----------------
Bugfix releases will be released regularly, fixing any problems that are
found. 


License
-------
Xpose 4 is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program (available in \<RHOME>\share\licenses).  
If not, see <http://www.gnu.org/licenses/>.


Known Bugs
----------------------
* None at present, but there will certainly be a few


