# Changes in xpose4 version 4.6.1

  * Updates to comply with changes in the readr and gam packages.

# Changes in xpose4 version 4.6.0

  * Update to xpose.VPC() so that outliers can be identified and plotted.
  
  * Update to xpose.VPC() so that for any lines in the VPC they are plotted at the 
    median of the observed values of the X axis for all bins by default.
    
  * Update to xpose.VPC() to allow a rug at the bottom of the plot showing where the 
    bins are located.
    
  * Update to the namespace so that only lattice is loaded when loading xpose.
  
  * All documentation now written in roxygen
  
  * Updates to boot GAM and boot SCM plots and documentation.
  
  * Various small bug fixes.

# Changes in xpose4 version 4.5.3

  * Update to ind.plots() to allow subsets on a per-y-variable basis.  
    Useful to show IPRED and PRED in a finer grid than DV. See option
    “y.vals.subset”.

  * Update to how axes limits are computed with xpose.plot.default.

  * Fix for using expression() in the ylb argument of xpose.VPC.

  * Various small bug fixes.

# Changes in xpose4 version 4.5.2

  * Internal release  

  * Updates to read.bootscm.par.est()

# Changes in xpose4 version 4.5.1

  * Internal release  

  * Updated xpose.gam to work with the latest version of the gam package 

  * Updated kaplan.plot to allow for ylim specification when "cov" 
    argument is used.

  * Updated compute.cwres and associated functions to work with NONMEM 7.

  * Fixed warnings created in xpose.VPC.categorical when creating personalized 
    x and y axis labels.

# Changes in xpose4 version 4.5.0

  * External release with just one package instead of five.
  
  * Added to ind.plots so that when there is only one data point for an
    individual, the PRED and IPRED show up in the plot.

# Changes in xpose4 version 4.4.3

  * Internal release  
  
  * Added functionality for plotting the delta mean output from the
    vpc tool in PsN.  Option is for xpose.VPC() and can be turned on
    using PI.delta.mean=T.  See ?xpose.panel.default for more
    information.  

# Changes in xpose4 version 4.4.2 

  * Internal release

  * Removed some default messages that were print to the screen when
    running xpose.VPC().  can change back to previous behavior with
    option verbose=TRUE.

  * Combined the five packages of xpose into one package.

  * Updated the Histogram functionality.

  * New plots randtest.hist() and boot.hist() for creating histograms
    of results from PsN's 'randtest' and 'bootstrap' tools.

  * Updated the xpose.VPC() function to handle plotting of mean values
    from simulations.  

# Changes in xpose4 version 4.4.1

  * Updates to kaplan.plot.R (thanks to Leonid Gibiansky for reporting the problems)

  * kaplan.plot.R: Removed debugging command that was mistakenly left in function

  * kaplan.plot.R: "ylab" argument is now passed to the plot when cov option is used.

  * kaplan.plot.R: Using the cov option with repeated censoring and no observations 
    to break up the chain then the mean value calculation was wrong (only used the
    surviving IDs and the last censored ID). Fixed now. 

  * Changed the "aspect" argument for all plots to default to "fill".  
    Previously it was "1".  

# Changes in xpose4 version 4.4.0

  * Added bootstrap of the GAM and diagnostics for the boostrap of the
    PsN function boot_scm. 

# Changes in xpose4 version 4.3.6
  
  * fixed the plot in the classic menu system "Weighted residuals vs
    covariates". 

# Changes in xpose4 version 4.3.5

  * Updated help files with workable examples, and an example
    dataset.  Look at data(simpraz.xpdb), simprazExample() and
    example(xpose.data) for dataset examples and example(basic.gof)
    and example(cwres.vs.idv) for plot examples. Most of the
    xpose4specific functions now have examples that can be run with
    example().

  * Updated kaplan.plot() so that the kaplan-Meier mean covariate
    (KMMC) plot can be created. Also added more options for adjusting
    plot properties.

  * New gofSetup() command to create your own customized series of GOF plots. 

  * fixed how RSE values were reported on runsum() when the parameter
    was fixed.

  * Fixed argument to xpose.VPC.categorical(max.plots.per.page=1), so
    that only one plot per page was possible. 

  * Fixed xpose.VPC() so that the psn option for vpc
    "confidence_interval=X" works.  

  * Fixed compute.cwres() function that wasn't computing anything (and
    returning an error).


# Changes in xpose4 version 4.3.2

  * Fixed bug in xpose.VPC when asking for logx=T (didn't work
    previously).

  * Fixed dOFV.vs.id for when there are ties in individual dOFV drops.

# Changes in xpose4 version 4.3.0
  * Updated read.nm.tables so that comma separated NONMEM 7 files
    can be read into Xpose.

  * Changing the behavior of xpose.multiple.plot.default. Now
    multiple plots are returned as objects just like single plots are
    (no automatic printing from the function that created the plot
    list). This is accomplished by defining a new class -
    xpose.multiple.plots - and corresponding print and show methods for
    that class. 

  * Updated xpose.VPC, xpose.VPC.categorical and xpose.VPC.both to
    handle the new format of PsN vpc_results.csv
    files. 

  * xpose.VPC.categorical now has a new option: censored (T or F)
    which will create BLOQ VPC plots when TRUE.  

  * xpose.VPC.both tries to combine continuous and categorical BLOQ
    plots.

  * page numbers can be turned off in multiple page plots using the
    page.numbers option (T or F).

# Changes in xpose4 version 4.1.0
  * Updated ind.plots(), the function is much more flexible now.
  * Added graphical options to xpose.VPC.categorical()
  * Fixed logy=T option for xpose.VPC(Pi.ci=T,logy=T).
  * Fixed logy=T and logx=T option (bug resulting in error).
  * VPC changed to require that y-axis be continuous as default.
  * Fixed classic version of parm.vs.parm() plot.
  * Fixed runsum(). Previous version had a line between each line in
    model file.
  * Added a new function change.xvardef(), which replaces much of the
    previous change functions. Thanks to Sebastien Bihorel for 
    input which helped create this function.
  * Added the ability to apply functions to the x-axis of plots. The
    function options are now called funx and funy.
  * Added support for reading NONMEM 7 table and output files. 
  * Added functions for odd type (categorical, TTE, count) plots including
    VPCs.
  * Updated handling of PsN vpc output file
  * Updated interpretation of categories in xpose.VPC.categorical()

# Changes in xpose4 version 4.0.4 
  * cwres.vs.pred.bw() was fixed. Previously cwres.vs.pred.bw() gave the
    same result as cwres.vs.idv.bw(). 
  * Fixed xpose.VPC() bug causing plots to not be created in some
    situations.  
  * Added functionality to xpose.VPC() so that users can define their
    own titles for each subplot if stratification is used in the VPC.
    see ?xpose.VPC for more info.
  * Updated method for opening graphical devices in windows to be
    consistent with the new methods used in R version 2.8.0. 
  * Added functionality to allow the user to plot vertical and
    horizontal lines in histograms.  See ?xpose.panel.histogram for more
    information. 
  * Fixed small bug with xpose.panel.splom(). 

# Changes in xpose4 version 4.0.3
  * in compute.cwres() a debugging flag had been left in the file
    resulting in R going into debugging mode when this function was
    called. This has been fixed. 

# Changes in xpose4 version 4.0.2
  * Added ability to smooth the PI.ci "area" plots so that they match
    the "line" plots.  See 'PI.ci.area.smooth' in
    xpose.panel.default()
  * Added 'logx' and 'logy' functionality to the PI plots.
  * Changed the par.summary and cov.summary routines and removed
    functions doing almost the same thing (adding that functionality
    to the current functions). 
  * fixed GAM plot problems in xp.plot() and added more support for
    GAM from the command line.
  * Fixed a problem with ind.plots() when the ID variable is not
    called ID.
  * Changed all functions in xpose4specific that began with "abs." to
    begin with "absval." to be consistent with the rules for generic
    function definitions in R.
  * Changed name of add.abs() to add.absval().
  * Changed name of par.summary() to parm.summary().   
  * Changed name of param.vs.cov() to parm.vs.cov().
  * Changed name of param.vs.param() to parm.vs.parm().


# Changes in xpose4 version 4.0.1
  * Added functionality for visual predictive checks
  * Added functionality for numerical predictive checks

# Changes in xpose4 version 4.0.0.3.7 
  * Added generic functions xpose.draw.table, xpose.draw.cell, xpose.get.c
      and xpose.get.r for drawing tables using the graphics device (JW)
  * Added specific function param.table to display parameter estimates 
      using the graphics device (e.g. in a PDF file) (JW)
  * Added additional specific functions for:
  * Added additional specific functions: IWRES distribution (histogram) (iwres.dist.hist)
  * Added additional specific functions: IWRES distribution (QQ) (iwres.dist.qq)
  * Added additional specific functions: ETA distribution (histogram) (ranpar.dist.hist)
  * Added additional specific functions: ETA scatter-plot matrices (ranpar.splom)
  * Added additional specific functions: ETAs vs covariates (ranpar.vs.cov)
  * Added additional specific functions: Parameter tables on the graphics device   
    (param.table)
  * Updated compute.cwres function so that it would work without xpose 4
      Just 'source' the file (compute.cwres.R) and it should work (AH)
  * fixed problems with the run summary function (AH)
  * added new general class of printing multiple plot objects on the
    same page (AH) 
  * Fixed bug with plotting results of GAM (AH)

# Changes in xpose4 version 4.0.0.3.5 
  * Bugs in 'groups' argument fixed in xpose.plot.default,
      dv.vs.pred.ipred, dv.preds.vs.idv (multiple values of x or
      y not properly handled) (JW)
  * File devices (e.g. pdf, postscript, etc) now work correctly in
      all functions (JW)
  * Bug in multiple-page covariate plots fixed (only the first page 
      would display) (JW)
  * Bug in reading table files could sometimes leave file debris, 
      which could interfere with reading subsequent data - fixed (JW)
  * Bug in covariate checking could sometimes cause plot functions 
      to fail (e.g. abs.wres.vs.pred.by.cov) - fixed (JW)
  * Bug in the classic menu system prevented display of some plots -
      fixed (JW)
  * Bug in the classic menu system prevented display of some plots -
      fixed (JW)
  * Bug in CWRES calculation fixed (AH)
  * Bug in parameter histogram display fixed (JW)
  * Missing values (defaults to -99) now handled correctly (JW)
  * QQ plots no longer display categorical variables (JW)

# Changes in xpose4 version 4.0.0.3.3 
  * Bug in 'subset' argument to individual plots corrected (JW)

# Changes in xpose4 version 4.0.0.3.2 
  * Online documentation cleaned up (JW) 
  * Numerous small bugs fixed (JW)
  * *nix support added (JW)
  * Multipage plots now create stacks of display windows, rather than
      stacks of plots in a single window (JW)
  * Scatter-plot matrices added (JW)
  * QQ plots for parameters and covariates added (JW)
  * Generic functions renamed for consistency (JW)
  
# Changes in xpose4 version 4.0.0.3.1 
  * Bugs in CWRES application and documentation fixed (AH)
  * Bugs in histogram functions fixed
      - lack of defined covariates no longer causes crash
      - customization options now work

# Changes in xpose4 version 4.0.0.3 
  * GAM added (AH)
  * CWRES plots and functions added (AH)
  * gam package now required
  * Known bugs corrected

# Changes in xpose4 version 4.0.0.2
  * SUBSET functionality fixed for all procedures
  * Preferences, summaries and data checkout implemented
  * Box and whisker plots now do what their preferences tell them to
  * 'label' function renamed to 'xlabel' for compatibility
  * Hmisc package now required
  * Many small additions and tweaks
  * R package functionality fixed

# Changes in xpose4 version 4.0.0.2.1 
  * Ind.plots.R updated (AH)

# Changes in xpose4 version 4.0.0.1 
  * Xpose 4 is a completely rewritten version of Xpose 3.1, and so
    everything has changed. 

