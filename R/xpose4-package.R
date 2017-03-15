

#' Model comparison plots, of absolute differences in goodness-of-fit
#' predictors against covariates, for Xpose 4
#' 
#' These functions plot absolute differences in PRED, IPRED, WRES, CWRES and
#' IWRES against covariates for the two specified model fits.
#' 
#' These functions produce plots of the absolute difference in population
#' predictions (\code{absval.dpred.vs.cov.model.comp}), individual predictions
#' (\code{absval.dipred.vs.cov.model.comp}), weighted population residuals
#' (\code{absval.dwres.vs.cov.model.comp}), conditional weighted population
#' residuals (\code{absval.dcwres.vs.cov.model.comp}) and individual
#' predictions (\code{absval.diwres.vs.cov.model.comp}).
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' @aliases absval.dcwres.vs.cov.model.comp absval.dwres.vs.cov.model.comp
#' absval.diwres.vs.cov.model.comp absval.dpred.vs.cov.model.comp
#' absval.dipred.vs.cov.model.comp
#' @param object An xpose.data object.
#' @param object.ref An xpose.data object. If not supplied, the user will be
#' prompted.
#' @param type 1-character string giving the type of plot desired.  The
#' following values are possible, for details, see 'plot': '"p"' for points,
#' '"l"' for lines, '"o"' for overplotted points and lines, '"b"', '"c"') for
#' (empty if '"c"') points joined by lines, '"s"' and '"S"' for stair steps and
#' '"h"' for histogram-like vertical lines.  Finally, '"n"' does not produce
#' any points or lines.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a stack of plots comprising comparisons of PRED, IPRED, WRES
#' (or CWRES) and IWRES for the two specified runs.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{compute.cwres}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for runs
#' ## 5 and 6 in the current working directory
#' xpdb5 <- xpose.data(5)
#' xpdb6 <- xpose.data(6)
#' 
#' ## A basic dWRES plot, without prompts
#' absval.dwres.vs.cov.model.comp(xpdb5, xpdb6)
#' 
#' ## Custom colours and symbols, no user IDs
#' absval.dpred.vs.cov.model.comp(xpdb5, xpdb6, cex=0.6, pch=8, col=1, ids=NULL)
#' }
#' 
#' 
NULL





#' Column-transformation functions for Xpose 4
#' 
#' These functions transform existing Xpose 4 data columns, adding new columns.
#' 
#' These functions may be used to create new data columns within the Xpose data
#' object by transforming existing ones. \code{add.absval} creates a column
#' reflecting the absolute value of a column, \code{add.dichot} creates a
#' categorical data column based upon a continuous variable, \code{add.exp}
#' creates an exponentiated version of an existing variable, \code{add.log}
#' provides log transformation, and \code{add.tad} creates a time-after-dose
#' (TAD) data item based upon the dose and time variables in the dataset.
#' 
#' @aliases add.absval add.dichot add.exp add.log add.tad
#' @param object An \code{xpose.data} object.
#' @param listall A logical operator specifying whether the items in the
#' database should be listed.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system.  This is an internal option and need never
#' be called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{xpose.data}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Create a column containing the absolute values of data in another 
#' ## column
#' add.absval(xpdb5)
#' 
#' ## Create a categorical data column based on a continuous data column, 
#' ## and do not list variables 
#' add.dichot(xpdb5, listall = FALSE)
#' 
#' ## Create a column containing the exponentiated values of data in 
#' ## another column
#' add.exp(xpdb5)
#' 
#' ## Create a column containing log-transformations of data in another 
#' ## column
#' add.log(xpdb5)
#' 
#' ## Create a time-after-dose column
#' add.tad(xpdb5)
#' }
#' 
NULL





#' Functions changing variable definitions in Xpose 4
#' 
#' These functions allow customization of Xpose's graphics settings.
#' 
#' These functions are used to customize graphics settings, the way Xpose draws
#' its graphs. \code{change.misc.graph.par} sets basic graphics parameters,
#' including plot type, point type and size, colour, line type, and line width.
#' \code{change.ab.graph.par} is used to change settings for the line of
#' identity, \code{change.lm.graph.par} is responsible for linear regression
#' lines, \code{change.smooth.graph.par} sets preferences for loess smooths,
#' \code{change.bw.graph.par} sets preferences for box-and-whisker plots, and
#' \code{change.label.par}, \code{change.dil.graph.par} and
#' \code{change.pi.graph.par} functions are responsible for labelling, dilution
#' and prediction interval plotting preferences, respectively.
#' \code{change.cond.graph.par} sets preferences for conditioning.
#' 
#' Settings can be saved and loaded using \code{\link{export.graph.par}} and
#' \code{\link{import.graph.par}}, respectively.
#' 
#' @aliases change.misc.graph.par change.ab.graph.par change.lm.graph.par
#' change.smooth.graph.par change.label.par change.bw.graph.par
#' change.dil.graph.par change.pi.graph.par change.cond.graph.par
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},\code{\link{xpose.panel.default}},
#' \code{\link{xpose.plot.bw}},\code{\link{xpose.panel.bw}},
#' \code{\link{xpose.plot.default}},\code{\link{import.graph.par}},
#' \code{\link{export.graph.par}},\code{\link{plot.default}},
#' \code{\link{par}},\code{\link{import.graph.par}},\code{\link[lattice]{panel.abline}},
#' \code{\link[lattice]{panel.lmline}},\code{\link{lm}},\code{\link[lattice]{panel.loess}},
#' \code{\link{loess.smooth}},\code{\link{loess}},\code{\link[lattice]{panel.bwplot}},
#' \code{\link[lattice]{shingle}},\code{reorder.factor}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Change default miscellaneous graphic preferences
#' xpdb5 <- change.misc.graph.par(xpdb5)
#' 
#' ## Change default linear regression line preferences, creating a new 
#' ## object
#' xpdb5.a <- change.lm.graph.par(xpdb5)
#' 
#' ## Change conditioning preferences
#' xpdb5 <- change.cond.graph.par(xpdb5)
#' }
#' 
NULL





#' Functions changing miscellaneous parameter settings in Xpose 4
#' 
#' These functions allow viewing and changing of settings relating to subsets,
#' categorical threshold values, documentation and numbers indicating missing
#' data values.
#' 
#' These functions are used to change settings for the number of unique data
#' values required in a variable in order to define it as continuous
#' (\code{change.cat.levels} and \code{change.dv.cat.levels} for ordinary
#' variables and the dependent variable, respectively), the value to use as
#' 'missing' (\code{change.miss}), and viewing (\code{get.doc}) and setting
#' (\code{set.doc}) the documentation field in the Xpose data object.
#' 
#' \code{change.cat.cont} allows interchange between categorical and continuous
#' data formats within the Xpose database. This in turn affects how plots are
#' drawn.
#' 
#' \code{change.subset} is used for setting the data item's subset field.  To
#' specify a subset of the data to process, you use the varable names and the
#' regular R selection operators. To combine a subset over two or more
#' variables, the selection expressions for the two variables are combined
#' using R's unary logical operators.
#' 
#' The variable names are those that are specified in the NONMEM table files
#' (e.g. PRED, TIME, SEX).
#' 
#' The selection operators are: == (equal) != (not equal) || (or) > (greater
#' than) < (less than)
#' 
#' For example, to specify that TIME less than 24 should be processed, you type
#' the expression: TIME < 24.
#' 
#' The unary logical operators are: & (and) | (or)
#' 
#' For example, to specify TIME less than 24 and males (SEX equal to 1), you
#' type the expression: TIME < 24 & SEX == 1
#' 
#' This subset selection scheme works on all variables, including ID numbers.
#' 
#' The subset selection is not entirely stable. For example, there is no check
#' that the user enters a valid expression, nor that the user specifies
#' existing variable names. An erroneous expression will not become evident
#' until a plot is attempted and the expression takes effect.
#' 
#' @aliases change.subset change.cat.levels change.cat.levels<-
#' change.dv.cat.levels change.dv.cat.levels<- change.cat.cont
#' change.cat.cont<- change.miss get.doc set.doc
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @param dv.cat.limit The limit for which we treat DV as categorical.  If
#' there are \code{dv.cat.limit} or less unique dv values then dv is treated as
#' categorical.
#' @param cat.limit The limit for which we treat a list of values as
#' categorical.  If there are \code{cat.limit} or less unique values then the
#' list is treated as categorical.
#' @param listall A logical operator specifying whether the items in the
#' database should be listed.
#' @param to.cat.vec A vector of strings specifying the names of the
#' categorical variables that should be transformed to continuous.
#' @param to.cont.vec A vector of strings specifying the names of the
#' continuous variables that should be transformed to categorical.
#' @param change.type.vec A vector of strings specifying the names of the
#' variables that should be transformed to/from continuous/categorical.
#' @param \dots arguments passed to other functions.
#' @param value This is the value that will be replaced in the xpose data
#' object \code{object}.  \code{value} is used in the \dQuote{replacement
#' function} version of these functions.  That is the form where we have
#' \code{function.name(object) <- value}.  If \code{value} is \code{NULL} then
#' the functions prompt the user for a value.  For \code{change.cat.levels},
#' \code{value} is the categorical limit \code{cat.limit}.  For
#' \code{change.dv.cat.levels}, \code{value} is the DV categorical limit
#' \code{dv.cat.limit}.  For \code{change.cat.cont}, \code{value} is the
#' \code{change.type.vec}.  See the examples below.
#' @return An \code{\link{xpose.data}} object, except \code{view.doc}, which
#' returns the value of object@Doc.
#' @author Andrew Hooker, Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{Data}}, \code{\link{SData}}, \code{\link{subset}},
#' \code{\link{xpose.data}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Change default subset
#' xpdb5 <- change.subset(xpdb5)
#' 
#' ## Set documentation field
#' xpdb5 <- set.doc(xpdb5)
#' ## View it
#' view.doc(xpdb5)
#' 
#' ## change the categorical limit for the dv variable
#' change.dv.cat.levels(xpdb5) <- 10
#' 
#' ## change the categorical limit for non DV variables
#' change.cat.levels(xpdb5) <- 2
#' ## or
#' xpdb5 <- change.cat.levels(xpdb5,cat.levels=2)
#' 
#' ## chnage variables from categorical to continuous
#' xpdb5 <- change.cat.cont(xpdb5,to.cat.vec=c("AGE"),to.cont.vec=c("SEX"))
#' xpdb5 <- change.cat.cont(xpdb5,change.type.vec=c("AGE","SEX"))
#' change.cat.cont(xpdb5) <- c("AGE","SEX")
#' }
#' 
NULL





#' Plot the parameter or covariate distributions using a histogram
#' 
#' These functions plot the parameter or covariate values stored in an Xpose
#' data object using histograms.
#' 
#' Each of the parameters or covariates in the Xpose data object, as specified
#' in \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$covariates}
#' or \code{object@Prefs@Xvardef$ranpar} is evaluated in turn, creating a stack
#' of histograms.
#' 
#' A wide array of extra options controlling histograms are available. See
#' \code{\link{xpose.plot.histogram}} for details.
#' 
#' @aliases parm.hist cov.hist ranpar.hist
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{\link{xpose.plot.histogram}}.
#' @return Delivers a stack of histograms.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.histogram}},
#' \code{\link{xpose.panel.histogram}}, \code{\link[lattice]{histogram}},
#' \code{\link{xpose.data-class}}, \code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' }
#' 
#' ## Here we load the example xpose database 
#' data(simpraz.xpdb)
#' xpdb <- simpraz.xpdb
#' 
#' \dontrun{
#' ## A stack of parameter histograms, with 3 mirror panes
#' parm.hist(xpdb, mirror=3)
#' }
#' 
#' ## Covariate distribution, in green
#' cov.hist(xpdb, hicol=11, hidcol="DarkGreen", hiborder="White")
#' 
NULL





#' Plot the parameter or covariate distributions using quantile-quantile (Q-Q)
#' plots
#' 
#' These functions plot the parameter or covariate values stored in an Xpose
#' data object using Q-Q plots.
#' 
#' Each of the parameters or covariates in the Xpose data object, as specified
#' in \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$ranpar} or
#' \code{object@Prefs@Xvardef$covariates}, is evaluated in turn, creating a
#' stack of Q-Q plots.
#' 
#' A wide array of extra options controlling Q-Q plots are available. See
#' \code{\link{xpose.plot.qq}} for details.
#' 
#' @aliases parm.qq cov.qq ranpar.qq
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{\link{xpose.plot.qq}}.
#' @return Delivers a stack of Q-Q plots.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.qq}}, \code{\link{xpose.panel.qq}},
#' \code{\link[lattice]{qqmath}}, \code{\link{xpose.data-class}},
#' \code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' }
#' 
#' ## Here we load the example xpose database 
#' data(simpraz.xpdb)
#' xpdb <- simpraz.xpdb
#' 
#' \dontrun{
#' ## A stack of parameter histograms, with 3 mirror panes
#' parm.qq(xpdb5, mirror=3)
#' }
#' 
#' ## A stack of random parameter histograms
#' ranpar.qq(xpdb)
#' 
#' ## Covariate distribution, in green with red line of identity
#' cov.qq(xpdb, col=11, ablcol=2)
#' 
NULL





#' Data functions for Xpose 4
#' 
#' These functions perform various tasks in managing Xpose data objects.
#' 
#' These are internal Xpose functions, not intended for direct use.
#' 
#' @aliases check.vars is.readable.file test.xpose.data xpose.bin
#' @param vars List of variables to be checked.
#' @param object An \code{xpose.data} object.
#' @param silent A logical operator specifying whether output should be
#' displayed.
#' @param filename A filename to be checked for readability.
#' @return TRUE, FALSE or NULL.
#' @author Niclas Jonsson and Andrew Hooker
#' @seealso \code{\link{xpose.prefs-class}}, \code{\link{xvardef}}
#' @keywords methods
NULL





#' Generic internal functions for Xpose 4
#' 
#' These are internal functions relating to the Xpose generic functions.
#' 
#' These are internal Xpose functions, for adding ID numbers, computing
#' prediction intervals, randomization, stacking, and binning. They are not
#' intended for direct use.
#' 
#' @aliases addid computePI create.rand create.strat.rand eq.xpose get.refrunno
#' xpose.stack
#' @return Internal helper functions for the generic Xpose functions.
#' @author Justin Wilkins
#' @keywords methods
NULL





#' Internal functions for the VPC
#' 
#' Internal functions for the VPC
#' 
#' 
#' @aliases get.polygon.regions find.right.table setup.PPI
#' @param object Xpose object
#' @param inclZeroWRES Include row sof data with WRES=0
#' @param onlyfirst Use only first data for each individual
#' @param samp sample number
#' @param PI.subset Prediction interval subset
#' @param subscripts subscripts
#' @param PI.bin.table prediction interval binning table
#' @param panel.number panel number
#' @param PPI Plot prediction intervals
#' @param PI.mirror Prediction interval mirror
#' @param PIlimits Prediction interval limits
#' @param tmp.table temporary table
#' @param \dots Extra options passed to arguments
#' @return Returned to \code{\link{xpose.VPC}}
#' @keywords methods
NULL





#' Read NONMEM output files into Xpose 4
#' 
#' These are functions that read in a NONMEM output file (a '*.lst' file) and
#' then format the input for various \link{xpose4specific-package} fucntions.
#' 
#' These are internal Xpose functions used to read a NONMEM output file.
#' 
#' \code{fix.wrapped.lines} unwraps matrix lines that NONMEM wraps in table
#' files.  Assumes no more than 60 ETAs.
#' 
#' \code{read.lst} parses information out of NONMEM output, specified by the
#' \code{filename} argument, for use in run summaries.
#' 
#' \code{calc.npar} calculates the number and type of parameters included in a
#' NONMEM output file
#' 
#' @aliases fix.wrapped.lines read.lst read.lst7 read.lst6 read.phi
#' create.parameter.list calc.npar
#' @param par.mat A parameter matrix.
#' @param filename A NONMEM output file.
#' @param phi.file A NONMEM .phi file
#' @param phi.prefix prefix of a NONMEM .phi file
#' @param runno NONMEM run number
#' @param phi.suffix suffix of a NONMEM .phi file
#' @param quiet Quiet or not
#' @param nm7 NM7 or not
#' @param directory directory in which the NONMEM output files is
#' @param \dots Items passed to functions within this function.
#' @param listfile A NONMEM output file.
#' @param object The return value of \code{read.lst(filename)}
#' @return Internal helper functions for the generic Xpose functions.
#' @author Niclas Jonsson, Andrew Hooker & Justin Wilkins
#' @keywords methods
NULL





#' Resets Xpose variable definitions to factory settings
#' 
#' Function to reset Xpose's graphics parameters definitions to the default.
#' 
#' This functions is used to reset Xpose's graphic settings definitions to
#' their default values. Graphical settings are read from the file 'xpose.ini'
#' in the root of the 'xpose4' package.
#' 
#' @aliases reset.graph.par reset.variables
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{xpose.prefs-class}}, \code{\link{import.graph.par}},
#' \code{\link{change.xvardef}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Import graphics preferences you saved earlier using export.graph.par
#' xpdb5 <- import.graph.par(xpdb5)
#' 
#' ## Reset to default values
#' xpdb5 <- reset.graph.par(xpdb5)
#' 
#' ## Change WRES definition
#' xpdb5 <- change.wres(xpdb5)
#' }
#' 
NULL





#' Simulated prazosin Xpose database.
#' 
#' Xpose database from the NONMEM output of a model for prasozin using
#' simulated data (and NONMEM 7.2).
#' 
#' The database can be used to test functions in Xpose 4.  This database is
#' slightly different than the database that is created when reading in the
#' files created by \code{\link{simprazExample}} using
#' \code{\link{xpose.data}}.
#' 
#' @name simpraz.xpdb
#' @docType data
#' @format The format is: Formal class 'xpose.data' [package ".GlobalEnv"] with
#' 8 slots ..@ Data :'data.frame': 640 obs. of 42 variables: .. ..$ ID : num
#' [1:640] 1 1 1 1 1 1 1 1 1 1 ...  .. ..$ TIME : num [1:640] 0 1 2 3 4 5 6 7 9
#' 11 ...  .. ..$ IPRED : num [1:640] 0 69.2 80.2 75.3 66.9 ...  .. ..$ IWRES :
#' num [1:640] 0 0.0368 -0.0944 0.1683 -0.206 ...  .. ..$ DV : num [1:640] 0
#' 71.7 72.6 88 53.1 ...  .. ..$ PRED : num [1:640] 0 86.4 89 75.5 61 ...  ..
#' ..$ RES : num [1:640] 0 -14.68 -16.4 12.54 -7.91 ...  .. ..$ WRES : num
#' [1:640] 0 -0.105 -0.759 1.208 -1.539 ...  .. ..$ CL : num [1:640] 13.6 13.6
#' 13.6 13.6 13.6 ...  .. ..$ V : num [1:640] 93.6 93.6 93.6 93.6 93.6 ...  ..
#' ..$ KA : num [1:640] 1.22 1.22 1.22 1.22 1.22 ...  .. ..$ ETA1 : num [1:640]
#' -0.268 -0.268 -0.268 -0.268 -0.268 ...  .. ..$ ETA2 : num [1:640] 0.198
#' 0.198 0.198 0.198 0.198 ...  .. ..$ ETA3 : num [1:640] -0.164 -0.164 -0.164
#' -0.164 -0.164 ...  .. ..$ SEX : Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2
#' 2 2 ...  .. ..$ RACE : Factor w/ 3 levels "1","2","3": 2 2 2 2 2 2 2 2 2 2
#' ...  .. ..$ SMOK : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...  ..
#' ..$ HCTZ : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...  .. ..$ PROP
#' : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...  .. ..$ CON : Factor
#' w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...  .. ..$ OCC : Factor w/ 1 level
#' "0": 1 1 1 1 1 1 1 1 1 1 ...  .. ..$ AGE : num [1:640] 55 55 55 55 55 55 55
#' 55 55 55 ...  .. ..$ HT : num [1:640] 154 154 154 154 154 154 154 154 154
#' 154 ...  .. ..$ WT : num [1:640] 81 81 81 81 81 ...  .. ..$ SECR : num
#' [1:640] 1 1 1 1 1 1 1 1 1 1 ...  .. ..$ NPRED : num [1:640] 0 86.4 89 75.5
#' 61 ...  .. ..$ NRES : num [1:640] 0 -14.68 -16.4 12.54 -7.91 ...  .. ..$
#' NWRES : num [1:640] 0 -0.0997 -0.7782 1.2737 -1.6358 ...  .. ..$ PREDI : num
#' [1:640] 0 86.4 89 75.5 61 ...  .. ..$ RESI : num [1:640] 0 -14.68 -16.4
#' 12.54 -7.91 ...  .. ..$ WRESI : num [1:640] 0 -0.105 -0.759 1.208 -1.539 ...
#' .. ..$ CPRED : num [1:640] 0 85.8 91.3 79.3 65.3 ...  .. ..$ CRES : num
#' [1:640] 0 -14.03 -18.67 8.73 -12.16 ...  .. ..$ CWRES : num [1:640] 0
#' -0.0646 -0.9411 1.1911 -1.5154 ...  .. ..$ CPREDI: num [1:640] 0 85.8 91.3
#' 79.3 65.3 ...  .. ..$ CRESI : num [1:640] 0 -14.03 -18.67 8.73 -12.16 ...
#' .. ..$ CWRESI: num [1:640] 0 -0.101 -0.825 1.071 -1.504 ...  .. ..$ EPRED :
#' num [1:640] 0 80.9 80.5 69.4 57.7 ...  .. ..$ ERES : num [1:640] 0 -9.14
#' -7.84 18.63 -4.57 ...  .. ..$ EWRES : num [1:640] 0 -0.162 -0.626 1.505
#' -1.485 ...  .. ..$ NPDE : num [1:640] 0 -0.00836 -0.54367 1.5548 -1.8339 ...
#' .. ..$ ECWRES: num [1:640] 0 -0.156 -0.65 1.457 -1.336 ...  ..@ SData : NULL
#' ..@ Data.firstonly :'data.frame': 64 obs. of 12 variables: .. ..$
#' SUBJECT_NO: int [1:64] 1 2 3 4 5 6 7 8 9 10 ...  .. ..$ ID : int [1:64] 1 2
#' 3 4 5 6 7 8 9 10 ...  .. ..$ ETA.1.  : num [1:64] -0.2677 -0.7097 -0.4762
#' 0.0996 -0.3529 ...  .. ..$ ETA.2.  : num [1:64] 0.198 0.186 0.202 -0.429
#' 0.098 ...  .. ..$ ETA.3.  : num [1:64] -0.164 0.737 0.436 0.151 0.524 ...
#' .. ..$ ETC.1.1.  : num [1:64] 0.00412 0.00646 0.00401 0.00321 0.00328 ...
#' .. ..$ ETC.2.1.  : num [1:64] -0.002413 -0.002923 -0.001677 0.003449
#' -0.000594 ...  .. ..$ ETC.2.2.  : num [1:64] 0.00971 0.00622 0.00661 0.00605
#' 0.00691 ...  .. ..$ ETC.3.1.  : num [1:64] -0.00947 -0.01828 -0.01274
#' -0.00658 -0.01295 ...  .. ..$ ETC.3.2.  : num [1:64] 0.01757 0.00998 0.01247
#' 0.00237 0.01117 ...  .. ..$ ETC.3.3.  : num [1:64] 0.0821 0.3496 0.1956
#' 0.0754 0.2295 ...  .. ..$ OBJ : num [1:64] 54.04 60.73 9.13 27.62 25.53 ...
#' ..@ SData.firstonly: NULL ..@ Runno : num 5 ..@ Nsim : NULL ..@ Doc : NULL
#' ..@ Prefs :Formal class 'xpose.prefs' [package ".GlobalEnv"] with 8 slots ..
#' .. ..@ Xvardef :List of 14 .. .. .. ..$ id : chr "ID" .. .. .. ..$ idlab :
#' chr "ID" .. .. .. ..$ idv : chr "TIME" .. .. .. ..$ occ : chr "OCC" .. .. ..
#' ..$ dv : chr "DV" .. .. .. ..$ pred : chr "PRED" .. .. .. ..$ ipred : chr
#' "IPRED" .. .. .. ..$ iwres : chr "IWRES" .. .. .. ..$ wres : chr "WRES" ..
#' .. .. ..$ cwres : chr "CWRES" .. .. .. ..$ res : chr "RES" .. .. .. ..$
#' parms : chr [1:6] "CL" "V" "KA" "ETA1" ...  .. .. .. ..$ covariates: chr
#' [1:11] "SEX" "RACE" "SMOK" "HCTZ" ...  .. .. .. ..$ ranpar : chr [1:3]
#' "ETA1" "ETA2" "ETA3" .. .. ..@ Labels :List of 43 .. .. .. ..$ OCC : chr
#' "Occasion" .. .. .. ..$ TIME : chr "Time" .. .. .. ..$ PRED : chr
#' "Population predictions" .. .. .. ..$ IPRED : chr "Individual predictions"
#' .. .. .. ..$ WRES : chr "Weighted residuals" .. .. .. ..$ CWRES : chr
#' "Conditional weighted residuals" .. .. .. ..$ IWRES : chr "Individual
#' weighted residuals" .. .. .. ..$ DV : chr "Observations" .. .. .. ..$ RES :
#' chr "Residuals" .. .. .. ..$ CL : chr "Clearance" .. .. .. ..$ V : chr
#' "Volume" .. .. .. ..$ TAD : chr "Time after dose" .. .. .. ..$ ID : chr "ID"
#' .. .. .. ..$ KA : chr "KA" .. .. .. ..$ ETA1 : chr "ETA1" .. .. .. ..$ ETA2
#' : chr "ETA2" .. .. .. ..$ ETA3 : chr "ETA3" .. .. .. ..$ SEX : chr "SEX" ..
#' .. .. ..$ RACE : chr "RACE" .. .. .. ..$ SMOK : chr "SMOK" .. .. .. ..$ HCTZ
#' : chr "HCTZ" .. .. .. ..$ PROP : chr "PROP" .. .. .. ..$ CON : chr "CON" ..
#' .. .. ..$ AGE : chr "AGE" .. .. .. ..$ HT : chr "HT" .. .. .. ..$ WT : chr
#' "WT" .. .. .. ..$ SECR : chr "SECR" .. .. .. ..$ NPRED : chr "NPRED" .. ..
#' .. ..$ NRES : chr "NRES" .. .. .. ..$ NWRES : chr "NWRES" .. .. .. ..$ PREDI
#' : chr "PREDI" .. .. .. ..$ RESI : chr "RESI" .. .. .. ..$ WRESI : chr
#' "WRESI" .. .. .. ..$ CPRED : chr "CPRED" .. .. .. ..$ CRES : chr "CRES" ..
#' .. .. ..$ CPREDI: chr "CPREDI" .. .. .. ..$ CRESI : chr "CRESI" .. .. .. ..$
#' CWRESI: chr "CWRESI" .. .. .. ..$ EPRED : chr "EPRED" .. .. .. ..$ ERES :
#' chr "ERES" .. .. .. ..$ EWRES : chr "EWRES" .. .. .. ..$ NPDE : chr "NPDE"
#' .. .. .. ..$ ECWRES: chr "ECWRES" .. .. ..@ Graph.prefs :List of 102 .. ..
#' .. ..$ type : chr "b" .. .. .. ..$ pch : num 1 .. .. .. ..$ cex : num 0.8 ..
#' .. .. ..$ lty : num 1 .. .. .. ..$ lwd : num 1 .. .. .. ..$ col : num 4 ..
#' .. .. ..$ fill : chr "lightblue" .. .. .. ..$ grid : logi FALSE .. .. .. ..$
#' aspect : num 1 .. .. .. ..$ condvar : NULL .. .. .. ..$ byordfun : chr
#' "median" .. .. .. ..$ ordby : NULL .. .. .. ..$ shingnum : num 6 .. .. ..
#' ..$ shingol : num 0.5 .. .. .. ..$ abline : NULL .. .. .. ..$ abllwd : num 1
#' .. .. .. ..$ ablcol : num 1 .. .. .. ..$ abllty : num 1 .. .. .. ..$ smlwd :
#' num 2 .. .. .. ..$ smcol : num 2 .. .. .. ..$ smlty : num 1 .. .. .. ..$
#' smspan : num 0.667 .. .. .. ..$ smdegr : num 1 .. .. .. ..$ lmline : NULL ..
#' .. .. ..$ lmlwd : num 2 .. .. .. ..$ lmcol : num 2 .. .. .. ..$ lmlty : num
#' 1 .. .. .. ..$ suline : NULL .. .. .. ..$ sulwd : num 2 .. .. .. ..$ sucol :
#' num 3 .. .. .. ..$ sulty : num 1 .. .. .. ..$ suspan : num 0.667 .. .. ..
#' ..$ sudegr : num 1 .. .. .. ..$ ids : logi FALSE .. .. .. ..$ idsmode : NULL
#' .. .. .. ..$ idsext : num 0.05 .. .. .. ..$ idscex : num 0.7 .. .. .. ..$
#' idsdir : chr "both" .. .. .. ..$ dilfrac : num 0.7 .. .. .. ..$ diltype :
#' NULL .. .. .. ..$ dilci : num 0.95 .. .. .. ..$ PIuplty : num 2 .. .. .. ..$
#' PIdolty : num 2 .. .. .. ..$ PImelty : num 1 .. .. .. ..$ PIuptyp : chr "l"
#' .. .. .. ..$ PIdotyp : chr "l" .. .. .. ..$ PImetyp : chr "l" .. .. .. ..$
#' PIupcol : chr "black" .. .. .. ..$ PIdocol : chr "black" .. .. .. ..$
#' PImecol : chr "black" .. .. .. ..$ PIuplwd : num 2 .. .. .. ..$ PIdolwd :
#' num 2 .. .. .. ..$ PImelwd : num 2 .. .. .. ..$ PIupltyR : num 1 .. .. ..
#' ..$ PIdoltyR : num 1 .. .. .. ..$ PImeltyR : num 2 .. .. .. ..$ PIuptypR :
#' chr "l" .. .. .. ..$ PIdotypR : chr "l" .. .. .. ..$ PImetypR : chr "l" ..
#' .. .. ..$ PIupcolR : chr "blue" .. .. .. ..$ PIdocolR : chr "blue" .. .. ..
#' ..$ PImecolR : chr "blue" .. .. .. ..$ PIuplwdR : num 2 .. .. .. ..$
#' PIdolwdR : num 2 .. .. .. ..$ PImelwdR : num 2 .. .. .. ..$ PIupltyM : num 1
#' .. .. .. ..$ PIdoltyM : num 1 .. .. .. ..$ PImeltyM : num 2 .. .. .. ..$
#' PIuptypM : chr "l" .. .. .. ..$ PIdotypM : chr "l" .. .. .. ..$ PImetypM :
#' chr "l" .. .. .. ..$ PIupcolM : chr "darkgreen" .. .. .. ..$ PIdocolM : chr
#' "darkgreen" .. .. .. ..$ PImecolM : chr "darkgreen" .. .. .. ..$ PIuplwdM :
#' num 0.5 .. .. .. ..$ PIdolwdM : num 0.5 .. .. .. ..$ PImelwdM : num 0.5 ..
#' .. .. ..$ PIarcol : chr "lightgreen" .. .. .. ..$ PIlimits : num [1:2] 0.025
#' 0.975 .. .. .. ..$ bwhoriz : logi FALSE .. .. .. ..$ bwratio : num 1.5 .. ..
#' .. ..$ bwvarwid : logi FALSE .. .. .. ..$ bwdotpch : num 16 .. .. .. ..$
#' bwdotcol : chr "black" .. .. .. ..$ bwdotcex : num 1 .. .. .. ..$ bwreccol :
#' chr "blue" .. .. .. ..$ bwrecfill: chr "transparent" .. .. .. ..$ bwreclty :
#' num 1 .. .. .. ..$ bwreclwd : num 1 .. .. .. ..$ bwumbcol : chr "blue" .. ..
#' .. ..$ bwumblty : num 1 .. .. .. ..$ bwumblwd : num 1 .. .. .. ..$ bwoutcol
#' : chr "blue" .. .. .. ..$ bwoutcex : num 0.8 .. .. .. ..$ bwoutpch : num 1
#' .. .. .. ..$ hicol : num 5 .. .. .. ..$ hiborder : num 1 .. .. .. ..$ hilty
#' : num 1 .. .. .. ..$ hilwd : num 1 .. .. .. .. [list output truncated] .. ..
#' ..@ Miss : num -99 .. .. ..@ Cat.levels : num 4 .. .. ..@ DV.Cat.levels: num
#' 7 .. .. ..@ Subset : NULL .. .. ..@ Gam.prefs :List of 21 .. .. .. ..$
#' onlyfirst : logi TRUE .. .. .. ..$ wts : logi FALSE .. .. .. ..$ start.mod :
#' NULL .. .. .. ..$ steppit : logi TRUE .. .. .. ..$ disp : NULL .. .. .. ..$
#' nmods : num 3 .. .. .. ..$ smoother1 : num 0 .. .. .. ..$ smoother2 : num 1
#' .. .. .. ..$ smoother3 : chr "ns" .. .. .. ..$ smoother4 : chr "ns" .. .. ..
#' ..$ arg1 : NULL .. .. .. ..$ arg2 : NULL .. .. .. ..$ arg3 : chr "df=2" ..
#' .. .. ..$ arg4 : chr "df=3" .. .. .. ..$ excl1 : NULL .. .. .. ..$ excl2 :
#' NULL .. .. .. ..$ excl3 : NULL .. .. .. ..$ excl4 : NULL .. .. .. ..$ extra
#' : NULL .. .. .. ..$ plot.ids : logi TRUE .. .. .. ..$ medianNorm: logi TRUE
#' @seealso \code{\link{simprazExample}}
#' @keywords datasets
#' @examples
#' 
#' data(simpraz.xpdb)
#' str(simpraz.xpdb)
#' 
NULL





#' Summarize individual parameter values and covariates
#' 
#' These functions produce tables, printed to the screen, summarizing the
#' individual parameter values or covariates from a dataset in Xpose 4.
#' 
#' 
#' @aliases parm.summary cov.summary
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 are
#' included in the plot. The default is FALSE.
#' @param out.file Where the results should be output to.  Can be ".screen",
#' ".ask", ".graph" or a filename in quotes.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param fill The color to fill the boxes in the table if the table is printed
#' to ".graph"
#' @param values.to.use Which values should be summarized
#' @param value.name The name of the values
#' @param max.plots.per.page Maximum plots per page.
#' @param \dots Other arguments passed to \code{Data} and \code{SData}.
#' @return Returned is the matrix of values from the table. \code{parm.summary}
#' and \code{cov.summary} produce summaries of parameters and covariates,
#' respectively. \code{parm.summary} produces less attractive output but
#' supports mirror functionality.
#' 
#' \code{parm.summary} and \code{cov.summary} utilize
#' \code{\link[Hmisc]{print.char.matrix}} to print the information to the
#' screen.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{Data}}, \code{\link{SData}},
#' \code{\link{xpose.data-class}}, \code{\link[Hmisc]{print.char.matrix}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' }
#' 
#' ## Here we load the example xpose database 
#' data(simpraz.xpdb)
#' xpdb <- simpraz.xpdb
#' 
#' parm.summary(xpdb)
#' 
#' cov.summary(xpdb)
#' 
NULL





#' Generic table functions for Xpose 4
#' 
#' These are internal table functions relating to the Xpose summary functions.
#' 
#' These are internal Xpose functions for outputting summary tables. They are
#' not intended for direct use.
#' 
#' @aliases continuous.table categorical.table
#' @return Internal helper functions for the generic Xpose summary functions.
#' @author Niclas Jonsson & Justin Wilkins
#' @keywords methods
NULL





#' GAM functions for Xpose 4
#' 
#' These are functions for running the generalized additive model within Xpose
#' 4, summarizing its results, and plotting them.
#' 
#' Forthcoming.
#' 
#' @aliases xp.gam check.gamobj xp.akaike.plot xp.check.scope xp.cook
#' xp.ind.stud.res xp.ind.inf.fit xp.ind.inf.terms xp.plot xp.scope3 xp.summary
#' @param object An xpose.data object.
#' @param title A text string indicating plot title. If \code{NULL}, left
#' blank.
#' @param xlb A text string indicating x-axis legend. If \code{NULL}, left
#' blank.
#' @param ylb A text string indicating y-axis legend. If \code{NULL}, left
#' blank.
#' @param covnam A list of covariate variables to use in the GAM search.
#' @param wts.col Column in the object@Data.firstonly to use as weights on the
#' parnam values.
#' @param nmods The number of modelfits to use when setting GAM scope. The
#' default is 3.
#' @param smoother1 Smoother for each model.
#' @param smoother2 Smoother for each model.
#' @param smoother3 Smoother for each model.
#' @param smoother4 Smoother for each model.
#' @param arg1 Argument for model 1.
#' @param arg2 Argument for model 2.
#' @param arg3 Argument for model 3.
#' @param arg4 Argument for model 4.
#' @param excl1 Covariate exclusion from model 1.
#' @param excl2 Covariate exclusion from model 2.
#' @param excl3 Covariate exclusion from model 3.
#' @param excl4 Covariate exclusion from model 4.
#' @param extra Scope parameter for the GAM.
#' @param gam.object A GAM object (see \code{\link[gam]{gam}}.
#' @param plot.ids Logical, specifies whether or not ID numbers should be
#' displayed.
#' @param idscex ID label size.
#' @param ptscex Point size.
#' @param recur If dispersion should be used in the GAM object.
#' @param prompt Specifies whether or not the user should be prompted to press
#' RETURN between plot pages. Default is TRUE.
#' @param parnam The parameter to run the GAM on.
#' @param covnams The covariates to test on the parnam
#' @param ask.for.input Should the program ask for input from the user?  Can be
#' TRUE or FALSE.
#' @param overwrite Should we overwrite the gam object stored in memory if it
#' exists already.  Can be TRUE or FALSE.
#' @param gamobj A GAM object to use in the plot. IF null then the user is
#' asked to choose from a list of GAM objects in memory.
#' @param \dots Other arguments passed to the GAM functions.
#' @return A GAM fit.
#' @author Niclas Jonsson & Andrew Hooker
#' @seealso \code{\link[gam]{gam}}, \code{\link[lattice]{dotplot}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb <- xpose.data(5)
#' 
#' ## Run a GAM
#' xp.gam(xpdb)
#' 
#' ## Summarize GAM
#' xp.summary()
#' 
#' ## An Akaike plot of the results
#' xp.akaike.plot()
#' 
#' ## Studentized residuals
#' xp.ind.stud.res()
#' 
#' ## Individual influence on GAM fit
#' xp.ind.inf.fit(plot.ids=xpdb@Prefs@Gam.prefs$plot.ids)
#' 
#' ## Individual influence on GAM terms
#' xp.ind.inf.terms(plot.ids=xpdb@Prefs@Gam.prefs$plot.ids)
#' }
NULL





#' Class xpose.data
#' 
#' The xpose.data class is the fundamental data object in Xpose 4. It contains
#' the data and preferences used in the creation of the Xpose plots and
#' analyses.
#' 
#' 
#' @name xpose.data-class
#' @aliases xpose.data-class numeric_or_NULL-class data.frame_or_NULL-class
#' @docType class
#' @note The is a validation function for the object called
#' \code{test.xpose.data}.
#' @section Objects from the Class: Objects are most easily created by the
#' \code{xpose.data} function, whcih reads the appropriate NONMEM table files
#' and populates the slots of the object.
#' @author Niclas Jonsson
#' @seealso \code{\link{xpose.data}}, \code{\link{Data}}, \code{\link{SData}}
#' \code{\link{read.nm.tables}}, \code{\link{xpose.prefs-class}}
#' @keywords classes
NULL





#' Class for creating multipe plots in xpose
#' 
#' Class for creating multipe plots in xpose
#' 
#' 
#' @name xpose.multiple.plot-class
#' @aliases xpose.multiple.plot-class list_or_NULL-class
#' logical_or_numeric-class character_or_NULL-class
#' @docType class
#' @section Slots:
#' 
#' \describe{ \item{list("plotList")}{A list of lattice plots}
#' 
#' \item{list("plotTitle")}{The plot title}
#' 
#' \item{list("prompt")}{Should prompts be used}
#' 
#' \item{list("new.first.window")}{Create a new first window?}
#' 
#' \item{list("max.plots.per.page")}{How many plots per page?}
#' 
#' \item{list("title")}{The title}
#' 
#' \item{list("mirror")}{Are there mirror plots to create}
#' 
#' \item{list("bql.layout")}{Should we use bql.layout} }
NULL





#' Class "xpose.prefs"
#' 
#' An object of the "xpose.prefs" class holds information about all the
#' variable and graphical preferences for a particular "xpose.data" object.
#' 
#' 
#' @name xpose.prefs-class
#' @aliases xpose.prefs-class character_or_numeric-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("xpose.prefs",...)} but this is usually not necessary since the
#' "xpose.prefs" object is created at the same time as the "xpose.data" object.
#' @author Niclas Jonsson & Andrew Hooker
#' @seealso \code{\link{xvardef}}, \code{\link{xlabel}}, \code{\link{xsubset}},
#' \code{\link{Data}}, \code{\link{SData}}, \code{\link{xpose.data}},
#' \code{\link{read.nm.tables}}, \code{\link{xpose.data-class}},
#' \code{\link{xpose.gam}}
#' @keywords classes
NULL





#' The Xpose Package
#' 
#' Xpose is an R-based model building aid for population analysis using NONMEM.
#' It facilitates data set checkout, exploration and visualization, model
#' diagnostics, candidate covariate identification and model comparison.
#' 
#' Xpose is an R library for post-processing of NONMEM output. It takes one or
#' more standard NONMEM table files as input and generates graphs or other
#' analyses. It is assumed that each NONMEM run can be uniquely identified by a
#' run number (see section below for how to generate the appropriate input to
#' Xpose). Xpose is implemented using the lattice graphics library.
#' 
#' The Xpose library can be divided up into five sub-sections:
#' 
#' \link{xpose4data-package} Functions for managing the input data and
#' manipulating the Xpose database.
#' 
#' \link{xpose4generic-package} Generic wrapper functions around the lattice
#' functions. These funtions can be invoked by the user but require quite
#' detailed instructions to generate the desired output.
#' 
#' \link{xpose4specific-package} These functions are single purpose functions
#' that generate specific output given only the Xpose database as input. The
#' behavior can, to some extent, be influenced by the user.
#' 
#' \link{xpose4classic-package} Xpose has a text based menu interface to make
#' it simple for the user to invoke the Xpose specific functions. This
#' interface is called Xpose Classic. Given the limitations a text based
#' interface imposes, Xpose Classic is not very flexible but may be useful for
#' quick assessment of a model and for learning to use Xpose.
#' 
#' \link{xpose4psn-package} These functions are the interface between Xpose and
#' PsN, i.e. they do not post-process NONMEM output but rather PsN output.
#' 
#' 
#' @name xpose4-package
#' @aliases xpose4-package xpose4 xpose
#' @docType package
#' @section How to make NONMEM generate input to Xpose: Xpose recognizes NONMEM
#' runs, and files associated to a particular run, though the run number.  This
#' is a number that is used in the name of NONMEM model files, output files and
#' table files.  The fundamental input to Xpose is one or more NONMEM table
#' files.  These table files should be named as below followed by the run
#' number, for example xptab1 for run number 1.  Xpose looks for files
#' according to the following pattern, where * is your run number:
#' 
#' \bold{sdtab*} Standard table file, containing ID, IDV, DV, PRED, IPRED,
#' WRES, IWRES, RES, IRES, etc.
#' 
#' \bold{patab*} Parameter table, containing model parameters - THETAs, ETAs
#' and EPSes
#' 
#' \bold{catab*} Categorical covariates, e.g. SEX, RACE
#' 
#' \bold{cotab*} Continuous covariates, e.g. WT, AGE
#' 
#' \bold{extra*, mutab*, mytab*, xptab*, cwtab*} Other variables you might need
#' to have available to Xpose
#' 
#' \bold{run*.mod} Model specification file
#' 
#' \bold{run*.lst} NONMEM output
#' 
#' Strictly, only one table file is needed for xpose (for example sdtab* or
#' xptab*).  However, using patab*, cotab*, catab* will influence the way that
#' Xpose interprets the data and are recommended to get full benefit from
#' Xpose.
#' 
#' You can use code in NONMEM similar to the following to generate the tables
#' you need.  NONMEM automatically appends DV, PRED, WRES and RES unless
#' NOAPPEND is specified.  Don't forget to leave at least one blank line at the
#' end of the NONMEM model specification file.
#' 
#' \code{$TABLE ID TIME IPRED IWRES EVID MDV NOPRINT ONEHEADER FILE=sdtab1}
#' \code{$TABLE ID CL V2 KA K SLP KENZ NOPRINT ONEHEADER FILE=patab1}
#' \code{$TABLE ID WT HT AGE BMI PKG NOPRINT ONEHEADER FILE=cotab1}
#' \code{$TABLE ID SEX SMOK ALC NOPRINT ONEHEADER FILE=catab1}
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @references \href{http://xpose.sf.netXpose Homepage},
#' \href{http://psn.sf.netPsN Homepage}
#' @keywords methods package
#' @examples
#' 
#' \dontrun{
#' # run the classic interface
#' library(xpose4)
#' xpose4()
#'   
#' # command line interface  
#' library(xpose4)
#' xpdb <- xpose.data(5)
#' basic.gof(xpdb)
#' }
#' 
NULL





#' Internal xpose4classic Functions
#' 
#' Internal xpose4classic functions
#' 
#' These are not to be called by the user (or in some cases are just waiting
#' for proper documentation to be written :).
#' 
#' @aliases bg.conv.crit1 bg.conv.crit2 bootgam.conv.crit1 bootgam.conv.crit2
#' bootgam.menu bootgam.plot.menu bootgam.settings.menu bootscm.menu
#' bootscm.plot.menu change.conv.alg exclude.individuals list.bootgam.settings
#' normalize.median set.max.replicates set.seed.number specify.interval
#' specify.start.check specify.start.model
#' @keywords internal
NULL





#' Xpose classic menu system
#' 
#' Functions used to control the classic menu system in Xpose.
#' 
#' Xpose has a text based menu interface to make it simple for the user to
#' invoke the Xpose specific functions. This interface is called Xpose Classic.
#' Given the limitations a text based interface imposes, Xpose Classic is not
#' very flexible but may be useful for quick assessment of a model and for
#' learning to use Xpose.
#' 
#' \itemize{ \item \code{\link{add.modify.db.items.menu}} \item
#' \code{\link{another.menu}} \item \code{\link{change.dispersion}} \item
#' \code{\link{change.graphical.par}} \item \code{\link{change.medianNorm}}
#' \item \code{\link{change.onlyfirst}} \item \code{\link{change.steppit}}
#' \item \code{\link{change.use.ids}} \item \code{\link{change.xp.obj}} \item
#' \code{\link{covariate.model.menu}} \item \code{\link{cwres.menu}} \item
#' \code{\link{data.checkout.menu}} \item \code{\link{documentation.menu}}
#' \item \code{\link{gam.change.weights}} \item \code{\link{gam.menu}} \item
#' \code{\link{gam.settings.menu}} \item \code{\link{gof.menu}} \item
#' \code{\link{list.gam.settings}} \item \code{\link{main.menu}} \item
#' \code{\link{manage.db}} \item \code{\link{model.comparison.covariates.menu}}
#' \item \code{\link{model.comparison.menu}} \item
#' \code{\link{parameters.menu}} \item \code{\link{preferences.menu}} \item
#' \code{\link{residual.diagnostics.menu}} \item \code{\link{runsum.print}}
#' \item \code{\link{struct.gof.menu}} \item
#' \code{\link{structural.diagnostics.menu}} \item
#' \code{\link{table.file.read.settings.menu}} \item \code{\link{vpc.npc.menu}}
#' \item \code{\link{xpose4}} }
#' 
#' @name xpose4classic-package
#' @docType package
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @keywords methods package
NULL





#' Internal xpose4data Functions
#' 
#' Internal xpose4data functions
#' 
#' These are not to be called by the user (or in some cases are just waiting
#' for proper documentation to be written :).
#' 
#' @aliases as.num ask.folder ask.group.by.cov convert.bootscm.bs.ids
#' convert.bootscm.cov.inclusion convert.cov.name convert.most.common
#' get.cov.stats get.pars.from.relations.file list.dirs read.bootscm.par.est
#' read.scm.covariate.sd reshape.simple
#' @keywords internal
NULL





#' Xpose database creation and manipulation
#' 
#' Functions used to read in NONMEM table files to an Xpose database and to
#' create and manipulate an Xpose database
#' 
#' \itemize{ \item \code{\link{add.absval}} \item \code{\link{add.dichot}}
#' \item \code{\link{add.exp}} \item \code{\link{add.log}} \item
#' \code{\link{add.tad}} \item \code{\link{bootscm.import}} \item
#' \code{\link{calc.npar}} \item \code{\link{change.ab.graph.par}} \item
#' \code{\link{change.bw.graph.par}} \item \code{\link{change.cat.cont}} \item
#' \code{\link{change.cat.levels}} \item \code{\link{change.cond.graph.par}}
#' \item \code{\link{change.dil.graph.par}} \item
#' \code{\link{change.dv.cat.levels}} \item \code{\link{change.label.par}}
#' \item \code{\link{change.lm.graph.par}} \item
#' \code{\link{change.misc.graph.par}} \item \code{\link{change.miss}} \item
#' \code{\link{change.parm}} \item \code{\link{change.pi.graph.par}} \item
#' \code{\link{change.smooth.graph.par}} \item \code{\link{change.subset}}
#' \item \code{\link{change.var.name}} \item \code{\link{change.xlabel}} \item
#' \code{\link{change.xvardef}} \item \code{\link{check.vars}} \item
#' \code{\link{create.parameter.list}} \item \code{\link{createXposeClasses}}
#' \item \code{\link{Data}} \item \code{\link{db.names}} \item
#' \code{\link{export.graph.par}} \item
#' \code{\link{export.variable.definitions}} \item
#' \code{\link{fix.wrapped.lines}} \item \code{\link{get.doc}} \item
#' \code{\link{import.graph.par}} \item
#' \code{\link{import.variable.definitions}} \item
#' \code{\link{is.readable.file}} \item \code{\link{make.sb.data}} \item
#' \code{\link{nsim}} \item \code{\link{nsim<-}} \item \code{\link{read.lst}}
#' \item \code{\link{read.lst6}} \item \code{\link{read.lst7}} \item
#' \code{\link{read.nm.tables}} \item \code{\link{read.npc.vpc.results}} \item
#' \code{\link{read.phi}} \item \code{\link{read.TTE.sim.data}} \item
#' \code{\link{read.vpctab}} \item \code{\link{reset.graph.par}} \item
#' \code{\link{reset.variables}} \item \code{\link{SData}} \item
#' \code{\link{set.doc}} \item \code{\link{simpraz.xpdb}} \item
#' \code{\link{simprazExample}} \item \code{\link{test.xpose.data}} \item
#' \code{\link{xlabel}} \item \code{\link{xpose.ask.for.filename}} \item
#' \code{\link{xpose.ask.for.lst}} \item \code{\link{xpose.ask.for.mod}} \item
#' \code{\link{xpose.bin}} \item \code{\link{xpose.data}} \item
#' \code{\link{xpose.data-class}} \item \code{\link{xpose.prefs-class}} \item
#' \code{\link{xpose.print}} \item \code{\link{xpose.read}} \item
#' \code{\link{xpose.write}} \item \code{\link{xsubset}} \item
#' \code{\link{xvardef}} }
#' 
#' @name xpose4data-package
#' @docType package
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @keywords methods package
NULL





#' Xpose generic multi-purpose functions
#' 
#' Generic xpose functions
#' 
#' These funtions can be invoked by the user but require quite detailed input
#' to generate the desired output.  used by \link{xpose4specific-package} to
#' create output.
#' 
#' \itemize{ \item \code{\link{add.grid.table}} \item
#' \code{\link{add.grid.text}} \item \code{\link{addid}} \item
#' \code{\link{categorical.table}} \item \code{\link{compute.cwres}} \item
#' \code{\link{computePI}} \item \code{\link{continuous.table}} \item
#' \code{\link{create.mirror}} \item \code{\link{create.rand}} \item
#' \code{\link{create.strat.rand}} \item
#' \code{\link{create.xpose.plot.classes}} \item \code{\link{eq.xpose}} \item
#' \code{\link{find.right.table}} \item \code{\link{get.polygon.regions}} \item
#' \code{\link{get.refrunno}} \item \code{\link{ind.cwres}} \item
#' \code{\link{is.cwres.readable.file}} \item
#' \code{\link{print.xpose.multiple.plot}} \item \code{\link{read.cwres.data}}
#' \item \code{\link{setup.PPI}} \item \code{\link{sqrtm}} \item
#' \code{\link{xpose.calculate.cwres}} \item \code{\link{xpose.create.label}}
#' \item \code{\link{xpose.create.title}} \item
#' \code{\link{xpose.create.title.hist}} \item
#' \code{\link{xpose.create.title.text}} \item \code{\link{xpose.dev.new}}
#' \item \code{\link{xpose.logTicks}} \item \code{\link{xpose.multiple.plot}}
#' \item \code{\link{xpose.multiple.plot.default}} \item
#' \code{\link{xpose.multiple.plot.title}} \item \code{\link{xpose.panel.bw}}
#' \item \code{\link{xpose.panel.default}} \item
#' \code{\link{xpose.panel.histogram}} \item \code{\link{xpose.panel.qq}} \item
#' \code{\link{xpose.panel.splom}} \item \code{\link{xpose.plot.bw}} \item
#' \code{\link{xpose.plot.default}} \item \code{\link{xpose.plot.histogram}}
#' \item \code{\link{xpose.plot.qq}} \item \code{\link{xpose.plot.splom}} \item
#' \code{\link{xpose.stack}} \item \code{\link{xpose.string.print}} \item
#' \code{\link{xpose.xscale.components.log10}} \item
#' \code{\link{xpose.yscale.components.log10}} }
#' 
#' @name xpose4generic-package
#' @docType package
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @keywords methods package
NULL





#' Xpose PsN post-processing functions
#' 
#' Xpose functions for post-processing PsN output.
#' 
#' 
#' These functions are the interface between Xpose and PsN, i.e. they do not
#' post-process NONMEM output but rather PsN output.
#' 
#' \itemize{
#' 
#' \item bootstrap \itemize{ \item \code{\link{boot.hist}} }
#' 
#' \item boot_scm \itemize{ \item \code{\link{bootscm.import}} \item
#' \code{\link{bootgam.print}} \item \code{\link{xp.inc.prob}} \item
#' \code{\link{xp.inc.prob.comb.2}} \item \code{\link{xp.distr.mod.size}} \item
#' \code{\link{xp.inc.stab.cov}} \item \code{\link{xp.incl.index.cov}} \item
#' \code{\link{xp.incl.index.cov.ind}} \item
#' \code{\link{xp.incl.index.cov.comp}} \item \code{\link{xp.boot.par.est}}
#' \item \code{\link{xp.boot.par.est.corr}} \item \code{\link{xp.dofv.plot}} }
#' 
#' \item randtest \itemize{ \item \code{\link{randtest.hist}} }
#' 
#' \item vpc and npc \itemize{ \item \code{\link{cat.pc}} \item
#' \code{\link{kaplan.plot}} \item \code{\link{npc.coverage}} \item
#' \code{\link{xpose.VPC}} \item \code{\link{xpose.VPC.both}} \item
#' \code{\link{xpose.VPC.categorical}} }
#' 
#' }
#' 
#' @name xpose4psn-package
#' @docType package
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @keywords methods package
NULL





#' Internal xpose4specific Functions
#' 
#' Internal xpose4specific functions
#' 
#' These are not to be called by the user (or in some cases are just waiting
#' for proper documentation to be written :).
#' 
#' @aliases ask.bootgam.bootscm.type ask.covs.plot ask.incl.range
#' check.bootgamobj data.long get.boot.obj get.boot.type panel.ci prepanel.ci
#' test.covariate.levels xp.bootgam xpose.bootgam xpose.bootgam.drawsample
#' xpose.bootgam.extract.covnames ask.cov.name
#' @keywords internal
NULL





#' Xpose specific functions for a single purpose
#' 
#' A list of specific functions for tasks to be performed using an Xpose
#' database
#' 
#' These functions are single purpose functions that generate specific output
#' given only the Xpose database as input. The behavior can, to some extent, be
#' influenced by the user.
#' 
#' \itemize{ \item \code{\link{absval.cwres.vs.cov.bw}} \item
#' \code{\link{absval.cwres.vs.pred}} \item
#' \code{\link{absval.cwres.vs.pred.by.cov}} \item
#' \code{\link{absval.dcwres.vs.cov.model.comp}} \item
#' \code{\link{absval.dipred.vs.cov.model.comp}} \item
#' \code{\link{absval.diwres.vs.cov.model.comp}} \item
#' \code{\link{absval.dpred.vs.cov.model.comp}} \item
#' \code{\link{absval.dwres.vs.cov.model.comp}} \item
#' \code{\link{absval.iwres.cwres.vs.ipred.pred}} \item
#' \code{\link{absval.iwres.vs.cov.bw}} \item \code{\link{absval.iwres.vs.idv}}
#' \item \code{\link{absval.iwres.vs.ipred}} \item
#' \code{\link{absval.iwres.vs.ipred.by.cov}} \item
#' \code{\link{absval.iwres.vs.pred}} \item
#' \code{\link{absval.iwres.wres.vs.ipred.pred}} \item
#' \code{\link{absval.wres.vs.cov.bw}} \item \code{\link{absval.wres.vs.idv}}
#' \item \code{\link{absval.wres.vs.pred}} \item
#' \code{\link{absval.wres.vs.pred.by.cov}} \item \code{\link{add.model.comp}}
#' \item \code{\link{addit.gof}} \item \code{\link{autocorr.cwres}} \item
#' \code{\link{autocorr.iwres}} \item \code{\link{autocorr.wres}} \item
#' \code{\link{basic.gof}} \item \code{\link{basic.model.comp}} \item
#' \code{\link{bootgam.print}} \item \code{\link{cat.dv.vs.idv.sb}} \item
#' \code{\link{cat.pc}} \item \code{\link{check.gamobj}} \item
#' \code{\link{cov.hist}} \item \code{\link{cov.qq}} \item
#' \code{\link{cov.splom}} \item \code{\link{cov.summary}} \item
#' \code{\link{cwres.dist.hist}} \item \code{\link{cwres.dist.qq}} \item
#' \code{\link{cwres.vs.cov}} \item \code{\link{cwres.vs.idv}} \item
#' \code{\link{cwres.vs.idv.bw}} \item \code{\link{cwres.vs.pred}} \item
#' \code{\link{cwres.vs.pred.bw}} \item \code{\link{cwres.wres.vs.idv}} \item
#' \code{\link{cwres.wres.vs.pred}} \item \code{\link{data.checkout}} \item
#' \code{\link{dOFV.vs.cov}} \item \code{\link{dOFV.vs.id}} \item
#' \code{\link{dOFV1.vs.dOFV2}} \item \code{\link{dv.preds.vs.idv}} \item
#' \code{\link{dv.vs.idv}} \item \code{\link{dv.vs.ipred}} \item
#' \code{\link{dv.vs.ipred.by.cov}} \item \code{\link{dv.vs.ipred.by.idv}}
#' \item \code{\link{dv.vs.pred}} \item \code{\link{dv.vs.pred.by.cov}} \item
#' \code{\link{dv.vs.pred.by.idv}} \item \code{\link{dv.vs.pred.ipred}} \item
#' \code{\link{gof}} \item \code{\link{gofSetup}} \item \code{\link{ind.plots}}
#' \item \code{\link{ind.plots.cwres.hist}} \item
#' \code{\link{ind.plots.cwres.qq}} \item \code{\link{ind.plots.wres.hist}}
#' \item \code{\link{ind.plots.wres.qq}} \item \code{\link{ipred.vs.idv}} \item
#' \code{\link{iwres.dist.hist}} \item \code{\link{iwres.dist.qq}} \item
#' \code{\link{iwres.vs.idv}} \item \code{\link{kaplan.plot}} %\item
#' \code{\link{npc.coverage}} \item \code{\link{parm.hist}} \item
#' \code{\link{parm.qq}} \item \code{\link{parm.splom}} \item
#' \code{\link{parm.summary}} \item \code{\link{parm.vs.cov}} \item
#' \code{\link{parm.vs.parm}} \item \code{\link{pred.vs.idv}} \item
#' \code{\link{ranpar.hist}} \item \code{\link{ranpar.qq}} \item
#' \code{\link{ranpar.splom}} \item \code{\link{ranpar.vs.cov}} \item
#' \code{\link{runsum}} \item \code{\link{tabulate.parameters}} \item
#' \code{\link{wres.dist.hist}} \item \code{\link{wres.dist.qq}} \item
#' \code{\link{wres.vs.cov}} \item \code{\link{wres.vs.idv}} \item
#' \code{\link{wres.vs.idv.bw}} \item \code{\link{wres.vs.pred}} \item
#' \code{\link{wres.vs.pred.bw}} \item \code{\link{xp.akaike.plot}} \item
#' \code{\link{xp.boot.par.est}} \item \code{\link{xp.boot.par.est.corr}} \item
#' \code{\link{xp.check.scope}} \item \code{\link{xp.cook}} \item
#' \code{\link{xp.distr.mod.size}} \item \code{\link{xp.dofv.plot}} \item
#' \code{\link{xp.gam}} \item \code{\link{xp.get.disp}} \item
#' \code{\link{xp.inc.prob}} \item \code{\link{xp.inc.prob.comb.2}} \item
#' \code{\link{xp.inc.stab.cov}} \item \code{\link{xp.incl.index.cov}} \item
#' \code{\link{xp.incl.index.cov.comp}} \item
#' \code{\link{xp.incl.index.cov.ind}} \item \code{\link{xp.ind.inf.fit}} \item
#' \code{\link{xp.ind.inf.terms}} \item \code{\link{xp.ind.stud.res}} \item
#' \code{\link{xp.plot}} \item \code{\link{xp.scope3}} \item
#' \code{\link{xp.summary}} \item \code{\link{xpose.gam}} \item
#' \code{\link{xpose.license.citation}} \item \code{\link{xpose.VPC}} \item
#' \code{\link{xpose.VPC.both}} \item \code{\link{xpose.VPC.categorical}} \item
#' \code{\link{xpPage}} }
#' 
#' @name xpose4specific-package
#' @docType package
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @keywords methods package
NULL



