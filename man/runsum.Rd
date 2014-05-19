% Xpose 4
% An R-based population pharmacokinetic/
% pharmacodynamic model building aid for NONMEM.
% Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
% Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
% Mats O. Karlsson and E. Niclas Jonsson.
% Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
% E. Niclas Jonsson.

% This file is a part of Xpose 4.
% Xpose 4 is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public License
% as published by the Free Software Foundation, either version 3
% of the License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public License
% along with this program.  A copy can be cound in the R installation
% directory under \share\licenses. If not, see http://www.gnu.org/licenses/.

\name{runsum}
\alias{runsum}
\title{Print run summary in Xpose 4}
\description{
  Function to build Xpose run summaries.
}
\usage{
% runsum.print(object,
%              dev = "win",
%              printer = "HP8150",
%              modfile = paste("run", object@Runno, ".mod", sep=""),
%              listfile = paste("run", object@Runno, ".lst", sep=""),
%              new.version=TRUE,
%              print.cex=0.45,
%              ...)
    runsum(object,
           dir="",
           modfile=paste(dir,"run",object@Runno,".mod",sep=""),
           listfile=paste(dir,"run",object@Runno,".lst",sep=""),
           main=NULL,
           subset=xsubset(object),
           show.plots=TRUE,
           txt.cex=0.7,
           txt.font=1,
           show.ids=FALSE,
           param.table=TRUE,
           txt.columns=2,
           force.wres=FALSE,
           ...)
             
% runsum.old(object,
%              modfile = paste("run", object@Runno, ".mod", sep=""),
%              listfile = paste("run", object@Runno, ".lst", sep=""),
%              main = NULL,
%              subset = xsubset(object),
%              ...)

% add.plots(object,
%            main = NULL,
%            subset = NULL,
%            inclZeroWRES = FALSE,
%            onlyfirst = FALSE,
%            col = object@Prefs@Graph.prefs$col,
%            pch = object@Prefs@Graph.prefs$pch,
%            lty = object@Prefs@Graph.prefs$lty,
%            smlwd = object@Prefs@Graph.prefs$smlwd,
%            smlty = object@Prefs@Graph.prefs$smlty,
%            smcol = object@Prefs@Graph.prefs$smcol,
%            abllwd = object@Prefs@Graph.prefs$abllwd,
%            abllty = object@Prefs@Graph.prefs$abllty,
%            ablcol = object@Prefs@Graph.prefs$ablcol,
%            ids = object@Prefs@Graph.prefs$ids,
%            idsmode = object@Prefs@Graph.prefs$idsmode,
%            idsext = object@Prefs@Graph.prefs$idsext,
%            idscex = object@Prefs@Graph.prefs$idscex,
%            idsdir = object@Prefs@Graph.prefs$idsdir,
%            ...)
}
\arguments{
  \item{object}{An xpose.data object. }
  % \item{dev}{String, specifying the device to print to. This can be "win",
  %   specifying the default Windows printer, or "lpr", specifying a Postscript
  %   device.}
%  \item{printer}{The name of the Postscript printer to print to.}
  \item{dir}{The directory to look for the model and output file of a
    NONMEM run.}
  \item{modfile}{The name of the NONMEM control stream associated with the
    current run.}
  \item{listfile}{The name of the NONMEM output file associated with the
    current run.}
  \item{main}{A string giving the main heading. \code{NULL} if none.}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}

  \item{show.plots}{Logical indicating if GOF plots should be shown in
    the run summary.}
  \item{txt.cex}{Number indicating the size of the txt in the run summary.}
  \item{txt.font}{Font of the text in the run summary.}
  \item{show.ids}{Logical indicating if IDs should be plotted in the
    plots for the run summary.}
  \item{param.table}{Logical indicating if the parameter table should be
    shown in the run summary.}
  \item{txt.columns}{The number of text columns in the run summary.}
  \item{force.wres}{Plot the WRES even if other residuals are available.}

  % \item{new.version}{The run summary to create with
  %   \code{runsum.print}. If FALSE then \code{runsum} is used, if
  %   TRUE then \code{runsum.new} is used.}
  % \item{print.cex}{The size of the text used in \code{runsum.new} when
  %   the run summary is output to a printer.}
  
  %\item{onlyfirst}{Logical value indicating whether only the first row
  %  per individual is included in the plot. }
  %\item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
  %  is included in the plot. The default is FALSE. }
  %\item{col}{The color for lines and points. Specified as an integer or 
  %  a text string. A full list is obtained by the R command 
  %  \code{colours()}. The default is blue (col=4).}
  %\item{pch}{The plotting character, or symbol, to use. Specified as an 
  % integer. See R help on \code{\link{points}}. The default is an open circle.}
  %\item{lty}{The line type. Line types can either be specified as an
  %  integer (0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash,
  %  5=longdash, 6=twodash) or as one of the character strings
  %  '"blank"', '"solid"', '"dashed"', '"dotted"', '"dotdash"',
  %  '"longdash"', or '"twodash"', where '"blank"' uses 'invisible
  %  lines' (i.e., doesn't draw them).}
  %\item{abllwd}{Line width of the abline.}
  %\item{abllty}{Line type of the abline.}
  %\item{ablcol}{Line colour of the abline.}
  %\item{smlwd}{Line width of the x-y smooth.}
  %\item{smlty}{Line type of the x-y smooth.}
  %\item{smcol}{Line color of the x-y smooth.}
  %\item{ids}{Logical value specifying whether to label data points.}
  %\item{idsmode}{Determines the way text labels are added to
  %  plots. \code{NULL} means that only extreme points are
  %  labelled. Non-\code{NULL} means all data points are labelled. (See
  %  \code{\link{xpose.plot.default}})}
  %\item{idsext}{specifies the extent of the extremes to be used in labelling 
  %  points. The default is 0.05 (only the most extreme 5\% of points are
  %  labelled).}
  %\item{idscex}{the amount by which labels should be scaled relative to the 
  %  default. 'NULL' and 'NA' are equivalent to '1.0'.}
  %\item{idsdir}{a string indicating the directions of the extremes to include in 
  %  labelling. Possible values are "up", "down" and "both".}
  \item{\dots}{Other arguments passed to the various functions.}
}

\value{
  A compound plot containing an Xpose run summary is created.
}
\author{Niclas Jonsson and Andrew Hooker}

\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)
}

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

runsum(xpdb)

}
\keyword{methods}
