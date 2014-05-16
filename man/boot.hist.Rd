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
\name{boot.hist}
\alias{boot.hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create histograms  of results from the \code{bootstrap} tool in \href{http://psn.sf.net}{PsN}}
\description{
Reads results from the \code{bootstrap} tool in \href{http://psn.sf.net}{PsN} and then creates histograms.}
\usage{
boot.hist(results.file = "raw_results_run1.csv", 
          incl.ids.file = "included_individuals1.csv", 
          min.failed = FALSE, cov.failed = FALSE, 
          cov.warnings = FALSE, boundary = FALSE, 
          showOriginal = TRUE, showMean = FALSE, 
          showMedian = FALSE, showPCTS = FALSE, 
          PCTS = c(0.025, 0.975), excl.id = c(), 
          layout = NULL, sort.plots = TRUE, 
          main = "Default", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results.file}{
The location of the results file from the \code{bootstrap} tool in \href{http://psn.sf.net}{PsN}}
  \item{incl.ids.file}{
The location of the included ids file from the \code{bootstrap} tool in \href{http://psn.sf.net}{PsN}}

  \item{min.failed}{
Should NONMEM runs that had failed minimization be included? \code{TRUE} or \code{FALSE}}
  \item{cov.failed}{
Should NONMEM runs that had a failed covariance step be included? \code{TRUE} or \code{FALSE}}

  \item{cov.warnings}{
Should NONMEM runs that had covariance step warnings be included? \code{TRUE} or \code{FALSE}}

  \item{boundary}{
Should NONMEM runs that had boundary warnings be included? \code{TRUE} or \code{FALSE}}

  \item{showOriginal}{
Should we show the value from the original NONMEM run in the histograms? \code{TRUE} or \code{FALSE}}
  \item{showMean}{
Should we show the mean of the histogram data? \code{TRUE} or \code{FALSE}}

  \item{showMedian}{
Should we show the median of the histogram data? \code{TRUE} or \code{FALSE}}

  \item{showPCTS}{
Should we show the percentiles of the histogram data? \code{TRUE} or \code{FALSE}}

  \item{PCTS}{
the percentiles to show.  Can be a vector of any length.  For example, \code{c(0.05,0.2,0.5,0.7)}}
  \item{excl.id}{
Vector of id numbers to exclude.}
  \item{layout}{
Layout of plots.  A vector of number of rows and columns in each plot. \code{c(3,3)} for example.}
  \item{sort.plots}{
Should the plots be sorted based on type of parameter.  Sorting on parameters, standard errors, shrinkage and eigenvalues.}
  \item{main}{
The title of the plot.}
  \item{\dots}{
Additional arguments that can be passed to \link{xpose.plot.histogram}, \link{xpose.panel.histogram}, \link[lattice]{histogram} and other \link[lattice]{lattice-package} functions.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Plots or a list of plots are returned.
}
\references{
\href{http://psn.sf.net}{PsN} }
\author{
Andrew Hooker}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{xpose.plot.histogram}, \link{xpose.panel.histogram}, \link[lattice]{histogram} and other \link[lattice]{lattice-package} functions.}
\examples{
\dontrun{
boot.hist(results.file="./boot1/raw_results_run1.csv", 
          incl.ids.file="./boot1/included_individuals1.csv")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
