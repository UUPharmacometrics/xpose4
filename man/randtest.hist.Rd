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
\name{randtest.hist}
\alias{randtest.hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create a histogram of results from the randomization test tool (\code{randtest}) in \href{http://psn.sf.net}{PsN}}

\description{
Reads results from the \code{randtest} tool in \href{http://psn.sf.net}{PsN} and then creates a histogram.}

\usage{
randtest.hist(results.file = "raw_results_run1.csv", 
              df = 1, p.val = 0.05, main = "Default", 
              xlim = NULL, PCTSlcol = "black", 
              vlcol = c("red", "orange"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results.file}{
The location of the results file from the \code{randtest} tool in \href{http://psn.sf.net}{PsN}}

  \item{df}{
The degrees of freedom between the full and reduced model used in the randomization test.}
  \item{p.val}{
The p-value you would like to use.}
  \item{main}{
The title of the plot.}
  \item{xlim}{
The limits of the x-axis}
  \item{PCTSlcol}{
Color of the empirical line}
  \item{vlcol}{
Colors of the original and nominal line}
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
Returns a plot object.
}
\references{
\href{http://psn.sf.net}{PsN}}
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
randtest.hist(results.file="randtest_dir1/raw_results_run1.csv",df=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
