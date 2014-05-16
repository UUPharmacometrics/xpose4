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

\name{create.mirror}
\alias{create.mirror}
\title{Function to create mirror plots from the generic Xpose plotting
  commands}
\description{
This function takes the generic plotting fucntions from Xpose 4 and
calls them multiple times with the current arguments to the functions,
changing the arguments as needed for mirror plotting.
}
\usage{
create.mirror(fun,
              arg.list,
              mirror,
              plotTitle,
              fix.y.limits = TRUE,
              fix.x.limits = TRUE,
              ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{The function name that we will call multiple times}
  \item{arg.list}{The arguments to that function}
  \item{mirror}{The type of mirror plots desired (1 or 3 mirror plots
    can be created)}
  \item{plotTitle}{The title for the plots}
  \item{fix.y.limits}{Should we fix all the y axes to be the same?}
  \item{fix.x.limits}{Should we fix all the x axes to be the same?}
  \item{\dots}{additional arguments passed to the function.}
}
\details{
 mostly and internal function for Xpose
}
\value{
  a list of plots, or NULL.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Andrew Hooker}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.plot.histogram}},
  \code{\link{xpose.plot.qq}},
  \code{\link{xpose.plot.splom}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

