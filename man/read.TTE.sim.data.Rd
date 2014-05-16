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
\name{read.TTE.sim.data}
\alias{read.TTE.sim.data}

\title{
Read (repeated) time-to-event simulation data files.
}
\description{
Read (repeated) time-to-event simulation data files.
}
\usage{
read.TTE.sim.data(sim.file,
                  subset = NULL,
                  headers = c("REP", "ID", "DV", "TIME", "FLAG2","DOSE"),
                  xpose.table.file = FALSE,
                  ...)
}
\arguments{
  \item{sim.file}{
Name of the simulated file.
}
  \item{subset}{
subset to extract.
}
  \item{headers}{
headers in file.
}
  \item{xpose.table.file}{
xpose table files.
}
  \item{\dots}{
Extra arguments passed to function.
}
}
\author{
  Andrew C. Hooker
}
\keyword{ methods }
