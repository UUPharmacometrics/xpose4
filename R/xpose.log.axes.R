# Xpose 4
# An R-based population pharmacokinetic/
# pharmacodynamic model building aid for NONMEM.
# Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
# Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
# Mats O. Karlsson and E. Niclas Jonsson.
# Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
# E. Niclas Jonsson.

# This file is a part of Xpose 4.
# Xpose 4 is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  A copy can be cound in the R installation
# directory under \share\licenses. If not, see http://www.gnu.org/licenses/.

#' @describeIn xpose.yscale.components.log10 Make log tic marks
#' @export
xpose.logTicks <- function (lim, loc = c(1, 5)) {
  ii <- floor(log10(range(lim))) + c(-1, 2)
  main <- 10^(ii[1]:ii[2])
  r <- as.numeric(outer(loc, main, "*"))
  r[lim[1] <= r & r <= lim[2]]
}  



#' Functions to create nice looking axes when using Log scales.
#' 
#' The funcions are used to create standard tic marks and axis labels when the
#' axes are on the log scale.
#' 
#' These functions create log scales that look like they should (not the
#' default R scales). These functions are used as input to the
#' \code{\link[lattice:axis.default]{xscale.components}} argument in a lattice
#' plot.
#' 
#' @aliases xpose.yscale.components.log10 xpose.xscale.components.log10
#' xpose.logTicks
#' @param lim Limits
#' @param loc Locations
#' @param \dots Additional arguments pased to the function.
#' @author Andrew Hooker
#' @seealso \code{\link{xpose.plot.default}}
#' \code{\link[lattice:axis.default]{xscale.components}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' xpdb5 <- xpose.data(5)
#' xpose.plot.default("PRED","DV",xpdb,logy=T,logx=T)
#' xpose.plot.default("PRED","DV",xpdb,logy=T,logx=T,
#'                    yscale.components = xpose.yscale.components.log10,
#'                    xscale.components = xpose.xscale.components.log10)
#' 
#' ## both give the same result
#' }
#' 
#' @export 
xpose.yscale.components.log10 <- function(lim, ...) {
  ans <- yscale.components.default(lim = lim, ...)
  tick.at <- xpose.logTicks(10^lim, loc = 1:9)
  tick.at.major <- xpose.logTicks(10^lim, loc = 1)
  major <- tick.at %in% tick.at.major
  ans$left$ticks$at <- log(tick.at, 10)
  ans$left$ticks$tck <- ifelse(major, 1.5, 0.75)
  ans$left$labels$at <- log(tick.at, 10)
  ans$left$labels$labels <- as.character(tick.at)
  ans$left$labels$labels[!major] <- ""
  ans$left$labels$check.overlap <- FALSE
  ans
}

#' @describeIn xpose.yscale.components.log10 Make log scale on x-axis
#' @export
xpose.xscale.components.log10 <- function(lim, ...) {
  ans <- xscale.components.default(lim = lim, ...)
  tick.at <- xpose.logTicks(10^lim, loc = 1:9)
  tick.at.major <- xpose.logTicks(10^lim, loc = 1)
  major <- tick.at %in% tick.at.major
  ans$bottom$ticks$at <- log(tick.at, 10)
  ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
  ans$bottom$labels$at <- log(tick.at, 10)
  ans$bottom$labels$labels <- as.character(tick.at)
  ans$bottom$labels$labels[!major] <- ""
  ans$bottom$labels$check.overlap <- FALSE
  ans
}
