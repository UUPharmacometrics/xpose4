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



#' Print an Xpose multiple plot object.
#' 
#' Print an Xpose multiple plot object, which is the output from the function
#' \code{\link{xpose.multiple.plot}}.
#' 
#' Print method for a plot class.
#' 
#' @param x Output object from the function \code{\link{xpose.multiple.plot}}.
#' @param \dots Additional options passed to function.
#' @author Niclas Jonsson and Andrew C. Hooker
#' @seealso \code{\link{xpose.multiple.plot}}.
#' @keywords methods
#' @export print.xpose.multiple.plot
print.xpose.multiple.plot <-
  function(x,...) {

    xpose.multiple.plot.default(x@plotList,
                                plotTitle=x@plotTitle,
                                prompt=x@prompt,
                                new.first.window=x@new.first.window,
                                max.plots.per.page=x@max.plots.per.page,
                                title=x@title,               
                                mirror=x@mirror,
                                bql.layout=x@bql.layout,
                                ...)

    invisible()

  }
    
 
