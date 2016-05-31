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

\name{xpose.prefs-class}
\docType{class}
\alias{xpose.prefs-class}
\alias{character_or_numeric-class}

\title{Class "xpose.prefs"}
\description{An object of the "xpose.prefs" class holds information about
  all the variable and graphical preferences for a particular "xpose.data"
  object.}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("xpose.prefs",...)}
  but this is usually not necessary since the "xpose.prefs"
  object is created at the same time as the "xpose.data" object.
}
\section{Slots}{
  \describe{
    \item{\code{Xvardef}:}{Object of class \code{"list"}A list with the
      Xpose variable defintions. The values of the list
      are strings with the name of one or more columns in the data
      frames of Data or SData (\link{xpose.data-class}).

      To get a listing of the current variable definitions in a
      particular "xpose.data" object type: \code{object@Prefs@Xvardef} on the
      command line (\code{object} is the name of the xpose.data
      object). These definitions have default values that are adjusted
      to reflect the contents of the Data slot in the xpose.data object
      when the object is initialized. Look in the
      \code{createXposeClasses} function for the default values.

      To access a specific variable defintion, use
      \code{object@Prefs@Xvardef[["vardef"]]}, where \code{vardef} is the name
      of the Xpose variable. You can also use the utility function
      \code{\link{xvardef}}.
    }
    \item{\code{Labels}:}{Object of class \code{"list"} A list that
      translates column headers in Data and SData to labels to be used
      in graphs, e.g. "TIME" to "Time (h)". This is used in the labelling of
      axes, for example.

      To get a listing of the current labels in a
      particular "xpose.data" object type: \code{object@Prefs@Labels} on the
      command line (\code{object} is the name of the xpose.data
      object). These definitions have default values that are adjusted
      to reflect the contents of the Data slot in the xpose.data object
      when the object is initialized. Default Labels that are not
      present in the NONMEM table files are deleted and columns that are
      present in the table files but that do not have default Label get
      a Label that is the same as the column name. Look in the
      \code{createXposeClasses} function for the default values. 

      To access a specific variable defintion, use
      \code{object@Prefs@Label[["name"]]}, where \code{name} is the name
      of the column in the NONMEM table files. You can also use the utility function
      \code{\link{xlabel}} to access the labels.

      The label for a particular column can also be changed using the
      \code{\link{xlabel}} function.
    }
    \item{\code{Graph.prefs}:}{Object of class \code{"list"} This is a
      list with graphical parameters for the various elements in the
      Xpose graphs.

      To get a listing of the current graphical preferences in a
      particular "xpose.data" object type: \code{object@Prefs@Graph.prefs}
      on the command line (\code{object} is the name of the xpose.data
      object).

      Look in the \code{createXposeClasses} function for the default
      values and some comments.}

    \item{\code{Miss}:}{Object of class \code{"numeric"} This is a
      scalar value that indicate missing values in the NONMEM table
      files. The default is -99. Can be accessed by
      \code{object@Prefs@Miss}.
    }
    
    \item{\code{Cat.levels}:}{Object of class \code{"numeric"} This
      value is used when an "xpose.data" object is created. Any variable
      from a NONMEM table file that has fewer than or equal number of
      unque value tha Cat.levels are converted to factors. The default
      value is 4.
    }
    
    \item{\code{DV.Cat.levels}:}{Object of class \code{"numeric"} This
      value is used when an "xpose.data" object is created. If a
      variable from a NONMEM table file with the name DV has fewer than
      or equal number of unque value than DV.Cat.levels are converted to
      a factor. The default value is 7.
    }
    
    \item{\code{Subset}:}{Object of class \code{"character_or_NULL"} A
      string that indicates the subset of the data to use in a plot. An
      example is \code{SEX==1}. This string is used by the
      \code{\link{Data}} and \code{\link{SData}}
      functions when 
      extracting the data to plot from an "xpose.data" object. Note that
      it is not used in the \code{subset} argument to \code{xyplot}. The
      value of Subset can be accessed and changed using the
      \code{\link{xsubset} function}.
    }
    \item{\code{Gam.prefs}:}{Object of class \code{"list"} This is a
      list containing preferences for the general additive model (GAM).
      
      To get a listing of the current graphical preferences in a
      particular "xpose.data" object type: \code{object@Prefs@Gam.prefs}
      on the command line (\code{object} is the name of the xpose.data
      object).
      
      Look in the \code{createXposeClasses} function for the default
      values, and see also \code{\link{xpose.gam}}.}
  }    
}
\section{Methods}{
No methods defined with class "xpose.prefs" in the signature.
}
\author{Niclas Jonsson & Andrew Hooker}
\seealso{
  \code{\link{xvardef}},
  \code{\link{xlabel}},
  \code{\link{xsubset}},
  \code{\link{Data}},
  \code{\link{SData}},
  \code{\link{xpose.data}},
  \code{\link{read.nm.tables}},
  \code{\link{xpose.data-class}},
  \code{\link{xpose.gam}}
}
\keyword{classes}

