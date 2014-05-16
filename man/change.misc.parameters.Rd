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

\name{change.misc.parameters}
\alias{change.subset}
\alias{change.cat.levels}
\alias{change.cat.levels<-}
\alias{change.dv.cat.levels}
\alias{change.dv.cat.levels<-}
\alias{change.cat.cont}
\alias{change.cat.cont<-}
\alias{change.miss}
\alias{get.doc}
\alias{set.doc}
\title{Functions changing miscellaneous parameter settings in Xpose 4}
\description{
  These functions allow viewing and changing of settings relating to 
  subsets, categorical threshold values, documentation and numbers 
  indicating missing data values. 
}
\usage{                                   
change.cat.levels(object, 
                  classic = FALSE,
                  cat.limit=NULL,
                  ...)

change.cat.levels(object, 
                  classic = FALSE,
                  ...) <- value 
                                      
change.dv.cat.levels(object, 
                     classic = FALSE,
                     dv.cat.limit=NULL,
                     ...) 

change.dv.cat.levels(object, 
                     classic = FALSE,
                     ...) <- value 

change.cat.cont(object,
                listall=TRUE,
                classic=FALSE,
                to.cat.vec=NULL,
                to.cont.vec=NULL,
                change.type.vec=NULL,
                ...)

change.cat.cont(object,
                listall=TRUE,
                classic=FALSE,
                to.cat.vec=NULL,
                to.cont.vec=NULL,
                ...) <- value
  

change.miss(object,  
            classic = FALSE)
              
change.subset(object, 
              classic = FALSE) 
          
set.doc(object, 
        classic = FALSE)
          
get.doc(object,
    classic = FALSE)
        
}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{classic}{A logical operator specifying whether the function should 
    assume the classic menu system. This is an internal option 
    and need never be called from the command line.}
  \item{dv.cat.limit }{The limit for which we treat DV as categorical.
    If there are \code{dv.cat.limit} or less unique dv values then dv is
    treated as categorical.}
  \item{cat.limit }{The limit for which we treat a list of values  as categorical.
    If there are \code{cat.limit} or less unique values then the list is
    treated as categorical.}
  \item{listall}{A logical operator specifying whether the items in the
    database should be listed. }
  \item{to.cat.vec}{A vector of strings specifying the names of the
    categorical variables that should be transformed to continuous.}
  \item{to.cont.vec}{A vector of strings specifying the names of the
    continuous variables that should be transformed to categorical.}
  \item{change.type.vec}{A vector of strings specifying the names of the
    variables that should be transformed to/from
    continuous/categorical.}
  \item{\dots}{arguments passed to other functions.}
  \item{value }{This is the value that will be replaced in the xpose
    data object \code{object}.  \code{value} is used in the
    \dQuote{replacement function} version of these functions.  That is
    the form where we have \code{function.name(object) <- value}.
    If \code{value} is \code{NULL} then the functions prompt the user for
    a value.
    For \code{change.cat.levels}, \code{value} is the categorical limit
    \code{cat.limit}.
    For \code{change.dv.cat.levels}, \code{value} is the DV categorical
    limit \code{dv.cat.limit}.
    For \code{change.cat.cont}, \code{value} is the \code{change.type.vec}.
    See  the examples below.}
}
\details{
  These functions are used to change settings for
  the number of unique data values required in a variable in order to define
  it as continuous (\code{change.cat.levels} and \code{change.dv.cat.levels} for
  ordinary variables and the dependent variable, respectively), the value to use
  as 'missing' (\code{change.miss}), and viewing (\code{get.doc}) and setting 
  (\code{set.doc}) the documentation field in the Xpose data object.
  
  \code{change.cat.cont} allows interchange between categorical and continuous data 
  formats within the Xpose database. This in turn affects how plots are
  drawn. 
  
  \code{change.subset} is used for setting the data item's subset field. 
  To specify a subset of the data to process, you use the varable names
  and the regular R selection operators. To combine a subset over two
  or more variables, the selection expressions for the two variables are
  combined using R's unary logical operators.
  
  The variable names are those that are specified in the NONMEM table
  files (e.g. PRED, TIME, SEX).
  
  The selection operators are:
  ==   (equal)
  !=   (not equal)
  ||   (or)
  >    (greater than)
  <    (less than)
  
  For example, to specify that TIME less than 24 should be processed, you
  type the expression: TIME < 24.
  
  The unary logical operators are:
  &    (and)
  |    (or)
  
  For example, to specify TIME less than 24 and males (SEX equal to 1), you
  type the expression: TIME < 24 & SEX == 1
  
  This subset selection scheme works on all variables, including ID numbers.
  
  The subset selection is not entirely stable. For example, there is no
  check that the user enters a valid expression, nor that the user specifies 
  existing variable names. An erroneous expression will not become evident 
  until a plot is attempted and the expression takes effect.   
}
\value{
  An \code{\link{xpose.data}} object,
  except \code{view.doc}, which returns the value of object@Doc. 
}
\author{Andrew Hooker, Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{Data}},
  \code{\link{SData}},
  \code{\link{subset}},
  \code{\link{xpose.data}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Change default subset
xpdb5 <- change.subset(xpdb5)

## Set documentation field
xpdb5 <- set.doc(xpdb5)
## View it
view.doc(xpdb5)

## change the categorical limit for the dv variable
change.dv.cat.levels(xpdb5) <- 10

## change the categorical limit for non DV variables
change.cat.levels(xpdb5) <- 2
## or
xpdb5 <- change.cat.levels(xpdb5,cat.levels=2)

## chnage variables from categorical to continuous
xpdb5 <- change.cat.cont(xpdb5,to.cat.vec=c("AGE"),to.cont.vec=c("SEX"))
xpdb5 <- change.cat.cont(xpdb5,change.type.vec=c("AGE","SEX"))
change.cat.cont(xpdb5) <- c("AGE","SEX")
}
}
\keyword{methods}
