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
\name{kaplan.plot}
\alias{kaplan.plot}
\title{
Kaplan-Meier plots of (repeated) time-to-event data
}
\description{
Kaplan-Meier plots of (repeated) time-to-event data.  Includes VPCs.
}
\usage{
kaplan.plot(x="TIME",y="DV",id="ID",
           data=NULL,
           evid="EVID",
           by=NULL,
           xlab="Time",
           ylab="Default",
           object=NULL,
           events.to.plot="All",
           sim.data=NULL,
           sim.zip.file=NULL,
           VPC = FALSE,
           nsim.lab="simNumber",
           sim.evct.lab="counter",
           probs=c(0.025,0.975),
           add.baseline=T,
           add.last.area=T,
           subset=NULL,
           ##subset.real=NULL,
           main="Default",
           main.sub="Default",
           main.sub.cex = 0.8,
           nbins=NULL,
           real.type="l",
           real.lty=1,
           real.lwd=1,
           real.col="blue",
           real.se= if(!is.null(sim.data)) F else T,
           real.se.type="l",
           real.se.lty=2,
           real.se.lwd=0.5,
           real.se.col="red",
           cens.type="l",
           cens.lty=1,
           cens.col="black",
           cens.lwd=1,
           inclZeroWRES=TRUE,
           onlyfirst=FALSE,
           samp=NULL,
           poly.alpha=1,
           poly.fill="lightgreen",
           poly.line.col="darkgreen",
           poly.lty=2,
           censor.lines=TRUE,
           ylim=c(-5,105),
           cov = NULL,
           cov.fun = "mean",
           ...)
}
\arguments{
  \item{x}{
The indenpendent variable.
}
  \item{y}{
The dependent variable. event (>0) or no event (0).
}
  \item{id}{
The ID variable in the dataset.
}
  \item{data}{
A dataset can be used instead of the data in an Xpose object.  Must have
the same form as an xpose data object \code{xpdb@Data}.
}
  \item{evid}{
The EVID data item.  If not present then all rows are considered events
(can be censored or an event). Otherwise, EVID!=0 are dropped from the data set.
}
  \item{by}{
A vector of conditioning variables. 
}
  \item{xlab}{
X-axis label
}
  \item{ylab}{
Y-axis label
}
  \item{object}{
An Xpose object. Needed if no \code{data} is supplied.
}
  \item{events.to.plot}{
Vector of events to be plotted.  "All" means that all events are plotted.
}

\item{sim.data}{
  The simulated data file.  Should be a table file with one header row
  and have, at least, columns with headers coeesponding to
  \code{x}, \code{y}, \code{id}, \code{by} (if used), \code{nsim.lab}
  and \code{sim.evct.lab}.
}
  \item{sim.zip.file}{
The \code{sim.data} can be in \.zip format and xpose will unzip the file
  before reading in the data.  Must have the same structure as described
  above in \code{sim.data}.
}
  \item{VPC}{
\code{TRUE} or \code{FALSE}.  If \code{TRUE} then Xpose will search for
  a zipped file with name
  \code{paste("simtab",object@Runno,".zip",sep="")}, for example "simtab42.zip".
}
  \item{nsim.lab}{
The column header for \code{sim.data} that contains the simulation
  number for that row in the data.
}
\item{sim.evct.lab}{
  The column header for \code{sim.data} that contains the individual event counter 
  information.  For each individual the event counter should increase by
  one for each event (or censored event) that occurs.
}
\item{probs}{
  The probabilities (non-parametric percentiles) to use in computation
  of the prediciton intervals for the simulated dats. 
}
\item{add.baseline}{
  Should a (x=0,y=1) baseline measurement be added to each individual in the
  dataset. Otherwise each plot will begin at the first event in the dataset.
}
  \item{add.last.area}{
Should an area be added to the VPC extending the last PI?
}
  \item{subset}{
The subset of the data and sim.data to use.
}
  \item{main}{
The title of the plot. Can also be \code{NULL} or \code{"Default"}.
}
  \item{main.sub}{
The title of the subplots. Must be a list, the same length as the number
  of subplots (actual graphs), or \code{NULL} or \code{"Default"}.
}
\item{main.sub.cex}{
  The size of the title of the subplots.
}
\item{nbins}{
The number of bins to use in the VPC.  If \code{NULL}, the the number of
  unique \code{x} values in \code{sim.data} is used.
}
\item{real.se}{
Should the standard errors of the real (non simulated) data be plotted?
  Calculated using \code{\link[survival]{survfit}}.
}
\item{real.se.type}{
Type for the standard errors.
}
  \item{real.type}{
Type for the real data.
}
  \item{real.lwd}{
Line width (lwd) for the real data.
}
\item{real.lty}{
Line type (lty) for the curve of the original (or real) data.
}
\item{real.col}{
  Color for the curve of the original (or real) data.
}
  \item{real.se.lty}{
Line type (lty) for the standard error lines.
}
  \item{real.se.lwd}{
Line width (lwd) for the standard error lines.
}
  \item{real.se.col}{
Color for the standard error lines.
}
  \item{cens.type}{
Type for the censored lines.
}
  \item{cens.lty}{
Line type (lty) for the censored lines.
}
  \item{cens.col}{
Color for the censored lines.
}
  \item{cens.lwd}{
Line width for the censored lines.
}
\item{cov}{
  The covariate in the dataset to plot instead of the survival curve.
}
\item{cov.fun}{
  The summary function for the covariate in the dataset to plot instead of the survival curve.
}
  \item{inclZeroWRES}{
Include WRES=0 rows from the real data set in the plots?
}
  \item{onlyfirst}{
Include only the first measurement for the real data in the plots?
}
  \item{samp}{
Simulated data in the xpose data object can be used as the "real" data.
  \code{samp} is a number selecting which simulated data set to use.
}
  \item{poly.alpha}{
The tranparency of the VPC shaded region.
}
  \item{poly.fill}{
The fill color of the VPC shaded region.
}
  \item{poly.line.col}{
The line colors for the VPC region.
}
  \item{poly.lty}{
The line type for the VPC region.
}
  \item{censor.lines}{
Should censored observations be marked on the plot? 
}
  \item{ylim}{
Limits for the y-axes
}
  \item{\dots}{
Additional arguments passed to the function. 
}
}

\value{
  returns an object of class "xpose.multiple.plot".
}

\author{
Andrew C. Hooker
}

\seealso{
  \code{\link[survival]{survfit}},
  \code{\link[survival]{Surv}},
  \code{\link{xpose.multiple.plot}}.
}
\examples{
\dontrun{
library(xpose4)

## Read in the data
runno <- "57"
xpdb <- xpose.data(runno)

####################################
# here are the real data plots
####################################

kaplan.plot(x="TIME",y="DV",object=xpdb)
kaplan.plot(x="TIME",y="DV",object=xpdb,
            events.to.plot=c(1,2),
            by=c("DOSE==0","DOSE!=0"))
kaplan.plot(x="TIME",y="DV",object=xpdb,
            events.to.plot=c(1,2),
            by=c("DOSE==0","DOSE==10",
            "DOSE==50","DOSE==200"))

## make a PDF of the plots
pdf(file=paste("run",runno,"_kaplan.pdf",sep=""))
kaplan.plot(x="TIME",y="DV",object=xpdb,
            by=c("DOSE==0","DOSE==10",
            "DOSE==50","DOSE==200"))
dev.off()

####################################
## VPC plots
####################################

kaplan.plot(x="TIME",y="DV",object=xpdb,VPC=T,events.to.plot=c(1))
kaplan.plot(x="TIME",y="DV",object=xpdb,VPC=T,
            events.to.plot=c(1,2,3),
            by=c("DOSE==0","DOSE!=0"))
kaplan.plot(x="TIME",y="DV",object=xpdb,VPC=T,
            events.to.plot=c(1),
            by=c("DOSE==0","DOSE==10","DOSE==50","DOSE==200"))

## make a PDF of all plots
pdf(file=paste("run",runno,"_kaplan.pdf",sep=""))
kaplan.plot(x="TIME",y="DV",object=xpdb,VPC=T,
            by=c("DOSE==0","DOSE==10","DOSE==50","DOSE==200"))
dev.off()
}
}
\keyword{ methods }

