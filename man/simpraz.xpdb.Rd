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
\name{simpraz.xpdb}
\alias{simpraz.xpdb}
\docType{data}
\title{
Simulated prazosin Xpose database. 
}
\description{
  Xpose database from the NONMEM output of a model for prasozin using
  simulated data (and NONMEM 7.2).
}
\usage{data(simpraz.xpdb)}
\format{
  The format is:
Formal class 'xpose.data' [package ".GlobalEnv"] with 8 slots
  ..@ Data           :'data.frame':	640 obs. of  42 variables:
  .. ..$ ID    : num [1:640] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ TIME  : num [1:640] 0 1 2 3 4 5 6 7 9 11 ...
  .. ..$ IPRED : num [1:640] 0 69.2 80.2 75.3 66.9 ...
  .. ..$ IWRES : num [1:640] 0 0.0368 -0.0944 0.1683 -0.206 ...
  .. ..$ DV    : num [1:640] 0 71.7 72.6 88 53.1 ...
  .. ..$ PRED  : num [1:640] 0 86.4 89 75.5 61 ...
  .. ..$ RES   : num [1:640] 0 -14.68 -16.4 12.54 -7.91 ...
  .. ..$ WRES  : num [1:640] 0 -0.105 -0.759 1.208 -1.539 ...
  .. ..$ CL    : num [1:640] 13.6 13.6 13.6 13.6 13.6 ...
  .. ..$ V     : num [1:640] 93.6 93.6 93.6 93.6 93.6 ...
  .. ..$ KA    : num [1:640] 1.22 1.22 1.22 1.22 1.22 ...
  .. ..$ ETA1  : num [1:640] -0.268 -0.268 -0.268 -0.268 -0.268 ...
  .. ..$ ETA2  : num [1:640] 0.198 0.198 0.198 0.198 0.198 ...
  .. ..$ ETA3  : num [1:640] -0.164 -0.164 -0.164 -0.164 -0.164 ...
  .. ..$ SEX   : Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2 2 2 ...
  .. ..$ RACE  : Factor w/ 3 levels "1","2","3": 2 2 2 2 2 2 2 2 2 2 ...
  .. ..$ SMOK  : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ HCTZ  : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
  .. ..$ PROP  : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
  .. ..$ CON   : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
  .. ..$ OCC   : Factor w/ 1 level "0": 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ AGE   : num [1:640] 55 55 55 55 55 55 55 55 55 55 ...
  .. ..$ HT    : num [1:640] 154 154 154 154 154 154 154 154 154 154 ...
  .. ..$ WT    : num [1:640] 81 81 81 81 81 ...
  .. ..$ SECR  : num [1:640] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ NPRED : num [1:640] 0 86.4 89 75.5 61 ...
  .. ..$ NRES  : num [1:640] 0 -14.68 -16.4 12.54 -7.91 ...
  .. ..$ NWRES : num [1:640] 0 -0.0997 -0.7782 1.2737 -1.6358 ...
  .. ..$ PREDI : num [1:640] 0 86.4 89 75.5 61 ...
  .. ..$ RESI  : num [1:640] 0 -14.68 -16.4 12.54 -7.91 ...
  .. ..$ WRESI : num [1:640] 0 -0.105 -0.759 1.208 -1.539 ...
  .. ..$ CPRED : num [1:640] 0 85.8 91.3 79.3 65.3 ...
  .. ..$ CRES  : num [1:640] 0 -14.03 -18.67 8.73 -12.16 ...
  .. ..$ CWRES : num [1:640] 0 -0.0646 -0.9411 1.1911 -1.5154 ...
  .. ..$ CPREDI: num [1:640] 0 85.8 91.3 79.3 65.3 ...
  .. ..$ CRESI : num [1:640] 0 -14.03 -18.67 8.73 -12.16 ...
  .. ..$ CWRESI: num [1:640] 0 -0.101 -0.825 1.071 -1.504 ...
  .. ..$ EPRED : num [1:640] 0 80.9 80.5 69.4 57.7 ...
  .. ..$ ERES  : num [1:640] 0 -9.14 -7.84 18.63 -4.57 ...
  .. ..$ EWRES : num [1:640] 0 -0.162 -0.626 1.505 -1.485 ...
  .. ..$ NPDE  : num [1:640] 0 -0.00836 -0.54367 1.5548 -1.8339 ...
  .. ..$ ECWRES: num [1:640] 0 -0.156 -0.65 1.457 -1.336 ...
  ..@ SData          : NULL
  ..@ Data.firstonly :'data.frame':	64 obs. of  12 variables:
  .. ..$ SUBJECT_NO: int [1:64] 1 2 3 4 5 6 7 8 9 10 ...
  .. ..$ ID        : int [1:64] 1 2 3 4 5 6 7 8 9 10 ...
  .. ..$ ETA.1.    : num [1:64] -0.2677 -0.7097 -0.4762 0.0996 -0.3529 ...
  .. ..$ ETA.2.    : num [1:64] 0.198 0.186 0.202 -0.429 0.098 ...
  .. ..$ ETA.3.    : num [1:64] -0.164 0.737 0.436 0.151 0.524 ...
  .. ..$ ETC.1.1.  : num [1:64] 0.00412 0.00646 0.00401 0.00321 0.00328 ...
  .. ..$ ETC.2.1.  : num [1:64] -0.002413 -0.002923 -0.001677 0.003449 -0.000594 ...
  .. ..$ ETC.2.2.  : num [1:64] 0.00971 0.00622 0.00661 0.00605 0.00691 ...
  .. ..$ ETC.3.1.  : num [1:64] -0.00947 -0.01828 -0.01274 -0.00658 -0.01295 ...
  .. ..$ ETC.3.2.  : num [1:64] 0.01757 0.00998 0.01247 0.00237 0.01117 ...
  .. ..$ ETC.3.3.  : num [1:64] 0.0821 0.3496 0.1956 0.0754 0.2295 ...
  .. ..$ OBJ       : num [1:64] 54.04 60.73 9.13 27.62 25.53 ...
  ..@ SData.firstonly: NULL
  ..@ Runno          : num 5
  ..@ Nsim           : NULL
  ..@ Doc            : NULL
  ..@ Prefs          :Formal class 'xpose.prefs' [package ".GlobalEnv"] with 8 slots
  .. .. ..@ Xvardef      :List of 14
  .. .. .. ..$ id        : chr "ID"
  .. .. .. ..$ idlab     : chr "ID"
  .. .. .. ..$ idv       : chr "TIME"
  .. .. .. ..$ occ       : chr "OCC"
  .. .. .. ..$ dv        : chr "DV"
  .. .. .. ..$ pred      : chr "PRED"
  .. .. .. ..$ ipred     : chr "IPRED"
  .. .. .. ..$ iwres     : chr "IWRES"
  .. .. .. ..$ wres      : chr "WRES"
  .. .. .. ..$ cwres     : chr "CWRES"
  .. .. .. ..$ res       : chr "RES"
  .. .. .. ..$ parms     : chr [1:6] "CL" "V" "KA" "ETA1" ...
  .. .. .. ..$ covariates: chr [1:11] "SEX" "RACE" "SMOK" "HCTZ" ...
  .. .. .. ..$ ranpar    : chr [1:3] "ETA1" "ETA2" "ETA3"
  .. .. ..@ Labels       :List of 43
  .. .. .. ..$ OCC   : chr "Occasion"
  .. .. .. ..$ TIME  : chr "Time"
  .. .. .. ..$ PRED  : chr "Population predictions"
  .. .. .. ..$ IPRED : chr "Individual predictions"
  .. .. .. ..$ WRES  : chr "Weighted residuals"
  .. .. .. ..$ CWRES : chr "Conditional weighted residuals"
  .. .. .. ..$ IWRES : chr "Individual weighted residuals"
  .. .. .. ..$ DV    : chr "Observations"
  .. .. .. ..$ RES   : chr "Residuals"
  .. .. .. ..$ CL    : chr "Clearance"
  .. .. .. ..$ V     : chr "Volume"
  .. .. .. ..$ TAD   : chr "Time after dose"
  .. .. .. ..$ ID    : chr "ID"
  .. .. .. ..$ KA    : chr "KA"
  .. .. .. ..$ ETA1  : chr "ETA1"
  .. .. .. ..$ ETA2  : chr "ETA2"
  .. .. .. ..$ ETA3  : chr "ETA3"
  .. .. .. ..$ SEX   : chr "SEX"
  .. .. .. ..$ RACE  : chr "RACE"
  .. .. .. ..$ SMOK  : chr "SMOK"
  .. .. .. ..$ HCTZ  : chr "HCTZ"
  .. .. .. ..$ PROP  : chr "PROP"
  .. .. .. ..$ CON   : chr "CON"
  .. .. .. ..$ AGE   : chr "AGE"
  .. .. .. ..$ HT    : chr "HT"
  .. .. .. ..$ WT    : chr "WT"
  .. .. .. ..$ SECR  : chr "SECR"
  .. .. .. ..$ NPRED : chr "NPRED"
  .. .. .. ..$ NRES  : chr "NRES"
  .. .. .. ..$ NWRES : chr "NWRES"
  .. .. .. ..$ PREDI : chr "PREDI"
  .. .. .. ..$ RESI  : chr "RESI"
  .. .. .. ..$ WRESI : chr "WRESI"
  .. .. .. ..$ CPRED : chr "CPRED"
  .. .. .. ..$ CRES  : chr "CRES"
  .. .. .. ..$ CPREDI: chr "CPREDI"
  .. .. .. ..$ CRESI : chr "CRESI"
  .. .. .. ..$ CWRESI: chr "CWRESI"
  .. .. .. ..$ EPRED : chr "EPRED"
  .. .. .. ..$ ERES  : chr "ERES"
  .. .. .. ..$ EWRES : chr "EWRES"
  .. .. .. ..$ NPDE  : chr "NPDE"
  .. .. .. ..$ ECWRES: chr "ECWRES"
  .. .. ..@ Graph.prefs  :List of 102
  .. .. .. ..$ type     : chr "b"
  .. .. .. ..$ pch      : num 1
  .. .. .. ..$ cex      : num 0.8
  .. .. .. ..$ lty      : num 1
  .. .. .. ..$ lwd      : num 1
  .. .. .. ..$ col      : num 4
  .. .. .. ..$ fill     : chr "lightblue"
  .. .. .. ..$ grid     : logi FALSE
  .. .. .. ..$ aspect   : num 1
  .. .. .. ..$ condvar  : NULL
  .. .. .. ..$ byordfun : chr "median"
  .. .. .. ..$ ordby    : NULL
  .. .. .. ..$ shingnum : num 6
  .. .. .. ..$ shingol  : num 0.5
  .. .. .. ..$ abline   : NULL
  .. .. .. ..$ abllwd   : num 1
  .. .. .. ..$ ablcol   : num 1
  .. .. .. ..$ abllty   : num 1
  .. .. .. ..$ smlwd    : num 2
  .. .. .. ..$ smcol    : num 2
  .. .. .. ..$ smlty    : num 1
  .. .. .. ..$ smspan   : num 0.667
  .. .. .. ..$ smdegr   : num 1
  .. .. .. ..$ lmline   : NULL
  .. .. .. ..$ lmlwd    : num 2
  .. .. .. ..$ lmcol    : num 2
  .. .. .. ..$ lmlty    : num 1
  .. .. .. ..$ suline   : NULL
  .. .. .. ..$ sulwd    : num 2
  .. .. .. ..$ sucol    : num 3
  .. .. .. ..$ sulty    : num 1
  .. .. .. ..$ suspan   : num 0.667
  .. .. .. ..$ sudegr   : num 1
  .. .. .. ..$ ids      : logi FALSE
  .. .. .. ..$ idsmode  : NULL
  .. .. .. ..$ idsext   : num 0.05
  .. .. .. ..$ idscex   : num 0.7
  .. .. .. ..$ idsdir   : chr "both"
  .. .. .. ..$ dilfrac  : num 0.7
  .. .. .. ..$ diltype  : NULL
  .. .. .. ..$ dilci    : num 0.95
  .. .. .. ..$ PIuplty  : num 2
  .. .. .. ..$ PIdolty  : num 2
  .. .. .. ..$ PImelty  : num 1
  .. .. .. ..$ PIuptyp  : chr "l"
  .. .. .. ..$ PIdotyp  : chr "l"
  .. .. .. ..$ PImetyp  : chr "l"
  .. .. .. ..$ PIupcol  : chr "black"
  .. .. .. ..$ PIdocol  : chr "black"
  .. .. .. ..$ PImecol  : chr "black"
  .. .. .. ..$ PIuplwd  : num 2
  .. .. .. ..$ PIdolwd  : num 2
  .. .. .. ..$ PImelwd  : num 2
  .. .. .. ..$ PIupltyR : num 1
  .. .. .. ..$ PIdoltyR : num 1
  .. .. .. ..$ PImeltyR : num 2
  .. .. .. ..$ PIuptypR : chr "l"
  .. .. .. ..$ PIdotypR : chr "l"
  .. .. .. ..$ PImetypR : chr "l"
  .. .. .. ..$ PIupcolR : chr "blue"
  .. .. .. ..$ PIdocolR : chr "blue"
  .. .. .. ..$ PImecolR : chr "blue"
  .. .. .. ..$ PIuplwdR : num 2
  .. .. .. ..$ PIdolwdR : num 2
  .. .. .. ..$ PImelwdR : num 2
  .. .. .. ..$ PIupltyM : num 1
  .. .. .. ..$ PIdoltyM : num 1
  .. .. .. ..$ PImeltyM : num 2
  .. .. .. ..$ PIuptypM : chr "l"
  .. .. .. ..$ PIdotypM : chr "l"
  .. .. .. ..$ PImetypM : chr "l"
  .. .. .. ..$ PIupcolM : chr "darkgreen"
  .. .. .. ..$ PIdocolM : chr "darkgreen"
  .. .. .. ..$ PImecolM : chr "darkgreen"
  .. .. .. ..$ PIuplwdM : num 0.5
  .. .. .. ..$ PIdolwdM : num 0.5
  .. .. .. ..$ PImelwdM : num 0.5
  .. .. .. ..$ PIarcol  : chr "lightgreen"
  .. .. .. ..$ PIlimits : num [1:2] 0.025 0.975
  .. .. .. ..$ bwhoriz  : logi FALSE
  .. .. .. ..$ bwratio  : num 1.5
  .. .. .. ..$ bwvarwid : logi FALSE
  .. .. .. ..$ bwdotpch : num 16
  .. .. .. ..$ bwdotcol : chr "black"
  .. .. .. ..$ bwdotcex : num 1
  .. .. .. ..$ bwreccol : chr "blue"
  .. .. .. ..$ bwrecfill: chr "transparent"
  .. .. .. ..$ bwreclty : num 1
  .. .. .. ..$ bwreclwd : num 1
  .. .. .. ..$ bwumbcol : chr "blue"
  .. .. .. ..$ bwumblty : num 1
  .. .. .. ..$ bwumblwd : num 1
  .. .. .. ..$ bwoutcol : chr "blue"
  .. .. .. ..$ bwoutcex : num 0.8
  .. .. .. ..$ bwoutpch : num 1
  .. .. .. ..$ hicol    : num 5
  .. .. .. ..$ hiborder : num 1
  .. .. .. ..$ hilty    : num 1
  .. .. .. ..$ hilwd    : num 1
  .. .. .. .. [list output truncated]
  .. .. ..@ Miss         : num -99
  .. .. ..@ Cat.levels   : num 4
  .. .. ..@ DV.Cat.levels: num 7
  .. .. ..@ Subset       : NULL
  .. .. ..@ Gam.prefs    :List of 21
  .. .. .. ..$ onlyfirst : logi TRUE
  .. .. .. ..$ wts       : logi FALSE
  .. .. .. ..$ start.mod : NULL
  .. .. .. ..$ steppit   : logi TRUE
  .. .. .. ..$ disp      : NULL
  .. .. .. ..$ nmods     : num 3
  .. .. .. ..$ smoother1 : num 0
  .. .. .. ..$ smoother2 : num 1
  .. .. .. ..$ smoother3 : chr "ns"
  .. .. .. ..$ smoother4 : chr "ns"
  .. .. .. ..$ arg1      : NULL
  .. .. .. ..$ arg2      : NULL
  .. .. .. ..$ arg3      : chr "df=2"
  .. .. .. ..$ arg4      : chr "df=3"
  .. .. .. ..$ excl1     : NULL
  .. .. .. ..$ excl2     : NULL
  .. .. .. ..$ excl3     : NULL
  .. .. .. ..$ excl4     : NULL
  .. .. .. ..$ extra     : NULL
  .. .. .. ..$ plot.ids  : logi TRUE
  .. .. .. ..$ medianNorm: logi TRUE
}
\details{
The database can be used to test functions in Xpose 4.  This database is
slightly different than the database that is created when reading in the
files created by \code{\link{simprazExample}} using \code{\link{xpose.data}}.
}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\seealso{
  \code{\link{simprazExample}}
  }
\examples{
data(simpraz.xpdb)
str(simpraz.xpdb)
}
\keyword{datasets}
