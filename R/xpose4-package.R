#' The Xpose Package
#' 
#' Xpose is an R-based model building aid for population analysis using NONMEM. 
#' It facilitates data set checkout, exploration and visualization, model 
#' diagnostics, candidate covariate identification and model comparison.
#' 
#' Xpose takes output from NONMEM output and/or PsN output and generates graphs
#' or other analyses. It is assumed that each NONMEM run can be uniquely
#' identified by a run number (see section below for how to generate the
#' appropriate input to Xpose). Xpose is implemented using the lattice graphics
#' library.
#' 
#' The Xpose package can be divided up into six subsections (functions
#' associated with each of the different subsections are linked in the "See
#' Also" section): 
#' \describe{ 
#'   \item{Data Functions}{Functions for managing the
#'          input data and manipulating the Xpose database.} 
#'   \item{Generic Functions}{Generic wrapper functions around the lattice 
#'         functions. These functions can be invoked by the user but require quite 
#'         detailed instructions to generate the desired output.} 
#'   \item{Specific Functions}{These functions are single purpose functions 
#'       that generate specific output given only the Xpose database as input. The 
#'       behavior can, to some extent, be influenced by the user.}
#'   \item{Classic Functions}{Xpose has a text based menu interface to make it
#'       simple for the user to invoke the Xpose specific functions. This interface is
#'       called Xpose Classic. Given the limitations a text based interface imposes,
#'       Xpose Classic is not very flexible but may be useful for quick assessment of
#'       a model and for learning to use Xpose.}
#'   \item{PsN Functions}{These functions are the interface between Xpose and 
#'       PsN, i.e. they do not post-process NONMEM output but rather PsN output.}
#'   \item{GAM Functions}{Functions take an Xpose object and performs a generalized additive model
#'         (GAM) stepwise search for influential covariates on a single model parameter.}
#' }
#'
#' @name xpose4-package
#' @aliases xpose4-package xpose
#' @section How to make NONMEM generate input to Xpose: Xpose recognizes NONMEM 
#'   runs, and files associated to a particular run, through the run number. 
#'   This is a number that is used in the name of NONMEM model files, output
#'   files and table files.  The fundamental input to Xpose is one or more
#'   NONMEM table files.  These table files should be named as below followed by
#'   the run number, for example xptab1 for run number 1.  Xpose looks for files
#'   according to the following pattern, where * is your run number:
#'   
#'   \bold{sdtab*} Standard table file, containing ID, IDV, DV, PRED, IPRED, 
#'   WRES, IWRES, RES, IRES, etc.
#'   
#'   \bold{patab*} Parameter table, containing model parameters - THETAs, ETAs 
#'   and EPSes
#'   
#'   \bold{catab*} Categorical covariates, e.g. SEX, RACE
#'   
#'   \bold{cotab*} Continuous covariates, e.g. WT, AGE
#'   
#'   \bold{extra*, mutab*, mytab*, xptab*, cwtab*} Other variables you might
#'   need to have available to Xpose
#'   
#'   \bold{run*.mod} Model specification file
#'   
#'   \bold{run*.lst} NONMEM output
#'   
#'   Strictly, only one table file is needed for xpose (for example sdtab* or 
#'   xptab*).  However, using patab*, cotab*, catab* will influence the way that
#'   Xpose interprets the data and are recommended to get full benefit from 
#'   Xpose.
#'   
#'   You can use code in NONMEM similar to the following to generate the tables 
#'   you need.  NONMEM automatically appends DV, PRED, WRES and RES unless 
#'   NOAPPEND is specified.  Don't forget to leave at least one blank line at
#'   the end of the NONMEM model specification file.
#'   
#'   \code{$TABLE ID TIME IPRED IWRES EVID MDV NOPRINT ONEHEADER FILE=sdtab1} 
#'   \code{$TABLE ID CL V2 KA K SLP KENZ NOPRINT ONEHEADER FILE=patab1} 
#'   \code{$TABLE ID WT HT AGE BMI PKG NOPRINT ONEHEADER FILE=cotab1} 
#'   \code{$TABLE ID SEX SMOK ALC NOPRINT ONEHEADER FILE=catab1}
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker
#' @references \href{https://uupharmacometrics.github.io/PsN/}{PsN} 
#' @keywords methods package
#' @examples
#' 
#' \dontrun{
#' # run the classic interface
#' library(xpose4)
#' xpose4()
#'   
#' # command line interface  
#' library(xpose4)
#' xpdb <- xpose.data(5)
#' basic.gof(xpdb)
#' }
#' 
#' @family data functions 
#' @family generic functions 
#' @family specific functions 
#' @family classic functions 
#' @family PsN functions 
#' @family GAM functions 
#' 
#' @import lattice
#' @import grid
#' @import gam
#' @import methods
#' 
#' @importFrom stats aggregate anova as.formula coefficients
#' density dfbetas  dnorm    formula    lm  
#'  lm.influence    loess    median    predict    preplot  
#'  qchisq    quantile    quasi    reorder    reshape  
#'  residuals    sd    summary.glm    update  
#' @importFrom grDevices    bmp    dev.cur    dev.new    dev.off    jpeg   pdf    png    postscript    rgb    tiff 
#' @importFrom graphics    hist   par 
#' @importFrom utils    capture.output    citation    count.fields  
#' data    head    installed.packages    menu  
#'             packageDescription    packageVersion    read.csv  
#'             read.table    setTxtProgressBar    tail    txtProgressBar  
#'             write.table 

"_PACKAGE"


