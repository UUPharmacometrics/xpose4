setClassUnion("character_or_NULL",c("character","NULL"))
setClassUnion("character_or_numeric",c("character","numeric"))
setClassUnion("numeric_or_NULL",c("numeric","NULL"))
setClassUnion("data.frame_or_NULL",c("data.frame","NULL"))
setClassUnion("list_or_NULL",c("list","NULL"))
#setClassUnion("lang or numeric",c("vector","numeric","list"))
setClassUnion("logical_or_numeric",c("logical","numeric"))


#' Class "xpose.prefs"
#' 
#' An object of the "xpose.prefs" class holds information about all the
#' variable and graphical preferences for a particular "xpose.data" object.
#' 
#' 
#' @name xpose.prefs-class
#' @aliases xpose.prefs-class character_or_numeric-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("xpose.prefs",...)} but this is usually not necessary since the
#' "xpose.prefs" object is created at the same time as the "xpose.data" object.
#' @author Niclas Jonsson & Andrew Hooker
#' @seealso \code{\link{xvardef}}, \code{\link{xlabel}}, \code{\link{xsubset}},
#' \code{\link{Data}}, \code{\link{SData}}, \code{\link{xpose.data}},
#' \code{\link{read.nm.tables}}, \code{\link{xpose.data-class}},
#' \code{\link{xpose.gam}}
#' @keywords classes
#' @export
setClass("xpose.prefs",
         #representation = 
         slots = c(Xvardef       = "list",
                   Labels        = "list",
                   Graph.prefs   = "list",
                   Miss          = "numeric",
                   Cat.levels    = "numeric",
                   DV.Cat.levels = "numeric",
                   Subset        = "character_or_NULL",
                   Gam.prefs     = "list",
                   Bootgam.prefs = "list"
         ),
         prototype=list(
           Xvardef = list(
             id      = "ID",
             idlab   = "ID",
             idv     = "TIME",
             occ     = "OCC",
             dv      = "DV",
             pred    = "PRED",
             ipred   = "IPRED",
             iwres   = "IWRES",
             wres    = "WRES",
             cwres   = "CWRES",
             res     = "RES",
             parms   = c("CL","V","V1","V2","V3","Q","Q1","Q2","Q3","KA",
                         "ETA1","ETA2","ETA3","ETA4","ETA5","ETA6","ETA7",
                         "ETA8","ETA9","ET10","ET11","ET12","ET13","ET14",
                         "ET15","ET16","ET17","ET18","ET19","ET20"),
             covariates = c("GENO","SEX","RACE","DOSE","FLAG","DAY","PAT",
                            "GEND","AGE","WT","HT","CRCL","CLCR"),
             ranpar  = c("ETA1","ETA2","ETA3","ETA4","ETA5","ETA6","ETA7",
                         "ETA8","ETA9","ET10","ET11","ET12","ET13","ET14","ET15",
                         "ET16","ET17","ET18","ET19","ET20"),
             tvparms = c("TVCL","TVV","TVV1","TVV2","TVV3","TVQ","TVQ1",
                         "TVQ2","TVQ3","TVKA")
           ),
           
           Labels  = list(
             OCC  = "Occasion",
             TIME = "Time",
             PRED = "Population predictions",
             IPRED = "Individual predictions",
             IPRE = "Individual predictions",
             WRES = "Weighted residuals",
             CWRES = "Conditional weighted residuals",
             IWRES = "Individual weighted residuals",
             IWRE = "Individual weighted residuals",
             DV   = "Observations",
             RES  = "Residuals",
             CL = "Clearance",
             V  = "Volume",
             TAD  = "Time after dose"
           ),
           
           Graph.prefs = list(
             type   = "b" ,
             pch    = 1   ,
             cex    = 0.8 ,
             lty    = 1   ,
             lwd    = 1   ,
             col    = 4   ,
             fill   = "lightblue",
             grid   = FALSE ,
             aspect = "fill"   ,
             
             ## By arguments
             condvar   = NULL,
             byordfun  = "median" ,
             ordby     = NULL     ,
             shingnum  = 6        ,
             shingol   = 0.5      ,
             
             ## Abline settings
             abline = NULL ,
             abllwd = 1    ,
             ablcol = 1    ,
             abllty = 1,
             
             ## Smooth settings
             smooth = NULL ,
             smlwd  = 2    ,
             smcol  = "red" ,
             smlty  = 1    ,
             smspan = 2/3  ,
             smdegr = 1,
             
             ## Lm settings
             lmline = NULL,
             lmlwd  = 2,
             lmcol  = 2,
             lmlty  = 1,
             
             ## Superpose line settings
             suline = NULL,
             sulwd  = 2,
             sucol  = 3,
             sulty  = 1,
             suspan = 2/3,
             sudegr = 1,
             
             ## Text label settings,
             ids    = FALSE,
             idsmode= NULL,
             idsext = 0.05, ## In each end
             idscex = 0.7,
             idsdir = "both",
             
             ## Dilution stuff
             dilfrac = 0.7,
             diltype = NULL,
             dilci   = 0.95,
             
             ## Prediction interval stuff
             PIuplty = 2,
             PIdolty = 2,
             PImelty = 1,
             PIuptyp = "l",
             PIdotyp = "l",
             PImetyp = "l",
             PIupcol = "black",
             PIdocol = "black",
             PImecol = "black",
             PIuplwd = 2,
             PIdolwd = 2,
             PImelwd = 2,
             PIupltyR = 1,
             PIdoltyR = 1,
             PImeltyR = 2,
             PIuptypR = "l",
             PIdotypR = "l",
             PImetypR = "l",
             PIupcolR = "blue",
             PIdocolR = "blue",
             PImecolR = "blue",
             PIuplwdR = 2,
             PIdolwdR = 2,
             PImelwdR = 2,
             PIupltyM = 1,
             PIdoltyM = 1,
             PImeltyM = 2,
             PIuptypM = "l",
             PIdotypM = "l",
             PImetypM = "l",
             PIupcolM = "darkgreen",
             PIdocolM = "darkgreen",
             PImecolM = "darkgreen",
             PIuplwdM = 0.5,
             PIdolwdM = 0.5,
             PImelwdM = 0.5,
             PIarcol = "lightgreen",
             PIlimits=c(0.025,0.975),
             
             ## Categorical x-variable
             bwhoriz = FALSE,
             bwratio = 1.5,
             bwvarwid = FALSE,
             bwdotpch = 16,
             bwdotcol = "black",
             bwdotcex = 1,
             bwreccol = "blue",
             bwrecfill= "transparent",
             bwreclty = 1,
             bwreclwd = 1,
             bwumbcol = "blue",
             bwumblty = 1,
             bwumblwd = 1,
             bwoutcol  ="blue" ,
             bwoutcex  = 0.8,
             bwoutpch  = 1,
             
             ##Histogram settings
             hicol     = 5,#"blue",
             hiborder  = "black",
             hilty     = 1,
             hilwd     = 1,
             hidlty    = 2,
             hidlwd    = 2,
             hidcol    = 1
           ),
           
           Miss       = -99,
           Cat.levels = 4,
           DV.Cat.levels = 7,
           Subset     = NULL,
           
           Gam.prefs  = list(
             onlyfirst=TRUE,
             wts=FALSE,
             start.mod=NULL,
             steppit=TRUE,
             disp = NULL,
             nmods=3,
             smoother1=0,
             smoother2=1,
             smoother3="ns",
             smoother4="ns",
             arg1=NULL,
             arg2=NULL,
             arg3="df=2",
             arg4="df=3",
             excl1=NULL,
             excl2=NULL,
             excl3=NULL,
             excl4=NULL,
             extra=NULL,
             plot.ids=TRUE,
             medianNorm=TRUE
           ),
           Bootgam.prefs = list(n = 100,
                                algo = "fluct.ratio",
                                conv.value = as.numeric(1.04),
                                check.interval = as.numeric(20),
                                start.check = as.numeric(50),
                                liif = as.numeric(0.2),
                                ljif.conv = as.numeric(25),
                                seed = NULL,
                                start.mod = NULL,
                                excluded.ids = NULL
           )                  
           
         )
)

#' Class xpose.data
#' 
#' The xpose.data class is the fundamental data object in Xpose 4. It contains
#' the data and preferences used in the creation of the Xpose plots and
#' analyses.
#' 
#' 
#' @name xpose.data-class
#' @aliases xpose.data-class numeric_or_NULL-class data.frame_or_NULL-class
#' @docType class
#' @section Objects from the Class: Objects are most easily created by the
#' \code{xpose.data} function, which reads the appropriate NONMEM table files
#' and populates the slots of the object.
#' @author Niclas Jonsson and Andrew Hooker
#' @seealso \code{\link{xpose.data}}, \code{\link{Data}}, \code{\link{SData}}
#' \code{\link{read.nm.tables}}, \code{\link{xpose.prefs-class}}
#' @keywords classes
#' @export
setClass("xpose.data",
         slots=c(Data      = "data.frame_or_NULL",
                 SData     = "data.frame_or_NULL",
                 Data.firstonly = "data.frame_or_NULL",
                 SData.firstonly = "data.frame_or_NULL",
                 Runno     = "character_or_numeric",
                 Nsim      = "numeric_or_NULL",
                 Doc       = "character_or_NULL",
                 Prefs     = "xpose.prefs"
         ),
         prototype=list(Data    = NULL,
                        SData   = NULL,
                        Data.firstonly    = NULL,
                        SData.firstonly   = NULL,
                        Nsim    = NULL,
                        Runno   = NULL,
                        Doc     = NULL)#,
         #validity = test.xpose.data
)


#' Class for creating multiple plots in xpose
#' 
#' @slot plotList A list of lattice plots
#' @slot plotTitle The plot title
#' @slot prompt Should prompts be used
#' @slot new.first.window Create a new first window?
#' @slot max.plots.per.page How many plots per page?
#' @slot title The title
#' @slot mirror Are there mirror plots to create
#' @slot bql.layout Should we use bql.layout
#' @aliases list_or_NULL-class logical_or_numeric-class character_or_NULL-class 
#' 
#' @export
setClass("xpose.multiple.plot",#where=.GlobalEnv,
         slots=c(plotList           = "list_or_NULL",
                        plotTitle          = "character_or_NULL",
                        prompt             = "logical",
                        new.first.window   = "logical",
                        max.plots.per.page = "numeric",
                        title              = "list",
                        ##title.x            = "xptmp",
                        ##title.y            = "lang or numeric",
                        ##title.just         = "vector",
                        ##title.gp           = "lang or numeric",
                        mirror             = "logical_or_numeric",
                        bql.layout         = "logical"
         ),
         prototype=list(plotList = NULL,
                   plotTitle= NULL,
                   prompt   = FALSE,
                   new.first.window   = FALSE,
                   max.plots.per.page = 4,
                   title    = list(
                     title.x = unit(0.5, "npc"),
                     title.y = unit(0.5, "npc"),
                     title.gp= gpar(cex=1.2,fontface="bold"),#,font=2),
                     title.just = c("center","center")
                   ),
                   ##title.x            = unit(0.5, "npc"),
                   ##title.y            = c(unit(0.5, "npc")),
                   ##title.just         = c("center","center"),
                   ##title.gp           = list(cex=1.2,fontface="bold",font=2),
                   mirror             = FALSE,
                   bql.layout         = FALSE
         )
)

# @export
#setMethod("print",signature(x="xpose.multiple.plot"),print.xpose.multiple.plot)






