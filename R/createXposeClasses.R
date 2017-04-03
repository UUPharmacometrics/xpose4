

#' This function creates the Xpose data classes ("xpose.data" and
#' "xpose.prefs")
#' 
#' This function defines and sets the Xpose data classes.
#' 
#' 
#' @param nm7 \code{FALSE} if not using NONMEM 7.
#' @note All the default settings are defined in this function.
#' @author Niclas Jonsson and Andrew C. Hooker
#' @seealso \code{\link{xpose.data-class}},\code{\link{xpose.prefs-class}}
#' @keywords methods
#' @export createXposeClasses

"createXposeClasses" <-
  function(nm7=F) {
    
    setClassUnion("character or NULL",c("character","NULL"),where=.GlobalEnv)
    setClassUnion("character or numeric",c("character","numeric"),where=.GlobalEnv)
    setClassUnion("numeric or NULL",c("numeric","NULL"),where=.GlobalEnv)
    setClassUnion("data.frame or NULL",c("data.frame","NULL"),where=.GlobalEnv)
    setClassUnion("list or NULL",c("list","NULL"),where=.GlobalEnv)
    setClassUnion("lang or numeric",c("vector","numeric","list"),where=.GlobalEnv)
    setClassUnion("logical or numeric",c("logical","numeric"),where=.GlobalEnv)
    
    if(nm7) ipred.def <- "IPRED" else ipred.def <- "IPRE"
    if(nm7) iwres.def <- "IWRES" else iwres.def <- "IWRE"
    
    if(nm7) {
      labels.list  = list(
        OCC  = "Occasion",
        TIME = "Time",
        PRED = "Population predictions",
        IPRED = "Individual predictions",
        WRES = "Weighted residuals",
        CWRES = "Conditional weighted residuals",
        IWRES = "Individual weighted residuals",
        DV   = "Observations",
        RES  = "Residuals",
        CL = "Clearance",
        V  = "Volume",
        TAD  = "Time after dose"
      )
    } else {
      labels.list  = list(
        OCC  = "Occasion",
        TIME = "Time",
        PRED = "Population predictions",
        IPRE = "Individual predictions",
        WRES = "Weighted residuals",
        CWRES = "Conditional weighted residuals",
        IWRE = "Individual weighted residuals",
        DV   = "Observations",
        RES  = "Residuals",
        CL = "Clearance",
        V  = "Volume",
        TAD  = "Time after dose"
      )
    }
    
    
    setClass("xpose.prefs",where=.GlobalEnv,
             #representation = 
                      slots = c(Xvardef       = "list",
                                              Labels        = "list",
                                              Graph.prefs   = "list",
                                              Miss          = "numeric",
                                              Cat.levels    = "numeric",
                                              DV.Cat.levels = "numeric",
                                              Subset        = "character or NULL",
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
                          ipred   = ipred.def,
                          iwres   = iwres.def,
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
                        
                        Labels  = labels.list,
                        
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
    
    
    setClass("xpose.data",where=.GlobalEnv,
                      slots=c(Data      = "data.frame or NULL",
                                              SData     = "data.frame or NULL",
                                              Data.firstonly = "data.frame or NULL",
                                              SData.firstonly = "data.frame or NULL",
                                              Runno     = "character or numeric",
                                              Nsim      = "numeric or NULL",
                                              Doc       = "character or NULL",
                                              Prefs     = "xpose.prefs"
                      ),
                      prototype=list(Data    = NULL,
                                         SData   = NULL,
                                         Data.firstonly    = NULL,
                                         SData.firstonly   = NULL,
                                         Nsim    = NULL,
                                         Runno   = NULL,
                                         Doc     = NULL),
                      validity = test.xpose.data
    )
    
    
    
    
    invisible()
  }




