
#' Categorical (visual) predictive check.
#' 
#' Categorical (visual) predictive check plots.
#' 
#' 
#' @param object Xpose data object.
#' @param dv The dependent variable (e.g. \code{"DV"} or \code{"CP"}.)
#' @param idv The independent variable (e.g. \code{"TIME"}.)
#' @param level.to.plot The levels to plot.
#' @param subset Subset of data.
#' @param histo If \code{FALSE} then a VPC is created, given that \code{idv} is
#' defined.
#' @param median.line Make a median line?
#' @param PI.lines Make prediction interval lines?
#' @param xlb Label for x axis.
#' @param ylb label for y axis.
#' @param main Main title.
#' @param strip Defining how the strips should appear in the conditioning
#' plots.
#' @param \dots Extra arguments passed to the function.
#' @author Andrew C. Hooker
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## read in table files
#' runno <- 45
#' xpdb <- xpose.data(runno)
#' 
#' ## create proportion (visual) predictive check
#' cat.pc(xpdb,idv=NULL)
#' cat.pc(xpdb,idv="DOSE")
#' cat.pc(xpdb,idv="DOSE",histo=F)
#' cat.pc(xpdb,idv="TIME",histo=T,level.to.plot=1)
#' }
#' 
#' @export 
#' @family specific functions 
cat.pc <-
  function(object,
           dv=xvardef("dv",object),
           idv=xvardef("idv",object),
           level.to.plot=NULL,
           subset=NULL,
           histo=T,
           median.line=F,
           PI.lines=F,
           xlb=if(histo){
             paste("Proportion of ",dv)
           } else {
             paste(idv)
           },
           ylb=if(histo){
             paste("Percent of Total")
           } else {
             paste("Proportion of Total")
           },
           main=xpose.create.title.text(NULL,dv,
             "Predictive check of",object,subset=subset,...),
           strip="Default",
           ...)
  {
    ##Get data
    sdata <- SData(object,inclZeroWRES=TRUE,onlyfirst=FALSE,
                   subset=subset)
    data <- Data(object,inclZeroWRES=TRUE,onlyfirst=FALSE,
                 subset=subset)
    
    ## Strip "missing" data
    data <- subset(data, get(dv) != object@Prefs@Miss)
    sdata <- subset(sdata, get(dv) != object@Prefs@Miss)    
    if(any(is.null(data))) return("The subset expression is invalid!\n")
    if(any(is.null(sdata))) return("The subset expression is invalid!\n")

    ## get props for every simulated data set
    props <- c()
    for(i in 1:max(sdata[,"iter"])){
      tmp <- sdata[eval(parse(text=paste("sdata$iter==",i))),]
      tmp.retlist <- make.sb.data(tmp,idv,dv,...)
      tmp.ret <- tmp.retlist$ret
      if(!is.null(level.to.plot)){
        tmp.ret <- tmp.ret[tmp.ret["dv"]==level.to.plot,]
      }
      ##tmp.prop <- tmp.ret[paste(sim.dv,"=",level.to.plot,sep=""),]
      ##tmp.prop$level <- paste(sim.dv,"=",level.to.plot,sep="")
      props <- rbind(props,tmp.ret)
    }

    ## get the props for the real data set
    retlist <- make.sb.data(data,idv,dv,...)
    prop <- retlist$ret
    if(!is.null(level.to.plot)){
      prop <- prop[prop["dv"]==level.to.plot,]
    }

    ## set up the formula for plotting a histogram of the
    ## simulated data
    var.name <- c()
    dv.levels.sim <- unique(props[,"dv"])
    dv.levels.real <- unique(prop[,"dv"])    
    n.dv.levels.sim <- length(dv.levels.sim)
    n.dv.levels.real <- length(dv.levels.real)
    if(n.dv.levels.real != n.dv.levels.sim){
      if(n.dv.levels.sim < n.dv.levels.real){
        props[,"dv"] <- factor(props[,"dv"],levels=levels(prop[,"dv"]))
      }
      if(n.dv.levels.sim > n.dv.levels.real){
        prop[,"dv"] <- factor(prop[,"dv"],levels=levels(props[,"dv"]))
      }
    }
    dv.levels <- levels(prop[,"dv"])
    n.dv.levels <- length(dv.levels)
    if(is.null(idv)) { ## No conditioning
      histo=T
      if (n.dv.levels==1){
        formel <- paste("~proportion",sep="")
      } else {
        formel <- paste("~proportion|dv",sep="")
        var.name <- c(dv)
      }
    } else {
      if (n.dv.levels==1){
        formel <- paste("~proportion|idv",sep="")
        formel.2 <- paste("real~idv",sep="")
        var.name <- c(idv)
      } else {
        formel <- paste("~proportion|dv+idv",sep="")
        formel.2 <- paste("real~idv|by.var",sep="")
        var.name <- c(dv,idv)
      }
    }

    if(!histo){
      ## Set up the data frame for a VPC
      num.col.new <- 6
      idv.levs <- unique(props[,"idv"])
      num.row.new <- n.dv.levels*length(idv.levs)
      ret.new   <- data.frame(matrix(nrow = num.row.new,
                                     ncol = num.col.new))
      names(ret.new) <- c("idv","real","lower","median","upper","by.var")
      if(!is.null(levels(props[,"idv"]))){
        ret.new["idv"] <- factor(ret.new["idv"],levels=levels(props[,"idv"]))
      }
      ret.new["by.var"] <- factor(ret.new["by.var"],levels=levels(props[,"dv"]))
      
      i <- 1
      for(LEVS in 1:n.dv.levels){
        tmp.by=dv.levels[LEVS]
        dat1.sim <- props[props[,"dv"] == dv.levels[LEVS], ,drop=F ]
        dat1.real <- prop[prop[,"dv"] == dv.levels[LEVS], ,drop=F ]
        for(IDV in 1:length(idv.levs)){
          tmp.idv=idv.levs[IDV]
          dat2.sim <- dat1.sim[dat1.sim[,"idv"] == idv.levs[IDV], ,drop=F ]
          dat2.real <- dat1.real[dat1.real[,"idv"] == idv.levs[IDV], ,drop=F ]
          tmp.pctls <- quantile(dat2.sim$proportion,probs=c(0.05,0.5,0.95))
          tmp.real <- dat2.real$proportion
          ret.new[i,"idv"] <- tmp.idv
          ret.new[i,"real"] <- tmp.real
          ret.new[i,c("lower","median","upper")] <- tmp.pctls
          ret.new[i,"by.var"] <- tmp.by
          i <- i+1
        }
      }
    }
    if(strip=="Default"){
      strip <- strip.custom(strip.names=TRUE,
                            var.name=var.name)
    }
    
    if(histo){
      xplot <- histogram(formula(formel),data=props,
                         xlab=xlb,main=main,real.data=prop,
                         strip=strip,
                         ...,
                         panel=function(real.data,...){
                           ##tmp.data <- ret.new[ret.new$by.var==dv.levels[panel.number()],]
                           panel.histogram(...)
                           panel.abline(v=real.data$proportion[panel.number()],
                                        col="red",lwd=3,...)
                         }
                         )
    } else {
      xplot <-
        xyplot(formula(formel.2),
               data=ret.new,
               type="b",
               prepanel = function(x,y,...) {
                 tmp.data <- ret.new[ret.new$by.var==dv.levels[panel.number()],]
                 if(is.factor(x)){#length(levs <- unique(x)) < object@Prefs@Cat.levels) {
                   xlim <- sort(levels(x))
                 } else {
                   xlim <- range(x)
                 }
                 ylim <- range(c(y,tmp.data$lower,tmp.data$upper))
                 list(xlim=xlim,ylim=ylim)
               },
               xlab=xlb,main=main,ylab=ylb,
               strip=strip,
               ...,
               panel=function(x,y,subscripts,...){
                 tmp.data <- ret.new[ret.new$by.var==dv.levels[panel.number()],]
                 grid.polygon(c(tmp.data$idv,rev(tmp.data$idv)),
                              c(tmp.data$upper,rev(tmp.data$lower)),
                              default.units="native",
                              gp=gpar(fill="green",alpha=0.3,col=NULL,lty=0)
                              )
                 panel.xyplot(x,y,...)
                 if(median.line){
                   panel.lines(tmp.data$idv,tmp.data$median,type="b",
                               col="darkgrey",
                               lty=2)
                 }
                 if(PI.lines){
                   panel.lines(tmp.data$idv,tmp.data$lower,type="b",
                               col="darkgreen",
                               lty=2)
                   panel.lines(tmp.data$idv,tmp.data$upper,type="b",
                               col="darkgreen",
                               lty=2)
                 }
               }
               )

      
##                  grid.polygon(xrecs,y.up.recs,
##                               default.units="native",
##                               gp=gpar(fill=PI.ci.up.arcol,alpha=0.3,col=NULL,lty=0)
##                               )

##       ,
##                       ...)
    }
    return(xplot)
  }
