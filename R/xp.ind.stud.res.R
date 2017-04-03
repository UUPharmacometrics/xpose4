#' @describeIn GAM_summary_and_plot Studentized residuals.
#' @export

"xp.ind.stud.res" <-
  function(gamobj=NULL,
           title = "Default",
           recur = FALSE,
           xlb = NULL,
           ylb = NULL){

      if(is.null(gamobj)){
          gamobj <- check.gamobj()
          if(is.null(gamobj)){
              return()
          } else {
          }
      } else {
          c1 <- call("assign",pos=1, "current.gam", gamobj,immediate=T)
          eval(c1)
      }

    if(eval(parse(text="current.gam$family$family")) == "gaussian"){
      sd <- sqrt(eval(parse(text="current.gam$deviance"))/eval(parse(text="current.gam$df.residual")))
    } else {
      sd <- 1
    }

    dev  <- residuals(eval(parse(text="current.gam")), type = "deviance")/sd
    pear <- residuals(eval(parse(text="current.gam")), type = "pearson")/sd
    h    <- lm.influence(eval(parse(text="current.gam")))$hat
    rp   <- pear/sqrt(1 - h)
    #for (i in 1:length(rp)){
    #  if(is.na(rp[i])){
    #    rp[i] <- pear[i]
    #  }
    #}

    sgn <- function(x)
      ifelse(x > 0, 1, ifelse(x < 0, -1, 0))

    res  <- sgn(dev) * sqrt(dev^2 + h * rp^2)

    pdata <- data.frame(cbind(eval(parse(text="current.gam$data[,1]")),res))
    names(pdata) <- c("ID","studres")
    studres.ord <- order(pdata$studres)
    pdata <- pdata[studres.ord,  ]
    pdata$ID <- reorder(as.factor(pdata$ID),pdata$studres)

    if(is.null(xlb))
      xlb <- "Studentized residual"
    if(is.null(ylb))
      ylb <- "ID"

    if(!is.null(title) && title == "Default") {
      title <- paste("Studentized residual of the GAM fit for ", eval(parse(text="current.gam$pars"))," (Run ",
                     eval(parse(text="current.gam$runno")), ")",sep="")
    }

#    xplot <- dotchart(pdata$studres,labels=as.character(pdata$ID),
#                      main = title, xlab = xlb,ylab=ylb,cex=0.6)

    xplot <- dotplot(ID~studres,
                     pdata,
                     main=title,
                     xlab=xlb,
                     ylab=ylb,
                     scales=list(
                       tck=-0.01,
                       y=list(cex=0.6 )
                       )
                     )
    #print(xplot)
    return(xplot)
    #invisible()

  }
