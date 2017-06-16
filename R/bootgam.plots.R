


#' Compare parameter estimates for covariate coefficients
#' 
#' This function creates a plot of the estimates for covariate coefficients,
#' obtained from the first step (univariate testing) in each scm performed in
#' the bootscm. When normalized for their standard deviation, these plots can
#' be used to compare the strength of the covariate relationship. Coloring is
#' based on the covariate being included in the final model (blue) not being
#' included (red).
#' 
#' Optionally, estimated bias is plotted in the graph (as text). Bias is also
#' shown by the difference in mean of parameter estimates when the covariate is
#' included (blue diamond), as opposed to the mean of all parameter estimates
#' (grey diamond)
#' 
#' Note: For dichotomous covariates, the default PsN implementation is to use
#' the most common covariate value as base, while the effect of the other
#' value, is estimated by a theta. Xpose (bootscm.import) however recalculates
#' the estimated parameters, to the parametrization in which the lowest value
#' of the dichotomous covariate is the base (e.g. 0), and the estimated THETA
#' denotes the proportional change, when the covariate has the other value
#' (e.g. 1).
#' 
#' 
#' @param bootgam.obj The object created using bootscm.import(), which hold the
#' data for plotting.
#' @param sd.norm Perform normalization of the covariate coefficients (default
#' is TRUE). When TRUE, the estimated covariate coefficients will be multiplied
#' by the standard deviation of the specific covariate (both for continuous and
#' categorical covariates).
#' @param by.cov.type Split the plot for continuous and dichotomous covariates.
#' Default is FALSE.
#' @param abs.values Show the covariate coefficient in absolute values. Default
#' is FALSE.
#' @param show.data Show the actual covariate coefficients in the plot. Default
#' is TRUE.
#' @param show.means Show the means of included covariates (blue) and all
#' covariates (grey) in the plot. Default is TRUE.
#' @param show.bias Show estimated bias as text in the plot. Default is TRUE.
#' @param dotpch The character used for plotting.
#' @param labels Custom labels for the parameter-covariate relationships,
#' (character vector)
#' @param xlab Custom x-axis label
#' @param ylab Custom y-axis label
#' @param pch.mean The character used for plotting the mean.
#' @param col The color scheme.
#' @param \dots Additional plotting arguments may be passed to this function.
#' @return No value returned.
#' @author Ron Keizer
#' @keywords ~bootscm
#' @examples
#' 
#'   xp.boot.par.est()
#' 
#' @export xp.boot.par.est
xp.boot.par.est <- function (bootgam.obj = NULL,
                             sd.norm = TRUE,
                             by.cov.type = FALSE,
                             abs.values = FALSE,
                             show.data = TRUE,
                             show.means = TRUE,
                             show.bias = TRUE,
                             dotpch = c(1,19),
                             labels = NULL,
                             pch.mean = "|",
                             xlab = NULL,
                             ylab = NULL,
                             col = c(rgb(.8, .5, .5), rgb(.2, .2, .7), rgb(.2,.2,.7), rgb(.6,.6,.6)),
                             ...) {
    boot.type <- "bootscm"
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
        return()
    }
    if (bootgam.obj$group.by.cov == TRUE) {
        cat ("This plot cannot be created when imported bootscm results are grouped by covariate.\nPlease re-import the bootscm results.")
        return()
    }
    if (!("par.est.first" %in% names(bootgam.obj))) {
        cat ("The required data is not available. Please check that all necessary PsN data was imported.\n")
        cat ("Note: If you've used the bootscm.import function, please set 'skip.par.est.import' to FALSE.\n\n")
        return(NULL)
    }
    if (is.null(xlab)) {
        xlab <- "Relative parameter estimate (from 1st scm-step)"
    }
    if (is.null(ylab)) {
        ylab <- "Covariate"
    }

    if (sd.norm == TRUE) {
        pl.dat <- bootgam.obj$par.est.long.norm
        bias.dat <- bootgam.obj$bias.dat.norm
    } else {
        pl.dat <- bootgam.obj$par.est.long
        bias.dat <- bootgam.obj$bias.dat
    }

    ## order by inclusion frequency
    rem <- seq(along = bootgam.obj$results.tab[,1])[bootgam.obj$failed == 1]
    cleaned.data <- bootgam.obj$results.tab
    if (length(rem)>0) {
        cleaned.data <- cleaned.data[-rem,]
    }
    incl.freq <- apply (cleaned.data, 2, sum)
    lev.ord <- names(incl.freq)[order(incl.freq)]


    lev.ord <- unlist(sapply(lev.ord,function(x) levels(pl.dat$cov)[grep(x,levels(pl.dat$cov))]),use.names = F)

    abs.fun <- function (dat) {return(dat)}
    if (abs.values == TRUE) {
        abs.fun <- abs
        xlab <- paste("Absolute", xlab)
    }
    if (by.cov.type == TRUE) {
        formula <- factor(cov, levels=lev.ord) ~ abs.fun(value) | cov.type
    } else {
        formula <- factor(cov, levels=lev.ord) ~ abs.fun(value)
    }
    if (!is.null(labels)) {
        labels <- rev(labels)
        if (length(labels)==length(lev.ord)) {
            idx1 <- match(bias.dat$cov, lev.ord)
            idx2 <- match(names(incl.freq), lev.ord)
            idx3 <- match(pl.dat$cov, lev.ord)
            bias.dat$cov <- labels[idx1]
            names(incl.freq) <- labels[idx2]
            pl.dat$cov <- labels[idx3]
            lev.ord <- names(incl.freq)[order(incl.freq)]
        } else {
            cat ("Length of specified labels-vector not equal to number of covariate-parameter relationships. Returning to default.")
        }
    }
    legend <- list(text = list("Selected", cex=.75),
                   points = list(pch=dotpch[2], col=col[3], cex=1),
                   text = list("Not selected", cex=.75),
                   points = list(pch=dotpch[1], col=col[1], cex=1) )
    if (show.means == TRUE) {
        legend <- list(text = list("Selected", cex=.75),
                       points = list(pch=dotpch[2], col=col[3], cex=1),
                       text = list("Not selected", cex=.75),
                       points = list(pch=dotpch[1], col=col[1], cex=1),
                       text = list("mean (selected)", cex=.75),
                       lines = list(lwd=1.5, span=0.1, col=col[3]),
                       text = list("mean (all)", cex=.75),
                       lines = list(lwd=1.5, span=0.1, col=col[4])
                   )
    }
    p <- stripplot (formula,
                    data = pl.dat,
                    ylab = ylab,
                    xlab = xlab,
                    groups = factor(eval(as.name("incl")), levels = c("Not included", "Included")),
                    par.settings = simpleTheme (col=col, pch=dotpch),
                    key = legend,
                    levels = lev.ord,
                    panel = function (...) {
                        panel.abline (v=0, lty=3)
                        if (show.data == TRUE) {
                            panel.stripplot (jitter.data=TRUE, ...)
                        }
                        if (show.means == TRUE) {
                            panel.xyplot (y = factor(bias.dat[bias.dat$incl == "Included",]$cov, levels=lev.ord),
                                          x = abs.fun(as.num(bias.dat[bias.dat$incl == "Included",]$mean)),
                                          bias.data=bias.dat, pch = pch.mean, cex=2.5, col=col[3]
                                          )
                            panel.xyplot (y = factor(bias.dat[bias.dat$incl == "Included",]$cov, levels=lev.ord),
                                          x = abs.fun(as.num(bias.dat[bias.dat$incl == "Included",]$All)),
                                          bias.data=bias.dat, pch = pch.mean, cex=2.5, col=col[4]
                                          )
                        }
                        if (show.bias == TRUE) {
                            panel.text (y = factor(bias.dat[bias.dat$incl=="Included",]$cov, levels=lev.ord),
                                        x = abs.fun(max(pl.dat[!is.na(pl.dat$value),]$value)*0.94),
                                        labels = paste (round(bias.dat[bias.dat$incl=="Included",]$bias,0), "%", sep=""), cex=0.8)
                        }
                    }, ...)
    return(p)
}

ask.covs.plot <- function (bootgam.obj = NULL) {
    if (!is.null(bootgam.obj)) {
        cat ("Covariates in database: ")
        covs <- colnames(bootgam.obj$covariate$sd.all)
        cat (covs)
        cat ("\n\nPlot for which covariates (separate by space, return for all): ")
        ans <- readline()
        if (ans == "") {
            return()
        }
        ans.cov <- strsplit(ans, " ")[[1]]

        if (length(ans.cov) < 2) {
            cat("Please choose at least 2 covariatess from the list!\n\n")
            Recall(bootgam.obj)
        } else {
            if (sum((ans.cov %in% covs)*1) == length(ans.cov)) {
                return (ans.cov)
            } else {
                cat("Please choose covariates from the list only!\n\n")
                Recall(bootgam.obj)
            }
        }
    }
}



#' Correlations between covariate coefficients
#' 
#' This function creates a plot showing the correlations in estimates for
#' covariate coefficients, obtained from the first step (univariate testing) in
#' each scm performed in the bootscm.
#' 
#' 
#' @param bootgam.obj The object created using bootscm.import(), which hold the
#' data for plotting.
#' @param sd.norm Perform normalization of the covariate coefficients (default
#' is TRUE). When TRUE, the estimated covariate coefficients will be multiplied
#' by the standard deviation of the specific covariate (both for continuous and
#' categorical covariates).
#' @param by.cov.type Split the plot for continuous and dichotomous covariates.
#' Default is FALSE.
#' @param cov.plot A character vector which lists the covariates to include in
#' the plot. If none are specified (NULL), all covariate coefficients will be
#' included in the plot.
#' @param ask.covs Ask the user which covariates to include in the plot.
#' Default is FALSE.
#' @param dotpch The character used for plotting.
#' @param col The colors used for plotting.
#' @param \dots Additional plotting arguments may be passed to this function.
#' @return No value returned.
#' @author Ron Keizer
#' @keywords ~bootscm
#' @examples
#' 
#' \dontrun{
#' xp.boot.par.est.corr(current.bootscm, sd.norm = TRUE,
#'                           cov.plot = c("CLSEX", "VSEX", "CLWT"))
#' 
#' }
#' @export xp.boot.par.est.corr
xp.boot.par.est.corr <- function (bootgam.obj = NULL,
                                  sd.norm = TRUE,
                                  by.cov.type = FALSE,
                                  cov.plot = NULL, # covariates to plot if not all are wanted
                                  ask.covs = FALSE,
                                  dotpch = 19,
                                  col = rgb(.2, .2, .9, .75),
                                  ...) {
    boot.type <- "bootscm"
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
        return()
    }
    if (bootgam.obj$group.by.cov == TRUE) {
        cat ("This plot cannot be created when imported bootscm results are grouped by covariate.\nPlease re-import the bootscm results.")
        return()
    }
    if (!("par.est.first" %in% names(bootgam.obj))) {
        cat ("The required data is not available. Please check that all necessary PsN data was imported.\n")
        cat ("Note: If you've used the bootscm.import function, please set 'skip.par.est.import' to FALSE.\n\n")
        return(NULL)
    }
    tmp <- bootgam.obj$par.est.first
    if (sd.norm == TRUE) { # for non-dichotomous covariates, do correction
        tmp <- bootgam.obj$par.est.first.corr
        xlab <- "Parameter estimate (from 1st scm-step), SD-normalized"
    }
    pl.dat <- tmp
    pl.dat.incl <- (!is.na(bootgam.obj$par.est.final))*1

    ## filter out the desired covariates
    if (is.null(cov.plot)) {
        if (ask.covs==TRUE) {
            cov.plot <- ask.covs.plot (bootgam.obj)
        }
    }
    if ((!is.null(cov.plot))&&(sum(cov.plot %in% colnames(tmp))>0)) {
        pl.dat <- tmp[,cov.plot]
        pl.dat.incl <- pl.dat.incl[,cov.plot]
    }

    p <- splom (pl.dat, pch = dotpch, col=col)
    return(p)
}



#' Print summary information for a bootgam or bootscm
#' 
#' This functions prints some summary information for a bootgam performed in
#' Xpose, or for a bootscm performed in PsN.
#' 
#' 
#' @param bootgam.obj The bootgam or bootscm object.
#' @return No value returned
#' @author Ron Keizer
#' @keywords ~bootgam ~bootscm
#' @examples
#' 
#' \dontrun{
#' bootgam.print(current.bootgam)  # Print summary for the current Xpose bootgam object
#' bootgam.print(current.bootscm)  # Print summary for the current Xpose bootscm object
#' }
#' 
#' @export bootgam.print
bootgam.print <- function(bootgam.obj = NULL) {
    bootgam.obj <- get.boot.obj(bootgam.obj, NULL)
    if (is.null(bootgam.obj)) {
        return()
    }
    boot.type <- get.boot.type (bootgam.obj)
    cat("\n********************************************************************\n")
    if (boot.type == "bootgam") {
        cat("************************* BootGAM results **************************\n")
    } else {
        cat("************************* BootSCM results **************************\n")
    }
    cat("Run number:", bootgam.obj$runno, "\n")
    failed <- NULL
    if (boot.type == "bootgam") {
        if(is.null(startm <- bootgam.obj$start.mod)) {
            cat("No start model specified.\n")
        } else {
            cat("Start model set to:", startm,"\n")
        }
        cat("Seed number:", bootgam.obj$seed,"\n")
        if(length(bootgam.obj$excluded.ids)>0) {
            cat("Excluded individuals:",bootgam.obj$excluded.ids,"\n")
        } else {
            cat("No individuals were excluded.\n")
        }
        cat("\nConvergence algorithm:", bootgam.obj$algo,"\n")
        if(bootgam.obj$algo == "fluct.ratio") {
            cat("Lowest important inclusion frequency:")
            cat("\n  Convergence criterium:", format(bootgam.obj$fluct.ratio.last, digits = 5), "(target=", bootgam.obj$conv.value, ")\n")
        } else {
            cat("Lowest absolute joint inclusion frequency:")
            cat("\n  Convergence criterium:", format(bootgam.obj$ljif.last, digits = 5), "(target=", bootgam.obj$ljif, ")\n")
        }
        failed <- seq(along=eval(as.name("current.bootgam"))$failed)[eval(as.name("current.bootgam"))$failed==1]
        cat ("Failed BootGAM replicates: ", failed, "\n")
    }
    cat("\nTotal number of iterations:", length(bootgam.obj$results.tab[,1]), "\n")
    cat("\nModel size: ")
    res <- bootgam.obj$results.tab
    if (!is.null(failed)) {
        if (length(failed)>0) {
            res <- bootgam.obj$results.tab[-failed,]
        }
    }
    print (summary(apply(res, 1, sum)))
    cat("\nInclusion probabilities:\n")
    tot.prob <- tail(bootgam.obj$incl.freq,1)
    ord <- rev(order(tot.prob))
    print(t(as.list(round(tot.prob[ord],3))))
    cat("********************************************************************\n\n")
}

check.bootgamobj <- function () {
    getit <- function() {
        cat("\nYou have to specify the parameter name and the run number",
            "of the bootgam objects you want to plot. The following",
            "bootgam objects are available:\n", fill = 60)
        if (.Platform$OS == "windows") {
            cat(objects(pattern = "bootgam.xpose*", pos = 1), fill = 60)
        }
        else {
            cat(objects(pattern = "^bootgam.xpose", pos = 1), fill = 60)
        }
        cat("\nParameter (0 to exit): ")
        ans <- readline()
        if (ans == 0) {
            return(ans <- NULL)
        }
        cat("Run number (0 to exit):")
        ans1 <- readline()
        if (ans1 == 0) {
            return(ans1 <- NULL)
        }
        gobjname <- paste("bootgam.xpose.", ans, ".", ans1, sep = "")
        if (!exists(gobjname, where = 1)) {
            cat("\n*There are no objects that matches", gobjname,
                "\n")
            gobjname <- Recall()
        }
        return(gobjname)
    }

    if (exists("current.bootgam", where = 1)) {
      cur.boot <- eval(as.name("current.bootgam"))
      cat("\nThe current bootgam object is for", cur.boot$parnam,
          "in run", cur.boot$runno, ".\n")
      cat("\nDo you want to proceed with this bootgam object? y(n) ")
      ans <- readline()
      if (ans != "y" && ans != "") {
        gobjname <- getit()
        if (!is.null(gobjname)) {
          c1 <- call("assign",pos = 1, "current.bootgam", eval(as.name(gobjname)),
                 immediate = T)
          eval(c1)
        }
      } else {
        gobjname <- T
      }
    } else {
        gobjname <- getit()
        if (!is.null(gobjname)) {
            c2 <- call("assign",pos = 1, "current.bootgam", eval(as.name(gobjname)),
                   immediate = T)
            eval(c2)
        }
    }
    return(gobjname)
}

ask.bootgam.bootscm.type <- function () {
    cat ("Both a bootgam and a bootscm object are available, which one\nwould you like to summarize?\n")
    cat ("  1) the current bootgam object\n")
    cat ("  2) the current bootscm object\n")
    ans <- readline()
    if (ans == "") {
        Recall()
    } else {
        if ((ans == 1)|(ans == 2)) {
            if (ans == 1) {return ("bootgam")}
            if (ans == 2) {return ("bootscm")}
        } else {
            cat("Please choose either 1 or 2!\n\n")
            Recall()
        }
    }
}

get.boot.obj <- function (bootgam.obj = NULL,
                          boot.type = NULL
                          ) {
                                        # switch between supplied object or global object, and bootscm/bootgam
    if ((is.null(boot.type))&(is.null(bootgam.obj))) {
        if (("current.bootgam" %in% ls(.GlobalEnv))&(!"current.bootscm" %in% ls(.GlobalEnv))) {
            boot.type <- "bootgam"
        }
        if (("current.bootscm" %in% ls(.GlobalEnv))&(!"current.bootgam" %in% ls(.GlobalEnv))) {
            boot.type <- "bootscm"
        }
        if (("current.bootscm" %in% ls(.GlobalEnv))&("current.bootgam" %in% ls(.GlobalEnv))) {
            boot.type <- ask.bootgam.bootscm.type()
            cat ("\n")
        }
        if (is.null(boot.type)) {
            cat ("No bootgam or bootscm object found!\n")
            return()
        }
    }
    if (is.null(boot.type)) {
        boot.type <- get.boot.type (bootgam.obj)
    }
    if (boot.type == "bootscm") {
        if (is.null(bootgam.obj)) {
            if ("current.bootscm" %in% objects(pos=1)) {
                if (!is.null(eval(as.name("current.bootscm")))) {
                    bootgam.obj <- eval(as.name("current.bootscm"))
                } else {
                    cat ("Data not available. Did you import the bootSCM data?\n")
                }
            } else {
                cat (paste(objects()))
                cat ("Data not available. Did you import the bootSCM data?\n")
            }
        } else {
            c3 <- call("assign",pos = 1, "current.bootscm", bootgam.obj, immediate = T)
            eval(c3)
        }
    } else { # load bootgam object
        if (is.null(bootgam.obj)) {
            if ("current.bootgam" %in% objects()) {
                if (!is.null(bootgam.obj)) {
                    bootgam.obj <- eval(as.name("current.bootgam"))
                }
            } else {
                if (check.bootgamobj()) {
                    bootgam.obj <- eval(as.name("current.bootgam"))
                } else {
                    cat ("Data not available. Did you run the bootGAM data?\n")
                }
            }
        } else {
            c4 <- call("assign",pos = 1, "current.bootgam", bootgam.obj, immediate = T)
            eval(c4)
        }
    }
    return(bootgam.obj)
}



#' Plot of model size distribution for a bootgam or bootscm
#' 
#' This function creates a kernel smoothed plot of the number of covariates
#' included in the final model in each gam/scm in the bootgam/bootscm
#' procedure.
#' 
#' 
#' @param bootgam.obj The bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means
#' the user will be asked to make a choice.
#' @param main Plot title.
#' @param bw The smoothing bandwidth to be used for the kernel.
#' @param xlb The x-axis label.
#' @param \dots Additional plotting parameter may be passed to this function.
#' @return A lattice plot object will be returned.
#' @author Ron Keizer
#' @keywords ~bootgam ~bootscm
#' @export xp.distr.mod.size
xp.distr.mod.size <- function (bootgam.obj = NULL,
                               boot.type = NULL,
                               main = NULL,
                               bw = 0.5,
                               xlb = NULL,
                               ... ) {
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
        return()
    }
    boot.type <- get.boot.type (bootgam.obj)

    ## Sort out the titles
    if(is.null(main)) {
        main <- paste("Distribution of covariate model sizes", bootgam.obj$runno)
    }
    if (is.null(xlb)) {
        if (boot.type == "bootgam") {
            xlb <- paste ("Covariate model size (on", bootgam.obj$parnam, ")", sep = "")
        } else {
            xlb <- paste ("Covariate model size (on any parameter)")
        }
    }
                                        # Plot
    res <- bootgam.obj$results.tab
    if (!is.null(bootgam.obj$failed)) {
        res <- res[bootgam.obj$failed == 0,]
    }
    sizes <- apply (res, 1, sum)
    pl <- densityplot (sizes,
                       bw = bw,
                       main = main,
                       ... )
    return(pl)
}

ask.incl.range <- function (bootgam.obj = NULL) {
    text <- paste("The plots that show correlations between covariate inclusion\n",
                  "frequencies (inclusion index) are not informative when the inclusion\n",
                  "frequency for a covariate is either very high or very low. Therefore\n",
                  "it is advised to show these plots only for intermediately strong \n",
                  "covariates. The default range is 20% to 80%.\n\n", sep="")
    cat (text)
    cat ("Specify range (e.g.: 20 80): ")
    ans <- readline()
    if (ans == "") {
        range <- c(20,80)
    } else {
        range <- as.numeric(strsplit (ans, " ")[[1]])
    }
    if (length(range) == 2) {
        return (range)
    } else {
        cat("Please specify two numbers, separated by a space!\n\n")
        Recall(bootgam.obj)
    }
}

#' Plot of inclusion index of covariates.
#' 
#' Covariate inclusion indices show the correlation in inclusion of a covariate
#' in the final model in a bootgam or bootscm.
#' 
#' @param bootgam.obj The bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means
#'   the user will be asked to make a choice.
#' @param main Plot title.
#' @param xlb Label for the x-axis.
#' @param ylb Label for the y-axis.
#' @param add.ci Add a confidence interval to the plotted data.
#' @param incl.range Included range
#' @param return_plot Should the function return a plot?
#' @param results.tab Specify your own results table.
#' @param ...  Additional plotting information.
#'   
#' @return A lattice plot object is returned.
#' @author Ron Keizer
#' @export
#' 
#' @family bootgam
#' @family bootscm
xp.incl.index.cov <- function (
  bootgam.obj = NULL,
  boot.type = NULL,
  main = NULL,
  xlb = "Index",
  ylb = "Covariate",
  add.ci = FALSE,
  incl.range = NULL,
  return_plot = TRUE,
  results.tab = NULL,
  ...) {
  bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
  if (is.null(bootgam.obj)) {
    return()
  }
  boot.type <- get.boot.type(bootgam.obj)
  as.num <- function(dat) {
    return(as.numeric(as.character(dat)))
  }
  if (is.null(main)) {
    main <- paste("Inclusion index for", bootgam.obj$runno)
  }
  se_idx <- function(p, q, n) {
    A <- (p/n) * (1 - (p/n))/n
    B <- (q/n) * (1 - (q/n))/n
    rho <- 1
    se <- sqrt(A + B + 2 * sqrt(A) * sqrt(B) * rho)
    return(se)
  }
  inc_obs <- tail(bootgam.obj$incl.freq, 1)
  if(!is.null(results.tab)) {
    res <- results.tab
  } else {
    res <- bootgam.obj$results.tab
  }
  if (is.null(incl.range)) {
    incl.range <- ask.incl.range()
  }
  if (length(incl.range) == 2) {
    filter <- inc_obs > incl.range[1]/100 & inc_obs < incl.range[2]/100
    res <- res[, filter]
    inc_obs <- inc_obs[, filter]
  }
  n_cov <- length(inc_obs)
  nam <- names(inc_obs)
  if (!is.null(bootgam.obj$failed)) {
    res <- res[bootgam.obj$failed == 0, ]
  }
  if (boot.type == "bootscm") {
    cols.dum <- grep("^X.", colnames(res))
    if (length(cols.dum) > 0) {
      res <- res[, -cols.dum]
    }
  }
  cov_idx <- c()
  n <- length(res[, 1])
  for (i in 1:n_cov) {
    sub <- res[res[, i] == 1, ]
    obs <- apply(sub, 2, sum)
    expect <- inc_obs * n
    idx <- as.num((obs/n)) - (as.num(inc_obs[i])*as.num(inc_obs))
    idx[i] <- NA
    se <- 0
    # RK: removed for now, not correct and problably not useful
    # if (add.ci == TRUE) {
    #   se <- unlist(se_idx(p = obs, q = expect, n = length(res[, 1])))
    # }
    cov_idx <- data.frame(rbind(cov_idx, cbind(COV1 = nam[i],
                                               COV2 = nam, idx, se, lbnd = (idx - (1.96 * se)),
                                               ubnd = (idx + (1.96 * se)))))
  }
  if(return_plot) {
    p <- dotplot(as.factor(COV1) ~ as.num(idx) | as.factor(COV2),
                 data = cov_idx, plot.zero = TRUE, main = main, xlab = xlb,
                 ylab = ylb, lx = as.num(cov_idx$lbnd), ux = as.num(cov_idx$ubnd),
                 prepanel = prepanel.ci,
                 panel = panel.ci,
                 ...)
  } else {
    return(cov_idx)
  }
  return(p)
}

ask.cov.name <- function (bootgam.obj = NULL) {
  if (!is.null(bootgam.obj)) {
    cat ("Covariates in database: ")
    cat (paste (bootgam.obj$covnams))
    cat ("\n\nPlot for which covariate (return to exit): ")
    ans <- readline()
    if (ans == "") {
      return()
    }
    if (ans %in% (bootgam.obj$covnams)) {
      return (ans)
    } else {
      cat("Please choose a covariate from the list!\n\n")
      Recall(bootgam.obj)
    }
  }
}


#' Individual inclusion index
#'
#' This function will generate a plot of individual inclusion indexes for a
#' specific covariate, which can be used to identify influential
#' individuals for inclusion of that covariate. The index for an individual is calculated as
#' the observed number of inclusions of that individual when the specific
#' covariate was included minus the expected number of inclusions (based
#' on the total bootstrap inclusions), divided by expected.
#'
#' @param bootgam.obj A bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means the user
#' will be asked to make a choice.
#' @param cov.name The name of the covariate for which to create the plot.
#' @param main The title of the plot.
#' @param ylb The label for the x-axis.
#' @param xlb The label for the y-axis.
#' @param return_plot Should a plot object be returned?
#' @param results.tab Supply your own results table.
#' @param ... Additional plotting parameters.
#'
#' @return A lattice plot object is returned.
#' @author Ron Keizer
#' @export
#' 
#' @family bootgam
#' @family bootscm
# @examples
xp.incl.index.cov.ind <- function (bootgam.obj = NULL,
                                   boot.type = NULL,
                                   cov.name = NULL,
                                   main = NULL,
                                   ylb = "ID",
                                   xlb = "Individual inclusion index",
                                   return_plot = TRUE,
                                   results.tab = NULL,
                                   ... ) {
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
      return()
    }
    boot.type <- get.boot.type (bootgam.obj)

    as.num <- function (dat) { return (as.numeric(as.character(dat))) }

    if (is.null(cov.name)) {
      cov.name <- ask.cov.name(bootgam.obj)
    }
    if (is.null(cov.name)) { return() }

    if(is.null(main)) {
      main <- paste ("Individual inclusion index (", cov.name, " on ", bootgam.obj$parnam, ") for ", bootgam.obj$runno, sep="")
    }
    if(!is.null(results.tab)) {
      res <- results.tab
      bootgam.obj$oid <- bootgam.obj$oid[1:length(results.tab[,1]),]
    } else {
      res <- bootgam.obj$results.tab
    }

    ids <- colnames(bootgam.obj$oid)
    oid.cnt <- apply (bootgam.obj$oid, 2, sum)

    if (!is.null(bootgam.obj$failed)) {
      res <- res[bootgam.obj$failed == 0,]
    }
    oid.rel <- oid.cnt / length(res[,1])
    nam <- names(res)

    cov_idx <- c()
    sub <- bootgam.obj$oid[res[, cov.name == nam]==1,]
    obs <- apply (sub, 2, sum)
    n <- length(sub[,1])
    idx <- (as.num(obs) / (n * as.num(oid.rel))) - 1
    ord <- order(idx)
    ids <- as.num(gsub("X","", ids))

    cov_idx <- data.frame(cbind ("idn" = ids[ord], "idx" = as.num(idx[ord])))
    scales <- list(y = list (labels = rev(cov_idx$idn)), cex=c(0.7,1))
    if(return_plot) {
      p <- xyplot (factor(idn, levels=rev(idn)) ~ as.num(idx),
                   data = cov_idx,
                   main = main,
                   xlab = xlb,
                   ylab = ylb,
                   scales = scales,
                   lx = 0, ux = 0, plot.zero=TRUE,
                   prepanel = prepanel.ci,
                   panel = panel.ci,
                   ... )
      return (p)
    } else {
      return(cov_idx)
    }
}



#' Inclusion index individuals, compare between covariates.
#' 
#' A plot showing the range of inclusion indices for individuals for all
#' covariates. This plot can be used to evaluate whether there were covariates
#' which were more influenced by the constituency of the bootstrapped dataset
#' than others.
#' 
#' 
#' @param bootgam.obj A bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means
#' the user will be asked to make a choice.
#' @param main The title of the plot.
#' @param xlb The label for the x-axis.
#' @param ylb The label for the y-axis.
#' @param \dots Additional plotting parameters.
#' @return A lattice plot object is returned.
#' @author Ron Keizer
#' @keywords ~bootgam ~bootscm
#' @export xp.incl.index.cov.comp
xp.incl.index.cov.comp <- function (bootgam.obj = NULL,
                                    boot.type = NULL,
                                    main = NULL,
                                    xlb = "Individual inclusion index",
                                    ylb = "ID",
                                    ... ) {
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
        return()
    }
    as.num <- function (dat) { return (as.numeric(as.character(dat))) }
    boot.type <- get.boot.type (bootgam.obj)

    if(is.null(main)) {
        main <- paste ("Individual inclusion indices for", bootgam.obj$runno)
    }

    ids <- colnames(bootgam.obj$oid)
    oid.cnt <- apply (bootgam.obj$oid, 2, sum)
    res <- bootgam.obj$results.tab
    if (!is.null(bootgam.obj$failed)) {
        res <- res[bootgam.obj$failed == 0,]
    }
    oid.rel <- oid.cnt / length(res[,1])
    nam <- names(res)

    cov_idx <- c()
    for (i in seq(along=nam)) {
        sub <- bootgam.obj$oid[res[,nam[i] == nam]==1,]
        obs <- apply (sub, 2, sum)
        n <- length(sub[,1])
        idx <- (as.num(obs) / (n * as.num(oid.rel))) - 1
        cov_idx <- data.frame(rbind (cov_idx, cbind ("cov" = nam[i], "idx" = as.num(idx))))
    }

    p <- xyplot (factor(cov) ~ as.num(idx),
                 data=cov_idx,
                 xlab = xlb,
                 ylab = ylb,
                 main = main,
                 lx = 0, ux = 0,
                 plot.zero = TRUE,
                 prepanel = prepanel.ci,
                 panel = panel.ci,
                 ... )
    return (p)
}



#' Inclusion frequency plot
#' 
#' Plot the inclusion frequencies of covariates in the final models obtained in
#' a bootgam or bootscm. Covariates are ordered by inclusion frequency.
#' 
#' 
#' @param bootgam.obj The bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means
#' the user will be asked to make a choice.
#' @param main Plot title
#' @param col Color used for the plot.
#' @param xlb Label for x-axis.
#' @param ylb Label for y-axis.
#' @param \dots Additional plotting parameters.
#' @return A lattice plot object will be returned.
#' @author Ron Keizer
#' @keywords ~bootgam ~bootscm
#' @export xp.inc.prob
xp.inc.prob <- function (bootgam.obj = NULL,
                         boot.type = NULL,
                         main = NULL,
                         col = "#6495ED",
                         xlb = NULL,
                         ylb = "Covariate",
                         ... ) {
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
        return()
    }
    boot.type <- get.boot.type (bootgam.obj)

    ## Sort out the titles
    if(is.null(main)) {
        main <- paste("Total frequency of covariates for", bootgam.obj$runno)
    }

    rem <- seq(along = bootgam.obj$results.tab[,1])[bootgam.obj$failed == 1]
    cleaned.data <- bootgam.obj$results.tab
    if (length(rem)>0) {
        cleaned.data <- bootgam.obj$results.tab[-rem,]
    }
    frac <- function (data) { sum (data) / length(data) }
    se <- function (data) {
        p <-  sum (data) / length(data)
        se <- p * (1-p) / length(data)
        return (se)
    }
    as.num <- function (data) { return (as.numeric(as.character(data)))}

    cov.prob <- apply (cleaned.data, 2, frac)
    cov.prob <- cov.prob[order(cov.prob)]
    cov.se <- apply (cleaned.data, 2, se)
    cov.se <- cov.se[order(cov.prob)]
    cov.ci <- cbind ("ubnd" = cov.prob + 1.96*cov.se, "lbnd" = cov.prob - 1.96*cov.se)
    cov.comb <- data.frame ( cbind ( "cov" = names(cov.prob), "prob" = cov.prob, cov.ci) )
    cov.comb <- cov.comb[order(cov.comb$prob),]

    if (is.null(xlb)) {
        xlb <- paste("Inclusion frequency (%) on ", bootgam.obj$parnam, sep="")
        if (boot.type == "bootscm") {
            xlb <- "Inclusion frequency (%)"
        }
    }

    pl <- xyplot (factor(cov, levels=cov) ~ 100*as.num(prob),
                  lx = as.num(cov.comb$lbnd), ux = as.num(cov.comb$ubnd),
                  data = cov.comb,
                  prepanel = prepanel.ci,
                  panel = panel.ci,
                  main = main,
                  xlim = c(0,100),
                  xlab = xlb,
                  ylab = ylb,
                  ... )
    return(pl)
}



#' Inclusion frequency plot for combination of covariates.
#' 
#' Plot the inclusion frequency of the most common 2-covariate combinations.
#' 
#' 
#' @param bootgam.obj The bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means
#' the user will be asked to make a choice.
#' @param main Plot title
#' @param col Color used for plot.
#' @param xlb Label for x-axis.
#' @param ylb Label for y-axis.
#' @param \dots Additional plotting parameters.
#' @return A lattice plot object will be returned.
#' @author Ron Keizer
#' @keywords ~bootgam ~bootscm
#' @export xp.inc.prob.comb.2
xp.inc.prob.comb.2 <- function (bootgam.obj = NULL,
                                boot.type = NULL,
                                main = NULL,
                                col = "#6495ED",
                                xlb = NULL,
                                ylb = "Covariate combination",
                                ... ) {
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
        return()
    }
    if(is.null(main)) {
        main <- paste("Most common 2-covariate combinations for", bootgam.obj$runno)
    }
    boot.type <- get.boot.type (bootgam.obj)

    rem <- seq(along = bootgam.obj$results.tab[,1])[bootgam.obj$failed == 1]
    cleaned.data <- bootgam.obj$results.tab
    if (length(rem) > 0) {
        cleaned.data <- cleaned.data[-rem,]
    }

    frac <- function (data) { sum (data) / length(data) }
    se <- function (data) {
        p <-  sum (data) / length(data)
        se <- p * (1-p) / length(data)
        return (se)
    }
    as.num <- function (data) { return (as.numeric(as.character(data)))}

    covs <- colnames(cleaned.data)
    cov_all <- c()
    for (i in seq(along=covs)) {
        tmp <- cleaned.data[cleaned.data[,i] == 1, -i]
        cov.prob <- apply (tmp, 2, frac)
        cov_all <- data.frame (rbind (cov_all, cbind ("cov1"=covs[i], "cov2" = names(cov.prob), "idx" = as.num(cov.prob))))
    }
    cov_all$idx <- as.num(cov_all$idx)
    cov_all_10 <- head(cov_all[order(cov_all$idx, decreasing=TRUE),], 10)
    cov_all_10$label <- paste(cov_all_10$cov1, "+", cov_all_10$cov2)

    if (is.null(xlb)) {
        xlb <- paste("Inclusion frequency (%) on ", bootgam.obj$parnam, sep="")
        if (boot.type == "bootscm") {
            xlb <- "Inclusion frequency (%) on any parameter)"
        }
    }
    pl <- dotplot (factor(label, levels=rev(cov_all_10$label)) ~ 100*as.num(idx),
                   lx = 0, ux=0,
                   data = cov_all_10,
                   prepanel = prepanel.ci,
                   panel = panel.ci,
                   xlim = c(0,100),
                   main = main,
                   xlab = xlb,
                   ylab = ylb,
                   ...)
    return(pl)
}

prepanel.ci <- function(x, y, lx, ux, subscripts, ...) {
    x <- as.numeric(x)
    lx <- as.numeric(lx[subscripts])
    ux <- as.numeric(ux[subscripts])
    list(xlim = range(x, ux, lx, finite = TRUE))
}

panel.ci <- function(x, y, lx, ux, subscripts, pch = 16, plot.zero = FALSE, ...) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    lx <- as.numeric(lx[subscripts])
    ux <- as.numeric(ux[subscripts])
    if (plot.zero == TRUE) {
        panel.abline (v = 0, lty = 3, lwd = 1, col="#999999")
    }
    panel.abline(h = unique(y), col = "grey", lwd = 1)
                                        # show SE of estimate. Disabled.
                                        #    panel.arrows(lx, y, ux, y, col = 'black', lwd = 2,
                                        #                 length = 0, unit = "native",
                                        #                 angle = 90, code = 3)
    panel.xyplot(x, y, pch = pch, ...)
}

#'   Inclusion stability plot
#'   
#'   A plot of the inclusion frequency of covariates vs bootgam/bootscm
#'   iteration number. This plot can be used to evaluate whether sufficient
#'   iterations have been performed.
#'
#' @param bootgam.obj The bootgam or bootscm object.
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, 
#' which means the user will be asked to make a choice.
#' @param main Plot title
#' @param normalize Should the plot be normalized?
#' @param split.plots Should the plots be split?
#' @param xlb The label for the x-axis.
#' @param ylb The label for the y-axis.
#' @param ... Additional plotting parameters
#'
#' @return A lattice plot object is returned.
#' @author Ron Keizer
#' @export
#'
#' @family bootgam
#' @family bootscm
#' 
# @examples
xp.inc.stab.cov <- function (bootgam.obj = NULL,
                             boot.type = NULL,
                             main = NULL,
                             normalize = TRUE,
                             split.plots = FALSE,
                             xlb = "Bootstrap replicate number",
                             ylb = "Difference of estimate with final",
                             ...) {
  
  var <- NULL
  
  ## Create a plot of d(inclusion frequency-final inclusion freq) versus bootstrap replicate number (x)
  bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
  if (is.null(bootgam.obj)) {
    return()
  }
  boot.type <- get.boot.type (bootgam.obj)

  if(is.null(main) && !is.null(bootgam.obj$runno) && bootgam.obj$runno != "") {
    main <- paste("Inclusion stability for", bootgam.obj$runno)
  }
  freq <- bootgam.obj$incl.freq
  if(normalize) {
    freq <- apply(bootgam.obj$incl.freq, 2, function(x) { x - tail(x,1) } )
  }
  if (!is.null(bootgam.obj$failed)) {
    freq <- freq[bootgam.obj$failed==0,]
  }
  freq <- data.frame (cbind (row = seq(along = freq[,1]), freq))
  freq.long <- reshape (freq,
                        ids=row.names(freq), varying = names(freq)[-1],
                        idvar = "row", timevar = "var", v.names = "value",
                        times = names(freq)[-1], direction="long")
  if(split.plots) {
    pl <- xyplot (value ~ row | var,
                  data = freq.long,
                  main = main,
                  xlab = xlb,
                  ylab = ylb,
                  type = "l",
                  panel=function(...) {
                    panel.abline(h = 0, col="#888888")
                    panel.xyplot(...)
                  },
                  ...)
  } else {
    pl <- xyplot (value ~ row,
                  groups = var,
                  col = rgb(0.4, 0.4, 0.4, 0.7),
                  data = freq.long,
                  main = main,
                  xlab = xlb,
                  ylab = ylb,
                  type = "l",
                  panel=function(...) {
                    panel.abline(h = 0, col='steelblue', lwd=2)
                    panel.xyplot(...)
                  },
                  ...)
  }
  return (pl)
}



#' OFV difference (optimism) plot.
#' 
#' A plot of the difference in OFV between final bootscm models and the
#' reference final scm model.
#' 
#' 
#' @param bootscm.obj The bootgam or bootscm object.
#' @param main Plot title.
#' @param xlb Label for x-axis.
#' @param ylb Label for y-axis.
#' @param \dots Additional plotting parameters.
#' @return A lattice plot object is returned.
#' @author Ron Keizer
#' @keywords ~bootgam ~bootscm
#' @export xp.dofv.plot
xp.dofv.plot <- function (bootscm.obj = NULL,
                          main = NULL,
                          xlb = "Difference in OFV",
                          ylb = "Density",
                          ... ) {
    bootscm.obj <- get.boot.obj(bootscm.obj, boot.type = "bootscm")
    if (is.null(bootscm.obj)) {
        return()
    }

    ## Sort out the titles
    if(is.null(main)) {
        main <- paste("Distribution of dOFV for", bootscm.obj$runno)
    }

                                        # Plot
    dofv <- bootscm.obj$dofv[!is.na(bootscm.obj$dofv$dOFV),]$dOFV
    dofv <- dofv[-1]
    pl <- densityplot (dofv, lwd=3,
                       main=main,
                       xlab = xlb,
                       ylab = ylb,
                       panel = function () {
                           panel.abline (v=0, lty=3, col="#888888")
                           panel.densityplot (dofv)
                       },
                       ... )
    return (pl)
}

get.boot.type <- function (bootscm.obj) {
    boot.type <- "bootgam"
    if ("dofv" %in% names(bootscm.obj)) {
        boot.type <- "bootscm"
    }
    return(boot.type)
}

#' Trace plots for conditional indices
#'
#' @inheritParams xp.dofv.npar.plot
#' @inheritParams xp.inc.stab.cov
#' @param boot.type Either "bootgam" or "bootscm". Default is NULL, which means
#'   the user will be asked to make a choice.
#' @param normalize Should one normalize?
#' @param split.plots Should the plots be split?
#'
#' @return A lattice plot object.
#' @export
#'
#' @family bootgam
#' @family bootscm
# @examples
xp.inc.cond.stab.cov <- function (
  ## trace plots for conditional indices
  bootgam.obj = NULL,
  boot.type = NULL,
  main = NULL,
  xlb = "Bootstrap replicate number",
  ylb = "Conditional inclusion frequency",
  normalize = TRUE,
  split.plots = FALSE,
  ...) {
  
    label <- NULL
    var <- NULL
    
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
      return()
    }
    boot.type <- get.boot.type(bootgam.obj)
    if (is.null(main) && !is.null(bootgam.obj$runno) && bootgam.obj != "") {
      main <- paste("Conditional index stability for", bootgam.obj$runno)
    }

    ## get inclusion frequency from bootscm object
    res <- c()
    for(i in 1:length(bootgam.obj$incl.freq[,1])) {
      tmp <- xp.incl.index.cov(bootgam.obj = bootgam.obj, return_plot = FALSE, results.tab = bootgam.obj$results.tab[1:i,], incl.range = c(20,80))
      tmp <- tmp[tmp$COV1 != tmp$COV2,]
      res <- rbind(res, cbind(i, as.character(tmp$COV1), as.character(tmp$COV2), as.numeric(as.character(tmp$idx))))
    }
    res <- data.frame(res)
    colnames(res) <- c("id", "COV1", "COV2", "value")
    res$id <- as.numeric(as.character(res$id))
    res$value <- as.numeric(as.character(res$value))
    res$label <- paste0(res$COV1, "-", res$COV2)
    if(normalize) {
      unq <- unique(res$label)
      lst <- res[res$id == max(res$id),]
      for(i in seq(unique(res$label))) {
        res[res$label == unq[i],]$value <- res[res$label == unq[i],]$value - lst[lst$label == unq[i],]$value
      }
    }
    if(split.plots) {
      pl <- xyplot(value ~ id | factor(label), data = res, main = main,
                   xlab = xlb, ylab = ylb, type = "l",
                   panel=function(...) {
                     panel.abline(h = 0, col="#888888")
                     panel.xyplot(...)
                   }, ...)
    } else {
      pl <- xyplot(value ~ id, data = res, main = main,
                   groups = label,
                   col = rgb(0.4, 0.4, 0.4, 0.5),
                   panel=function(...) {
                     panel.abline(h = 0, col='steelblue', lwd=2)
                     panel.xyplot(...)
                   },
                   xlab = xlb, ylab = ylb, type = "l", ...)
    }
    return(pl)
}



#' Trace plots for conditional indices rper replicate number
#'
#' @inheritParams xp.dofv.npar.plot
#' @inheritParams xp.inc.cond.stab.cov
#' @inheritParams xp.inc.stab.cov
#' 
#' @param limits Limits for the inclusion index.
#' @param start When to start.
#' @param ... Arguments passed to other functions.
#'
#' @return A lattice plot object.
#' @export
#'
#' @family bootgam
#' @family bootscm
# @examples
xp.inc.ind.cond.stab.cov <- function (
  ## trace plots for conditional indices
  bootgam.obj = NULL,
  boot.type = NULL,
  main = NULL,
  xlb = "Bootstrap replicate number",
  ylb = "Conditional inclusion frequency",
  limits = c(.2, .8),
  normalize = TRUE,
  split.plots = FALSE,
  start = 25,
  ...) {
  
    label <- NULL
    idn <- NULL
  
    bootgam.obj <- get.boot.obj(bootgam.obj, boot.type)
    if (is.null(bootgam.obj)) {
      return()
    }
    boot.type <- get.boot.type(bootgam.obj)
    if (is.null(main) && !is.null(bootgam.obj$runno) && bootgam.obj != "") {
      main <- paste("Conditional index stability for", bootgam.obj$runno)
    }

    ## get inclusion frequency from bootscm object
    res <- c()

    # get list of covariate names that have 20-80% inclusion index
    sel <- c(tail(bootgam.obj$incl.freq,1) > limits[1] & tail(bootgam.obj$incl.freq,1) < limits[2])
    cov_list <- names(bootgam.obj$incl.freq[sel])

    message("Calculating conditional inclusion indices per bootstrap iteration...")
    pb <- txtProgressBar(min = 0, max = length(bootgam.obj$incl.freq[,1]), initial = 0)
    res <- c()
    for(i in start:length(bootgam.obj$incl.freq[,1])) {
      setTxtProgressBar(pb, i)
      dat_i <- c()
      for(j in seq(cov_list)) {
        tmp <- xp.incl.index.cov.ind(bootgam.obj = bootgam.obj,
                                     return_plot = FALSE,
                                     results.tab = bootgam.obj$results.tab[1:i,],
                                     cov.name = cov_list[j])
        tmp <- tmp[order(tmp$idn),]
        if(j == 1) {
          dat_i <- tmp
          colnames(dat_i)[2] <- cov_list[j]
        } else {
          dat_i[[cov_list[j]]] <- tmp$idx
        }
      }
      res <- rbind(res, dat_i) # can be implemented faster!
    }
    res <- data.frame(res)
    res$n <- rep(start:length(bootgam.obj$incl.freq[,1]), each = length(dat_i[,1]))
    res.long <- reshape (res,
                          ids=row.names(res), varying = names(res)[-c(1, length(res[1,]))],
                          idvar = "row", timevar = "var", v.names = "value",
                          times = names(res)[-c(1, length(res[1,]))], direction="long")
    res.long$label <- paste0(res.long$var, "_", res.long$idn)
    if(normalize) {
      message("Normalizing...")
      pb2 <- txtProgressBar(min = 0, max = length(unique(res.long$label)), initial = 0)
      unq <- unique(res.long$label)
      lst <- res.long[res.long$n == max(res$n),]
      for(i in seq(unique(res.long$label))) {
        setTxtProgressBar(pb2, i)
        res.long[res.long$label == unq[i],]$value <- res.long[res.long$label == unq[i],]$value - lst[lst$label == unq[i],]$value
      }
    }

    message("Plotting...")
    if(split.plots) {
      pl <- xyplot(value ~ n | var, data = res.long, main = main,
                   group = idn, col = "#888888",
                   xlab = xlb, ylab = ylb, type = "l",
                   panel=function(...) {
                     panel.abline(h = 0, col="steelblue")
                     panel.xyplot(...)
                   }, ...)
    } else {
      pl <- xyplot(value ~ n, data = res.long, main = main,
                   groups = label,
                   col = rgb(0.4, 0.4, 0.4, 0.25),
                   panel=function(...) {
                     panel.abline(h = 0, col='steelblue', lwd=2)
                     panel.xyplot(...)
                   },
                   xlab = xlb, ylab = ylb, type = "l", ...)
    }
    return(pl)
}

#' Distribution of difference in OFV 
#'
#' @param bootscm.obj a bootscm object.
#' @param main The title of the plot
#' @param xlb The x-label of the plot
#' @param ylb The y-label of the plot
#' @param ... Additional parameters passed to \code{panel.xyplot} and \code{xyplot}.
#'
#' @return A lattice plot object.
#' @export
#'
#' @family bootgam
#' @family bootscm
#' 
# @examples
xp.dofv.npar.plot <- function (bootscm.obj = NULL, main = NULL, xlb = "Difference in OFV",
                               ylb = "Density", ...)  {
  bootscm.obj <- get.boot.obj(bootscm.obj, boot.type = "bootscm")
  if (is.null(bootscm.obj)) {
    return()
  }
  if (is.null(main)) {
    main <- paste("Distribution of dOFV for", bootscm.obj$runno)
  }
  size <- as.numeric(apply(cbind(bootscm.obj$results.tab, bootscm.obj$results.tab.dum), 1, "sum"))
  size_orig <- sum(bootscm.obj$results.tab.orig)
  dofv <- bootscm.obj$dofv$dOFV[-1]
  ofv <- bootscm.obj$dofv$OFV[-1]
  ofv_original <- bootscm.obj$ofv_original
  data <- data.frame(cbind(n = 1:length(size), size, dofv, ofv, ofv_original))
  data$class <- 0
  chi <- data.frame(cbind(x = c(-4, -3, -2, -1, 0, 1, 2, 3, 4) + size_orig,
                          y = c(qchisq(p = 0.95, df = c(4, 3, 2, 1)), 0, -qchisq(p = 0.95, df = c(1, 2, 3, 4))) ))
  data$class <- as.numeric(data$dofv <= chi$y[match(data$size, chi$x)])
  bg <- c(rgb(0.5,0.5,0.5,0.5), "darkblue")
  sz <- c(1, 1)
  font_sz <- c(0.5, .75)
  font_col <- c(rgb(1,1,1,0), "white")
  message("Models with largest dOFV:")
  print(data[order(data$dofv),][1:10,])
  pl <- xyplot(dofv ~ size, data=data,
               ylab = "dOFV",
               xlab = "Covariate model size",
               pch = 19,
               panel = function(...) {
                llines (x=chi$x, y=chi$y)
                panel.abline(h = 0, lty = "dotted", col = "black")
                panel.abline(v = size_orig, lty = "dotted", col = "black")
                panel.xyplot(..., cex = sz[data$class+1], col=bg[data$class+1])
            #    panel.text(size, dofv, labels = data$n, cex=font_sz[data$class+1], col=font_col[data$class+1])
               }, ...)
  return(pl)
}


#' Distribution of difference in AIC 
#'
#' @param bootscm.obj a bootscm object.
#' @param main The title of the plot
#' @param xlb The x-label of the plot
#' @param ylb The y-label of the plot
#' @param ... Additional parameters passed to \code{panel.xyplot} and \code{xyplot}.
#'
#' @return A lattice plot object.
#' @export
#'
#' @family bootgam
#' @family bootscm
# @examples
xp.daic.npar.plot <- function (bootscm.obj = NULL, main = NULL, xlb = "Difference in AIC",
                               ylb = "Density", ...)  {
  bootscm.obj <- get.boot.obj(bootscm.obj, boot.type = "bootscm")
  if (is.null(bootscm.obj)) {
    return()
  }
  if (is.null(main)) {
    main <- paste("Distribution of dAIC for", bootscm.obj$runno)
  }
  size <- as.numeric(apply(cbind(bootscm.obj$results.tab, bootscm.obj$results.tab.dum), 1, "sum"))
  size_orig <- sum(bootscm.obj$results.tab.orig)
  dofv <- bootscm.obj$dofv$dOFV[-1]
  ofv <- bootscm.obj$dofv$OFV[-1]
  ofv_original <- bootscm.obj$ofv_original
  data <- data.frame(cbind(n = 1:length(size), size, size_orig, ofv, dofv, ofv_original, class = 0))
  # AIC = 2k - 2log(L)
  # dAIC = 2k1 * log(L1) - 2k2 * log(L2)
  data$daic <- (2 * data$size + data$ofv) - (2 * data$size_orig + data$ofv_original)
  data$class <- as.numeric(data$daic <= 0)
  bg <- c(rgb(0.5,0.5,0.5,0.5), "darkblue")
  sz <- c(1, 1)
  font_sz <- c(0.5, .75)
  font_col <- c(rgb(1,1,1,0), "white")
  message("Models with largest dAIC:")
  print(data[order(data$daic),][1:20,])
  pl <- xyplot(daic ~ size, data=data,
               ylab = "dAIC", xlab = "Covariate model size",
               pch = 19,
               panel = function(...) {
                 panel.abline(h = 0, lty = "dotted", col = "black")
                 panel.abline(v = size_orig, lty = "dotted", col = "black")
                 panel.xyplot(..., cex = sz[data$class+1], col=bg[data$class+1])
                 #  panel.text(data$size, data$daic, labels = data$n, cex=font_sz[data$class+1], col=font_col[data$class+1])
               }, ...)
  return(pl)
}
