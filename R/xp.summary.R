#' @describeIn GAM_summary_and_plot Summarize GAM.
#' @export

"xp.summary" <-
  function(gamobj=NULL) {

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

    cat("\nSUMMARY")
    print(summary(eval(parse(text="current.gam"))))

    cat("\nPATH TO FINAL MODEL\n")
    print(eval(parse(text="current.gam$anova")))

    cat("\nCOEFFICIENTS\n")
    print(coefficients(eval(parse(text="current.gam"))))

    cat("\nPRERUN RESULTS\n")
    cat("Dispersion:",eval(parse(text="current.gam$dispersion")),"\n")

    cat("\nDATA\n")
    cat("Subset expression:",eval(parse(text="current.gam$subset")),"\n")
    cat("Only first value of covariate considered\n")
    cat("for each individual:",eval(parse(text="current.gam$onlyfirst")),"\n")
    cat("Covariates normalized to median:",eval(parse(text="current.gam$medianNorm")),"\n")

    return(invisible())

  }
