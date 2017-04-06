
#' Check through the source dataset to detect problems
#' 
#' This function graphically "checks out" the dataset to identify errors or
#' inconsistencies.
#' 
#' This function creates a series of \code{dotplots}, one for each variable in
#' the dataset, aginst individual ID. Outliers and clusters may easily be
#' detected in this manner.
#' 
#' @param obj NULL or an xpose.data object.
#' @param datafile A data file, suitable for import by
#' \code{\link{read.table}}.
#' @param hlin An integer, specifying the line number on which the column
#' headers appear.
#' @param dotcol Colour for the dots in the dotplot. If obj is an xpose data
#' object then the default is to use the same value as defined for
#' box-and-whisker plots.
#' @param dotpch Plotting character for the dots in the dotplot. If obj is an
#' xpose data object then the default is to use the same value as defined for
#' box-and-whisker plots.
#' @param dotcex Relative scaling for the dots in the dotplot.  If obj is an
#' xpose data object then the default is to use the same value as defined for
#' box-and-whisker plots.
#' @param idlab The ID column label in the dataset.  Input as a text string.
#' @param csv Is the data file in CSV format (comma separated values)?  If the
#' value is \code{NULL} then the user is asked at the command line.  If
#' supplied to the function the value can be \code{TRUE/FALSE}.
#' @param main The title to the plot. "default" means that Xpose creates a
#' title.
#' @param \dots Other arguments passed to \code{link[lattice]{dotplot}}.
#' @return A stack of dotplots.
#' @author Niclas Jonsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link[lattice]{dotplot}}, \code{\link{xpose.prefs-class}},
#' \code{\link{read.table}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run, table and data files for run
#' ## 5 in the current working directory 
#' xpdb5 <- xpose.data(5)
#' 
#' data.checkout(xpdb5, datafile = "mydata.dta")
#' data.checkout(datafile = "mydata.dta")
#' }
#' 
#' @export data.checkout
#' @family data functions 
#' @family specific functions 
"data.checkout"<-
    function(obj=NULL,
             datafile = ".ask.",
             hlin = -99,
             dotcol = "black",
             dotpch = 16,
             dotcex = 1,
             idlab="ID",
             csv=NULL,
             main="Default",
             ...) {

        if (!is.null(obj) &&
            missing(dotcol) &&
            missing(dotpch) &&
            missing(dotcex) ){
            dotcol = obj@Prefs@Graph.prefs$bwdotcol
            dotpch = obj@Prefs@Graph.prefs$bwdotpch
            dotcex = obj@Prefs@Graph.prefs$bwdotcex
        }

        if (datafile==".ask.") {
            cat("Please enter the name of the data file you wish to check:\n")
            datafile <- readline()
        }

        if(is.null(csv)){
            cat("Is the data in CSV format? (y/n):\n")
            csv <- readline()

            if ((csv == "y") ||
                (csv == "Y") ||
                (csv == "yes") ||
                (csv == "YES") ||
                (csv == "Yes")) {
                csv <- TRUE
            } else {
                csv <- FALSE
            }
        }

        if(!is.readable.file(datafile)) {
            cat("There is no file with that name in the current directory!\n")
            return()
        }

        if (hlin == -99) {
            cat("Please enter the line number in the data file\n")
            cat("containing the column headers. Note that if you have\n")
            cat("commented them out, with a '#', for example, you may get\n")
            cat("unpredictable results.) (1): ")
            hlin <- readline()
        }
        if(hlin == "")
            hlin <- 1
        hlin <- as.numeric(hlin)
        if(hlin < 0) {
            cat("The line number has to be > 0!\n"
                )
            invisible()
            return()
        }

        hlin <- hlin - 1

        if (csv) {
            data <- read.csv(datafile, skip = hlin, header = T)
        } else {
            data <- read.table(datafile, skip = hlin, header = T)
        }

        delcol <- grep("XXXX*", names(data))

        if(length(delcol) != 0) {
            data <- data[,  - delcol]
        }



        tit <- paste("Data set checkout of", datafile)

        ## create list for plots
        number.of.plots <- dim(data)[2] - 1
        plotList <- vector("list",number.of.plots)
        plot.num <- 0 # initialize plot number

        idcol <- match(idlab,names(data))
        if(is.na(idcol)) idcol <- 1
        #ids <- unique(data[, idcol])

        for(j in 1:dim(data)[2]) {

            if (j==idcol) next
            xlb <- names(data)[j]
            ylb <- names(data)[idcol]

            xplot <- dotplot(data[, idcol] ~ data[, j],
                             main = NULL,
                             scales = list(x = list(cex = 0.7),
                             y = list(cex = 0.45)),
                             xlab = xlb,
                             ylab = ylb,
                             col = dotcol,
                             cex = dotcex,
                             pch = dotpch
                             )

            plot.num <- plot.num+1
            plotList[[plot.num]] <- xplot
        }

        default.plot.title <- tit
        plotTitle <- xpose.multiple.plot.title(object=obj,
                                               plot.text = default.plot.title,
                                               main=main,
                                               ...)

        obj <- xpose.multiple.plot(plotList,plotTitle,...)
        return(obj)

    }
