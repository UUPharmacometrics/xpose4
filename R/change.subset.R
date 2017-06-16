#' @describeIn change_misc_parameters is used for setting the data item's subset field.  To
#' specify a subset of the data to process, you use the variable names and the
#' regular R selection operators. To combine a subset over two or more
#' variables, the selection expressions for the two variables are combined
#' using R's unary logical operators.
#' 
#' The variable names are those that are specified in the NONMEM table files
#' (e.g. PRED, TIME, SEX).
#' 
#' The selection operators are: == (equal) != (not equal) || (or) > (greater
#' than) < (less than)
#' 
#' For example, to specify that TIME less than 24 should be processed, you type
#' the expression: TIME < 24.
#' 
#' The unary logical operators are: & (and) | (or)
#' 
#' For example, to specify TIME less than 24 and males (SEX equal to 1), you
#' type the expression: TIME < 24 & SEX == 1
#' 
#' This subset selection scheme works on all variables, including ID numbers.
#' 
#' The subset selection is not entirely stable. For example, there is no check
#' that the user enters a valid expression, nor that the user specifies
#' existing variable names. An erroneous expression will not become evident
#' until a plot is attempted and the expression takes effect.
#' 
#' @export
"change.subset"  <- function(object, classic = FALSE)
{
  data <- object

  print.flag.help <- function() {
cat("
Subset selection help
---------------------
To specify a subset of the data to process, you use the variable names
and the regular R \"selection\" operators. To combine a subset over two
or more variables, the selection expressions for the two variables are
combined using R\'s unary logical operators.

The variable names are those that are specified in the NONMEM table
files (e.g. PRED, TIME, SEX).

The \"selection\" operators are:
==   (equal)
!=   (not equal)
||   (or)
>    (greater than)
<    (less than)
For example, to specify that TIME less than 24 should be processed, you
type the expression: TIME < 24.

The unary logical operators are:
&    (and)
|    (or)
For example, to specify TIME less than 24 and males (SEX equal to 1), you
type:TIME < 24 & SEX == 1

This subset selection scheme works on all variables, including ID numbers.

The subset selection is not entirely stable. For example, there is no
check that the user enters a valid expression, nor that the user specifies 
existing variable names. An erroneous expression will not become evident 
until a plot is attempted and the expression takes effect. 
")

}
cat("The current subset expression is:\n")
  cat(object@Prefs@Subset,"\n\n")

  cat("Type the expression that will evaluate to the logical vector which\n")
  cat("indicates the subset you want to use. Type h to get help, q to \n")
  cat("leave it as it is or NULL to unspecify:\n")
  

  ans <- readline()
  if(ans == "q")
    return(cat(""))
  if(ans == "n" || ans == "NULL") {
    ans <- NULL
    data@Prefs@Subset <- ans
        if (classic==TRUE) {
          c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
          eval(c1)
          c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
          eval(c2)
          return(cat(""))
          
        } else {
          return(data)
        }
  } else if(ans == "h") {
    print.flag.help()
  } else {
    data@Prefs@Subset <- ans
        if (classic==TRUE) {
          c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
          eval(c1)
          c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
          eval(c2)
          return(cat(""))
        } else {
          return(data)
        }
  }
  
}


