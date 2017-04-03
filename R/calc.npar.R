#' Read NONMEM output files into Xpose 4
#' 
#' These are functions that read in a NONMEM output file (a '*.lst' file) and
#' then format the input.
#' 
#' 
#' @param par.mat A parameter matrix.
#' @param filename A NONMEM output file.
#' @param phi.file A NONMEM .phi file
#' @param phi.prefix prefix of a NONMEM .phi file
#' @param runno NONMEM run number
#' @param phi.suffix suffix of a NONMEM .phi file
#' @param quiet Quiet or not
#' @param nm7 NM7 or not
#' @param directory directory in which the NONMEM output files is
#' @param \dots Items passed to functions within this function.
#' @param listfile A NONMEM output file.
#' @param object The return value of \code{read.lst(filename)}
#' @return lists of read values.
#' @author Niclas Jonsson, Andrew Hooker & Justin Wilkins
#' @family data functions 
#' @name read_NM_output
NULL

#' @describeIn read_NM_output calculates the number and type of parameters included in a
#' NONMEM output file
#' @export
#' 
"calc.npar" <- 
  function(object)
{

  ##attach(object)

  ## Thetas
  if(!any(is.null(object$thetas)||is.null(object$thetas))) nth <- length(object$thetas)

  ## SE of the thetas
  #if(!any(is.null(object$sethetas)||is.na(object$sethetas))) {
  if(!any(is.null(object$sethetas))){
    nseth <- length(object$sethetas[!is.na(object$sethetas)])
  }
  else {
    nseth <- 0
  }

  if(!any(is.null(object$omega)||is.na(object$omega))) {
    nom <- 0
    for(i in 1:length(object$omega)) {
      ## This only gives back the number of non-zero elements.
      ## Handling of initial zeros is done in plotsum2
      sel <- object$omega[[i]] != 0
      if(!any(sel==TRUE)){
        nom <- nom + 1
      }
      ##nom <- length(omega[[i]]) + nom
      nom <- length(object$omega[[i]][sel]) + nom
    }
  }
  ## SE of the omegas
  #if(!any(is.null(object$seomegas)||is.na(object$seomegas))) {
  if(!any(is.null(object$seomegas))) {
    nseom <- 0
    for(i in 1:length(object$seomegas)) {
      ## This only gives back the number of non-zero elements.
      ## Handling of initial zeros is done in plotsum2
      sel <- object$seomegas[[i]] != 0
      sel2 <- !is.na(object$seomegas[[i]])
      sel3 <- sel & sel2
      #if(!any(sel==TRUE)){
      #  nom <- nom + 1
      #}
      ##nom <- length(omega[[i]]) + nom
      nseom <- length(object$seomegas[[i]][sel3]) + nseom
    }
  }
  else {
    nseom <- 0
  }
  ## Sigmas
  nsi <- 0
  if(!any(is.null(object$sigma)||is.na(object$sigma))) {
    for(i in 1:length(object$sigma)) {
      sel <- object$sigma[[i]] != 0
      nsi <- length(object$sigma[[i]][sel]) + nsi
    }
  }
  ## SE of the sigmas
  #if(!any(is.null(object$sesigmas)||is.na(object$sesigmas))) {
  if(!any(is.null(object$sesigmas))) {
    nsesi <- 0
    for(i in 1:length(object$sesigmas)) {
      ## This only gives back the number of non-zero elements.
      ## Handling of initial zeros is done in plotsum2
      sel <- object$sesigmas[[i]] != 0
      sel2 <- !is.na(object$sesigmas[[i]])
      sel3 <- sel & sel2
      #if(!any(sel==TRUE)){
      #  nom <- nom + 1
      #}
      ##nom <- length(omega[[i]]) + nom
      nsesi <- length(object$sesigmas[[i]][sel3]) + nsesi
    }
  }
  else {
    nsesi <- 0
  }
  npar <- nth + nom + nsi
  if(length(nseth) > 0 || length(nseom) > 0 || length(nsesi) > 0) {
    ret.list <- list(npar = npar, nth = nth, nseth = nseth, nom = 
                     nom, nseom = nseom, nsi = nsi, nsesi = nsesi)
  }
  else {
    ret.list <- list(npar = npar, nth = nth, nom = nom, nsi = nsi)
  }
  #invisible(ret.list)
  return(ret.list)
}
