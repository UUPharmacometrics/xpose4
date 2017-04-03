#' @describeIn GAM_summary_and_plot Individual parameters to GAM fit.
#' @export

xp.cook <- function(gam.object)
{
  
  ##assign(pos = 1, "data", gam.object$data)
  fit.s <- summary.glm(gam.object)
  fit.infl <- lm.influence(gam.object)
  R <- gam.object$R
  I <- t(R) %*% R
  Iinv <- fit.s$cov.unscaled
  ass <- gam.object$assign
  names(ass) <- names(gam.object$coefficients)
  D <- matrix(0, length(gam.object$residuals), length(ass))
  dimnames(D) <- list(names(gam.object$residuals), names(gam.object$coefficients))
  Dcoef <- scale(fit.infl$coefficients, center = gam.object$coefficients, scale= F)
  for(subname in names(ass)) {
    sub <- ass[[subname]]
    Dcoefi <- Dcoef[, sub, drop = F] %*% t(R[, sub, drop = F])
    denom <- I[sub, sub, drop = F] %*% Iinv[sub, sub, drop = F]
    denom <- sum(diag(denom)) * fit.s$dispersion
    D[, subname] <- apply(Dcoefi^2, 1, sum)/denom
  }
  ##remove("data",fr=0)
  D
}
