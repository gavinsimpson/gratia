# Reconstruct the two explicit prediction scenarios from returned covariates.
conditional_difference_endpoints <- function(cd, model) {
  covars <- setdiff(model_vars(model), attr(cd, "by"))
  a <- b <- as.data.frame(cd[covars])
  for (nm in attr(cd, "by")) {
    prefix <- if (length(attr(cd, "by")) == 1) ".level" else nm
    a[[nm]] <- cd[[paste0(prefix, "_1")]]
    b[[nm]] <- cd[[paste0(prefix, "_2")]]
  }
  list(a = a, b = b)
}
