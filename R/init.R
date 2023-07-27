dxpy <- NULL

.onLoad <- function(libname, pkgname) {
  dxpy <- reticulate::import("dxpy", delay_load = TRUE)
}
