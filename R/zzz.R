.onLoad <- function(libname, pkgname) {
  # Default DB must be set.
  tryCatch(
    megautils::default_db(),
    error = function(err) warning(
      'package will not work properly without setting a megautils::default_db()'
    )
  )
}
