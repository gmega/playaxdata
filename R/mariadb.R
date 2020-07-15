.globals$mariadb_version <- list()

BUGS <- list(
  COLUMNSTORE_IN_BUG = list(
    major = 10,
    minor = 5,
    maintenance = 4
  )
)

#' @export
has_bug <- function(bug_id) {
  fix_version <- BUGS[[bug_id]]
  if (is.null(fix_version)) {
    stop(glue::glue('Unknown bug {bug_id}.'))
  }
  version <- mariadb_version()
  for (part in c('major', 'minor', 'maintenance')) {
    if (version[[part]] > fix_version[[part]]) {
      return(FALSE)
    } else if (version[[part]] < fix_version[[part]]) {
      return(TRUE)
    }
  }

  # If we got till here, we are running exactly the fix version, so the
  # bug is not present.
  return(FALSE)
}

#' @export
mariadb_version <- function() {
  version <- .globals$mariadb_version$version
  if (is.null(version)) {
    version <- tibble(
      version_str = db_tbl_sql('SELECT VERSION()') %>% pull(1)
    ) %>%
      tidyr::extract(version_str, c('major', 'minor', 'maintenance'), "([0-9]+)\\.([0-9]+)\\.([0-9]+)") %>%
      mutate_all(as.integer)

    .globals$mariadb_version$version <- version
  }

  return(version)
}
