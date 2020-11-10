.globals$mariadb_version <- list()
.globals$enabled_bugs <- list()

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

  # Bug may have been force-enabled/disabled by test code.
  if (force_enabled(bug_id)) {
    return(TRUE)
  } else if (force_disabled(bug_id)) {
    return(FALSE)
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

#' An alternative to the dbplyr %in% filter which will build a where expression
#' for versions of ColumnStore which cannot properly evaluate the IN operator.
#'
#' @export
in_filter <- function(.tbl, column, value_list) {
  value_list <- value_list # triggers evaluation
  if (has_bug('COLUMNSTORE_IN_BUG')) {
    expr <- eval(substitute(make_where_expression(column, value_list)))
    .tbl %>% filter(!!expr)
  } else {
    expr <- substitute(column %in% value_list)
    .tbl %>% filter(!!expr)
  }
}

force_bug <- function(bug_id, status = TRUE) {
  .globals$enabled_bugs[[bug_id]] <- status
}

force_enabled <- function(bug_id) {
  bug_id %in% names(.globals$enabled_bugs) &&
    .globals$enabled_bugs[[bug_id]] == TRUE
}

force_disabled <- function(bug_id) {
  bug_id %in% names(.globals$enabled_bugs) &&
    .globals$enabled_bugs[[bug_id]] == FALSE
}

make_where_expression <- function(column_sym, value_list) {
  value <- value_list[1]
  expr <- substitute(column_sym == value)
  if (length(value_list) > 1) {
    expr <- call('|', expr,
                 eval(substitute(
                   make_where_expression(column_sym, value_list[-1]))))
  }
  expr
}
