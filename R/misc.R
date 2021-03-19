#' Table column classes
#'
#' Returns a \code{\link{tibble}} containing the class of each column in a given
#' table. Works for in-memory and database tables.
#'
#' @examples
#'
#' col_classes(day_metrics())
#'#> # A tibble: 2 x 7
#'#> id      source_name source_id metric_type metric_date value   created_at
#'#> <chr>   <chr>       <chr>     <chr>       <chr>       <chr>   <chr>
#'#> 1 integer integer     character integer     POSIXct     numeric character
#'#> 2 integer integer     character integer     POSIXt      numeric character
#'
#' @export
col_classes <- function(.tbl) UseMethod('col_classes')

#' @export
col_classes.tbl_dbi <- function(.tbl) {
  col_classes.tbl_df(.tbl %>% head(0) %>% collect)
}

#' @export
col_classes.tbl_df <- function(.tbl) {
  .tbl %>% ungroup %>% summarise_all(class)
}

#' Checks if a table is in memory
#'
#' Checks if a table reference points to a table in memory or to a remote table
#' in the database.
#'
#' @return \code{True} if the table is in memory, or \code{False} otherwise.
#'
#' @export
is_in_memory <- function(.tbl) {
  if ('tbl_dbi' %in% class(.tbl)) {
    FALSE
  } else if ('tbl_df' %in% class(.tbl)) {
    TRUE
  } else {
    stop('Unknown table type.')
  }
}

#' Filters a table by key values
#'
#' get_keys is a specialized filter operation with additional checks. Given
#' a table, a set of keys and a key column, get_keys will keep lines in which
#' keys correspond to values in the key column, and check that:
#'
#'  1. no values are missing;
#'  2. there is only one row per key (optional).
#'
#' Keys are assumed to be strings, and are case-insensitive.
#'
get_keys <- function(.tbl, keys, key_col, unique = FALSE) {
  key_col_sym <- substitute(key_col)

  result <- lapply(
    keys,
    function(key) {
      key <- key # triggers evaluation
      key_col_sym <- key_col_sym # brings into inner scope for substitute
      eval(substitute(get_key(.tbl, key, key_col_sym)))
    }
  )

  result <- do.call(rbind, result)

  if (unique) {
    key_col_name <- deparse(substitute(key_col))
    if (length(result[[key_col_name]]) > length(keys)) {
      stop('Table contains multiple occurrences of one or more keys.')
    }
  }

  result
}

get_key <- function(.tbl, key, key_col) {
  key_col_name <- deparse(substitute(key_col))
  key <- key # evaluates promise
  fexpr <- substitute(tolower(key_col) == tolower(key))
  entry <- .tbl %>% filter(!!fexpr)

  if (nrow(entry) == 0) {
    stop(glue::glue('Invalid {key_col_name}: {key}'))
  }

  entry
}

item_index <- function(item_list, item, label = 'item') {
  index <- which(tolower(item_list) == tolower(item))
  if (length(index) == 0) {
    stop(glue::glue('Invalid {label} {item}.'))
  }
  index
}

get_parlist <- function(..., .dots = NULL) {
  if (!is.null(.dots)) {
    ellipsis::check_dots_empty()
    .dots
  } else {
    ellipsis::check_dots_unnamed()
    list(...)
  }
}

check_in_memory <- function(.tbl) {
  if (!is_in_memory(.tbl)) {
    parent <- sys.calls()[[sys.nframe() - 1]]
    stop(glue::glue('{parent}: This function only works ',
                    'for tables brought in-memory with `collect`.'))
  }
}

check_columns <- function(.tbl, expected_columns = list()) {
  actual_columns <- colnames(.tbl)
  actual_classes <- col_classes(.tbl)

  for (expected_column in names(expected_columns)) {
    expected_classes <- expected_columns[[expected_column]]
    if (!(expected_column %in% actual_columns)) {
      stop(glue::glue('Table is missing required column {expected_column}.'))
    }

    # Only checks actual class if there's an actual constraint.
    if ((length(expected_classes) == 1) && (expected_classes == '*')) {
      next
    }

    actual_class <- actual_classes[[expected_column]]
    if (!any(expected_classes %in% actual_class)) {
      stop(glue::glue('Column {expected_column} should be of class {expected_classes}, not {actual_class}.'))
    }
  }
  .tbl
}

join_mode <- function(drop_invalid) if (drop_invalid) inner_join else left_join
