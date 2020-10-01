table_entry <- function(.tbl, value, col, label = NULL) {
  label <- if (is.null(label)) deparse(substitute(col)) else label
  value <- value # Trigger evaluation
  fexpr <- substitute(tolower(col) == tolower(value))
  entry <- .tbl %>% filter(!!fexpr)

  if (nrow(entry) == 0) {
    stop(glue::glue('Invalid {label} {value}.'))
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
  if (!('tbl_df' %in% class(.tbl))) {
    parent <- sys.calls()[[sys.nframe() - 1]]
    stop(glue::glue('{parent}: This function only works ',
                    'for tables brought in-memory with `collect`.'))
  }
}

check_columns <- function(.tbl, expected_columns) {
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
}

col_classes <- function(.tbl) {
  .tbl %>%
    head(0) %>%
    collect %>%
    summarise_all(class)
}

join_mode <- function(drop_invalid) if (drop_invalid) inner_join else left_join

