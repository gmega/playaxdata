.globals$rh_cache <- list()

#' Right holder base data
#'
#' Table containing all right holders, with information such as
#' their name, genre, and language.
#'
#' @export
right_holders <- function() new_right_holders(db_tbl('right_holders'))

#' @export
for_right_holder_.right_holders <- function(.tbl, right_holder_ids) {
  .tbl %>% in_filter(id, right_holder_ids)
}

new_right_holders <- function(.tbl) {
  class(.tbl) <- c('right_holders', class(.tbl))
  .tbl
}

#' Enriches the current table with right holder information. By default,
#' drops all rows that cannot be matched to any right holder, or which are
#' associated to invalid right holders.
#'
#' @param if_missing (deprecated) enriches the table only if a certain column
#'                   is absent. Defaults to `NULL`.
#'
#' @param drop_invalid drops rows which cannot be matched to any right holder,
#'                     and right holders which are declared as not valid. Defaults
#'                     to TRUE.
#'
#' @export
with_right_holders <- function(.tbl, if_missing = NULL, drop_invalid = TRUE) {
  required <- if (is.null(if_missing)) '' else if_missing
  if (required %nin% colnames(.tbl)) {
    .tbl <- with_right_holders_(.tbl, drop_invalid)
  }
  .tbl
}

with_right_holders_ <- function(.tbl, drop_invalid = TRUE) {
  UseMethod('with_right_holders_')
}

#' @export
with_right_holders_.default <- function(.tbl, drop_invalid = TRUE) {
  # The default implementation looks for a right_holder_id.
  if (!('right_holder_id' %in% colnames(.tbl))) {
    stop('Cannot add right holder
         info to a table lacking a right_holder_id column.')
  }

  # XXX not sure running inner joins on drop_invalid is such a great
  # idea.
  .tbl <- .tbl %>%
    join_mode(drop_invalid)(
      right_holders(),
      by = c('right_holder_id' = 'id'),
      suffix = c('', '.rhs')
    )

  if (!drop_invalid) .tbl else .tbl %>% filter(is.null(group_id))
}

#' Enriches a table containing right holder information column with root
#' genres, placed under a `root_genre` column.
#'
#' @export
with_genres <- function(.tbl) {
  .tbl %>%
    with_right_holders(if_missing = 'genre') %>%
    left_join(db_tbl('genres'), by = 'genre')
}

#' Enriches a table containing right holder information with a "language"
#' column, representing the artist's main language.
#'
#' @export
with_language <- function(.tbl) with_right_holders(.tbl, 'language')

ensure_right_holders <- function(.tbl) {
  if ('right_holder_id' %nin% colnames(.tbl)) {
    stop('Operation requires right_holder_id. Apply with_right_holders first.')
  }
  .tbl
}
