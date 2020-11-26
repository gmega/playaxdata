#' @export
tracks <- function() new_tracks(db_tbl('tracks'))

new_tracks <- function(.tbl) {
  class(.tbl) <- c('tracks', class(.tbl))
  .tbl
}

is.tracks <- function(.tbl) inherits(.tbl, 'tracks')

#' @export
for_right_holder_.tracks <- function(.tbl, right_holder_ids) {
  # Theoretically we would not need a specialized "for_right_holder_"
  # implementation as the default implementation would handle it for us,
  # but it turns out that there's a *huge* performance hit with the
  # default query (tracks() %>% with_right_holders_.tracks %>%
  # for_right_holder_.default) so we need a custom query here.
  db_tbl('track_right_holders') %>%
    in_filter(right_holder_id, right_holder_ids) %>%
    filter(role == 'Interpreter') %>%
    inner_join(.tbl, by = c('track_id'= 'id'), suffix = c('.trh', ''))
}

#' @export
with_right_holders_.tracks <- function(.tbl, drop_invalid = TRUE) {
  # FIXME improve this query so that it keeps ONE right holder if so
  # desired.
  .tbl <- .tbl %>%
    left_join(db_tbl('track_right_holders'),
              by = c('track_id' = 'id'),
              suffix = c('', '.trh')) %>%
    filter(role == 'Interpreter')

  NextMethod()
}

#' Adds track info to the current table, if applicable.
#'
#' For tables which carry a `track_id`, adds info from the tracks table.
#'
#' @param drop_invalid if set to TRUE (default), drops rows which have
#' no corresponding track. Otherwise keeps them.
#'
#' @export
with_tracks <- function(.tbl, drop_invalid = TRUE) {
  check_absent(.tbl, 'track_id')
  .tbl %>% join_mode(drop_invalid)(
      # ColumnStore or RMySQL bug requires us to drop:
      #  1. the release column or the join turns up empty;
      #  2. the track_metadata column, or we'll get a large number of
      #     "truncatedinternal error" warnings (and perhaps errors in the data,
      #     who knows).
      db_tbl('tracks') %>% select(-release, -track_metadata),
      by = c('track_id' = 'id'),
      suffix = c('', '.tracks')
    )
}
