#' Lists tracks for a right holder
#'
#' Given a numeric right holder id or string pseudonym, returns all tracks that
#' have been released by that right holder.
#'
#' @export
#'
#' @examples
#'
#'   tracks_for('Ludmilla')
#'
tracks_for <- function(right_holder) {
  db_tbl('track_right_holders') %>%
    filter(right_holder_id == !!resolve_right_holders(right_holder), role == 'Interpreter') %>%
    inner_join(db_tbl('tracks'), by = c('track_id' = 'id'))
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
  check_columns(.tbl, 'track_id')
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
