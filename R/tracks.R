#' @export
tracks_for <- function(right_holder) {
  db_tbl('track_right_holders') %>%
    filter(right_holder_id == !!resolve_right_holder(right_holder), role == 'Interpreter') %>%
    inner_join(db_tbl('tracks'), by = c('track_id' = 'id'))
}
