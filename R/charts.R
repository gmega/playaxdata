# TODO use round period in for_dates, use different S3 classes for periodic
# charts.

#' @export
track_charts <- function() new_track_charts(db_tbl('track_charts'))

#' @export
day_track_charts <- function() {
  new_track_charts(track_charts() %>% filter(type == 'TrackChartDay'))
}

#' @export
week_track_charts <- function() {
  new_track_charts(track_charts() %>% filter(type == 'TrackChartWeek'))
}

#' @export
for_source.track_charts <- function(.tbl, source) {
  .tbl %>%
    filter(source_name == !!glue::glue('{stringr::str_to_title(source)}Count'))
}

#' @export
for_dates_.track_charts <- function(.tbl, start, end) {
  .tbl %>% filter(!!start <= chart_date && chart_date <= !!end)
}


new_track_charts <- function(.tbl) {
  class(.tbl) <- c('track_charts', class(.tbl))
  .tbl
}

