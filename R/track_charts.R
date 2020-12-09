# TODO use round period in for_dates, use different S3 classes for periodic
# charts.

# from https://github.com/playax/playax/blob/master/app/models/track_chart.rb#L4
TRACK_CHARTS_SOURCES <- c('Playax', 'Radio', 'Youtube',
                          'Spotify', 'Deezer', 'ITunes')

#' @export
track_charts <- function() new_track_charts(db_tbl('track_charts'))

#' @export
day_track_charts <- function() filter(track_charts(), type == 'TrackChartDay')

#' @export
week_track_charts <- function() filter(track_charts(), type == 'TrackChartWeek')

#' @export
collect.track_charts <- function(x, ...) mutate(
  new_track_charts(NextMethod()), across(matches('chart_date'),
                                         ~as.POSIXct(.)))

#' @export
with_matched_tracks <- function(.tbl, track_info = TRUE) {
  UseMethod('with_matched_tracks')
}

#' @export
with_matched_tracks.track_charts <- function(.tbl, track_info = TRUE) {
  .tbl <- .tbl %>%
    left_join(db_tbl('web_counts'), by = c('source_name', 'source_id'),
              suffix = c('', '.wc'))

  if (track_info) {
    .tbl <- .tbl %>% left_join(tracks(), by = c('track$id' = 'id'),
                       suffix = c('', '.tracks')) %>%
      # track_metadata causes errors, likely because of embedded NULLs in
      # strings (https://github.com/r-dbi/RMySQL/issues/139)
      select(-track_metadata)
  }
  .tbl
}

#' @export
with_right_holders_.track_charts <- function(.tbl, drop_invalid = TRUE) {
  # TODO we need a way to declaratively specify those schema enrichments,
  # and then encode and enforce them across the board.
  if ('track$id' %nin% colnames(.tbl)) {
    .tbl <- .tbl %>% with_matched_tracks()
  }

  # FIXME again, we're duplicating this join here
  .tbl <- .tbl %>%
    left_join(db_tbl('track_right_holders'), by = c('track$id' = 'track_id'),
              suffix = c('', '.trh'))

  filter(NextMethod(), role == 'Interpreter')
}

#' @export
for_source.track_charts <- function(.tbl, ..., .dots = NULL) {
  sources <- get_parlist(..., .dots = .dots)
  check_metrics(sources, tolower(TRACK_CHARTS_SOURCES))

  # fetches with right capitalization
  sources <- TRACK_CHARTS_SOURCES[
    tolower(TRACK_CHARTS_SOURCES) %in% tolower(sources)]

  .tbl %>% in_filter(source_name, glue::glue('{sources}Count'))
}

#' @export
for_dates_.track_charts <- function(.tbl, start, end) {
  .tbl %>% filter(!!start <= chart_date && chart_date <= !!end)
}

#' @export
with_source_names.track_charts <- function(.tbl) {
  check_in_memory(.tbl)
  .tbl %>% mutate(source_name = gsub('Count$', '', source_name))
}

new_track_charts <- function(.tbl) {
  class(.tbl) <- c('track_charts', class(.tbl))
  .tbl
}

