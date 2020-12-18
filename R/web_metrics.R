# web_metrics has yet another source coding scheme.
SOURCE_NAMES <- c(
  'Deezer', 'ITunes', 'Onerpm', 'Palcomp3', 'Rdio',
  'Rhapsody', 'Soundcloud', 'Spotify', 'Tratore', 'Vimeo',
  'YoutubeAsset', 'Youtube', 'Linkfire'
)

#' @export
web_metrics <- function(with_right_holders = FALSE) {
  # Due to limitations with ColumnStore, we cannot use with_right_holders as
  # an enrichment. We have to build the query by hand, right from the start.
  tbl <- if (!with_right_holders) {
    db_tbl('web_metrics') %>%
      select(-end) %>%
      rename(metric_date = start)
  } else {
    db_tbl_sql(
      'SELECT
         wm.id AS id,
         wm.track_id,
         wm.web_count_id,
         wm.source_name,
         wm.start AS metric_date,
         wm.feature_type,
         wm.feature_value,
         wm.metric_type,
         wm.metric_value,
         wm.created_at,
         rh.id AS right_holder_id,
         rh.name,
         rh.genre,
         rh.language,
         rh.tier,
         rh.price
       FROM web_metrics wm
       INNER JOIN track_right_holders trh ON wm.track_id = trh.track_id
       INNER JOIN right_holders rh ON trh.right_holder_id = rh.id
       WHERE trh.role = "Interpreter" AND rh.group_id IS NULL'
    )
  }
  new_web_metrics(tbl)
}

#' @export
collect.web_metrics <- function(x, ...) new_web_metrics(NextMethod())

new_web_metrics <- function(.tbl) {
  class(.tbl) <- c('web_metrics', class(.tbl))
  .tbl
}

#' @export
as.web_metrics <- function(.tbl) {
  class(.tbl) <- c('web_metrics', class(.tbl))
  .tbl
}

#' @export
with_right_holders_.web_metrics <- function(.tbl, drop_invalid = TRUE) {
  stop('A ColumnStore bug prevents with_right_holders on web_metrics. Use
       with_right_holders = TRUE instead when calling web_metrics()')
}

#' @export
for_source.web_metrics <- function(.tbl, ..., .dots = NULL) {
  sources <- get_parlist(..., .dots = .dots)
  source_indices <- match_metrics(sources, tolower(SOURCE_NAMES))

  .tbl %>% in_filter(source_name, source_indices)
}

#' @export
for_metric_type.web_metrics <- function(.tbl, ..., .dots = NULL) {
  metric_types <- get_parlist(..., .dots = .dots)
  metric_indices <- match_metrics(metric_types)
  .tbl %>% in_filter(metric_type, metric_indices)
}

#' @export
for_dates_.web_metrics <- function(.tbl, start, end) {
  .tbl %>% filter(!!start <= metric_date && metric_date <= !!end)
}

#' @export
aggregate.web_metrics <- function(.tbl) {
  # FIXME this whole method is pretty horrible. We should probably fold this
  # processing into a collect for web_metrics.
  check_in_memory(.tbl)
  new_web_metrics(
    .tbl %>%
      mutate(metric_date = parse_date_time(metric_date, 'Y!-m!*-d! H!:M!:S!')) %>%
      select(-id, -web_count_id, -created_at) %>%
      group_by_at(vars(-contains('metric_value'))) %>%
      summarise(metric_value = sum(metric_value, na.rm = TRUE)) %>%
      ungroup()
    )
}

#' @export
with_source_names.web_metrics <- function(.tbl) {
  check_in_memory(.tbl)
  .tbl %>% mutate(source_name = SOURCE_NAMES[source_name + 1])
}
