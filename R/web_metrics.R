# Ipsis literis from the Ruby code.
SOURCE_NAMES <- l(
  'DeezerCount'       = 0,
  'ITunesCount'       = 1,
  'OnerpmCount'       = 2,
  'Palcomp3Count'     = 3,
  'RdioCount'         = 4,
  'RhapsodyCount'     = 5,
  'SoundcloudCount'   = 6,
  'SpotifyCount'      = 7,
  'TratoreCount'      = 8,
  'VimeoCount'        = 9,
  'YoutubeAssetCount' = 10,
  'YoutubeCount'      = 11,
  'LinkfireCount'     = 12
)

METRIC_TYPES <- l(
  'plays'     = 0,
  'listeners' = 1
)

#' @export
web_metrics <- function(with_right_holders = FALSE) {
  # Due to limitations with ColumnStore, we cannot use with_right_holders as
  # an enrichment. We have to build the query by hand, right from the start.
  tbl <- if (!with_right_holders) {
    db_tbl('web_metrics')
  } else {
    db_tbl_sql(
      'SELECT wm.*,
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
web_metric_source <- function(name) {
  SOURCE_NAMES[[glue::glue('{stringr::str_to_title(name)}Count')]]
}

#' @export
web_metric_type <- function(name) {
  METRIC_TYPES[[tolower(name)]]
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
for_source.web_metrics <- function(.tbl, source_name) {
  .tbl %>% filter(source_name == !!web_metric_source(source_name))
}

#' @export
for_metric_type.web_metrics <- function(.tbl, metric_type) {
  .tbl %>% filter(metric_type == !!web_metric_type(metric_type))
}

#' @export
for_dates_.web_metrics <- function(.tbl, start, end) {
  .tbl %>% filter(!!start <= start && start <= !!end)
}

#' @export
aggregate.web_metrics <- function(.tbl) {
  check_in_memory(.tbl)
  new_web_metrics(
    .tbl %>%
      mutate(date = parse_date_time(end, 'Y!-m!*-d! H!:M!:S!')) %>%
      select(-id, -web_count_id, -start, -end, -created_at) %>%
      group_by_at(vars(-contains('metric_value'))) %>%
      summarise(metric_value = sum(metric_value, na.rm = TRUE)) %>%
      ungroup()
    )
}
