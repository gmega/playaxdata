#' @include raw_social_metrics.R
#' @include generics.R
#' @include locations.R

# Week metrics are aligned on Fridays.
FRIDAY_INDEX <- 5

# From https://github.com/playax/playax/blob/master/app/models/concerns/period_metric.rb
METRIC_NAMES <- c(
  STANDARD_METRICS$plays,
  STANDARD_METRICS$followers,
  STANDARD_METRICS$active_audience,
  'playax_index',
  'playax_index_internet'
)

#' @export
week_metrics <- function() {
  db_tbl('week_metrics') %>% as.week_metrics
}

#' @export
month_metrics <- function() {
  db_tbl('month_metrics') %>% as.month_metrics
}

#' @export
year_metrics <- function() {
  db_tbl('year_metrics') %>% as.year_metrics
}

#' @export
day_metrics <- raw_social_metrics

#' @export
as.week_metrics <- function(.tbl) {
  class(.tbl) <- c('week_metrics', 'period_metrics', class(.tbl))
  .tbl
}

#' @export
as.month_metrics <- function(.tbl) {
  class(.tbl) <- c('month_metrics', 'period_metrics', class(.tbl))
  .tbl
}

#' @export
as.year_metrics <- function(.tbl) {
  class(.tbl) <- c('year_metrics', 'period_metrics', class(.tbl))
  .tbl
}

for_dates_.week_metrics <- function(.tbl, start, end) {
  .tbl %>% round_period(start, end, 'week')
}

for_dates_.month_metrics <- function(.tbl, start, end) {
  .tbl %>% round_period(start, end, 'month')
}

for_dates_.year_metrics <- function(.tbl, start, end) {
  .tbl %>% round_period(start, end, 'year')
}

#' @export
for_location.period_metrics <- function(.tbl, ...) {
  location <- resolve_location(...)
  .tbl %>% filter(
    location_type == !!location$location_type,
    location_id == !!location$location_id
  )
}

round_period <- function(.tbl, start, end, unit) {
  .tbl %>%
    filter(
      # Have to use as.character or dbplyr will mess up the formatting
      # and the query will always turn out empty.
      date >= !!as.character(
        lubridate::floor_date(as.Date(start), unit = unit, week_start = FRIDAY_INDEX)) &
      date <= !!as.character(
        lubridate::floor_date(as.Date(end), unit = unit, week_start = FRIDAY_INDEX))
    )
}

#' @export
for_source.period_metrics <- function(.tbl, source_name) {
  index <- source_name_mapping %>%
    table_entry(source_name, source_name) %>%
    pull(period_metrics_index)

  .tbl %>%
    filter(source_type == !!index) %>%
    mutate(source_type = tolower(source_name))
}

#' @export
for_metric_type.period_metrics <- function(.tbl, metric_type) {
  metric_index <- which(METRIC_NAMES == tolower(metric_type))
  if (length(metric_index) == 0) {
    stop(glue::glue('Unknown metric type {metric_type}.'))
  }
  .tbl %>% filter(metric_type == !!(metric_index - 1)) %>%
    mutate(metric_type = tolower(metric_type))
}

#' @export
supported_sources.period_metrics <- function(.tbl) {
  source_name_mapping %>%
    filter(!is.na(period_metrics_index)) %>%
    pull(source_name)
}

#' @export
supported_metric_types_.period_metrics <- function(.tbl, source) {
  source_code <- source_name_mapping %>%
    table_entry(source, source_name) %>%
    pull(period_metrics_index)

  indices <- .tbl %>%
    filter(source_type == source_code) %>%
    select(metric_type) %>%
    distinct %>%
    pull(metric_type)

  # Tables often contain oddball metrics in them. We'll discard
  # those and emit a warning.
  n_metrics <- length(METRIC_NAMES)
  if (any(indices >= n_metrics))
    warning('Source contains unmapped metrics which will be dropped.')

  METRIC_NAMES[indices[indices < n_metrics] + 1]
}
