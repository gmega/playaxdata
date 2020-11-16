#' Standard metric types
#'
#' When supported, standard metrics have a well-defined semantics. Usually
#' used with \code{\link{for_metric_type}}.
#'
#' Taken from \link{https://github.com/playax/playax/blob/master/app/models/concerns/period_metric.rb}.
#'
STANDARD_METRICS <- list(
  plays = 'plays',
  followers = 'followers',
  active_audience = 'active_audience',
  playax = 'playax',
  internet = 'internet',
  streaming = 'streaming',
  social = 'social',
  pageviews = 'pageviews',
  visitors = 'visitors',
  engagers = 'engagers',
  click_throughs = 'click_throughs',
  unique_click_throughs = 'unique_click_throughs',
  unique_preview_clicks = 'unique_preview_clicks',
  unique_bounces = 'unique_bounces',
  unique_click_through_rate = 'unique_click_through_rate',
  plays_28 = 'plays_28',
  viewer_percentage = 'viewer_percentage',
  plays_percentage = 'plays_percentage'
)

#' Resolve metric type indices
#'
#' Matches a character vector of metric types to their corresponding metric
#' indices, raising an error for unknown metric types.
#'
match_metrics <- function(metric_types) {
  metric_types <- tolower(metric_types)
  invalid <- setdiff(metric_types, names(STANDARD_METRICS))
  if (length(invalid) > 0) {
    stop(glue::glue(
      'Unknown metric type(s): <<{ paste(invalid, collapse = ", ") }>>'))
  }

  which(names(STANDARD_METRICS) %in% metric_types) - 1
}
