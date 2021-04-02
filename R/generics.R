#' Filters a table by (a set of) right holder(s)
#'
#' Filters a table by right holder, translating its string name into the
#' corresponding numeric ID as needed. For some tables, it may be necessary to
#' add right holder information with \code{\link{with_right_holders}} first.
#'
#' @param ... a set of numeric IDs and/or string names of the desired right holders.
#' For each string supplied, will attempt translation with \code{\link{find_right_holder}}
#' first.
#'
#' @param .dots optionally, specify the arguments as a list/vector under this parameter.
#'
#' @examples
#'
#' week_metrics() %>%
#'    for_right_holder('Marília Mendonça')
#'
#' day_metrics() %>%
#'    with_right_holders %>%
#'    for_right_holder('Marília Mendonça', 'MC Kevinho')
#'
#' @export
for_right_holder <- function(.tbl, ..., .dots = NULL) {
  for_right_holder_(.tbl, right_holder_ids =
                      resolve_right_holders(..., .dots = .dots))
}

for_right_holder_ <- function(.tbl, right_holder_ids) {
  UseMethod('for_right_holder_')
}

#' @export
for_right_holder_.default <- function(.tbl, right_holder_ids) {
  if (!('right_holder_id' %in% colnames(.tbl))) {
    # Tries to patch right holder info in.
    .tbl <- tryCatch(
      with_right_holders(.tbl),
      # If we can't then raises an error.
      error = function(err) {
        stop(glue::glue('Don\'t know how to filter {class(.tbl)} by right holder',
                      '- maybe you should call `with_right_holders` first?'))
      })
  }

  .tbl %>% in_filter(right_holder_id, right_holder_ids)
}

#' The smallest possible date
MIN_DATE <- '1901-01-01'

#' Filters a given table by date bracket
#'
#' Will round start and end dates according to the granularity of the receiving
#' table. If end date is omitted, will default to the current date.
#'
#' @param start the start of a date bracket.
#' @param end the end of a date bracket. Defaults to today.
#'
#' @export
for_dates <- function(.tbl, start = NULL, end = NULL) {
  start <- if(is.null(start)) MIN_DATE else start
  end <- if (is.null(end)) Sys.Date() else end
  for_dates_(.tbl, start, end)
}

for_dates_ <- function(.tbl, start, end) UseMethod('for_dates_')

#' Filters a given table by source (e.g. `youtube`, or `spotify`)
#'
#' Valid source names depend on the actual source. Use \code{\link{supported_sources}}
#' to find out which ones are supported.
#'
#' @export
for_source <- function(.tbl, ..., .dots = NULL) {
  UseMethod('for_source')
}

#' Filters a given table by metric type
#'
#' Supported metric types depend on the table and, for some tables, may also
#' depend on the source. For that reason, it may not be possible to filter by
#' a metric type before specifying a source filter. See
#' \code{\link{supported_metric_types}} to verify which metric types are supported.
#'
#' @seealso supported_metric_types
#'
#' @export
for_metric_type <- function(.tbl, ..., .dots = NULL) {
  UseMethod('for_metric_type')
}

#' Returns a character vector with supported sources
#'
#' These are values can be used with \code{\link{for_source}}.
#'
#' @export
supported_sources <- function(.tbl) {
  UseMethod('supported_sources')
}

#' Returns a character vector with supported metric types
#'
#' These values can be later used with \code{\link{for_metric_type}}, with the
#' caveat that source selection with \code{\link{for_source}} may be required
#' first.
#'
#' @param standard returns only standard metrics. Defaults to TRUE.
#'
#' @export
supported_metric_types <- function(.tbl, source, standard = TRUE) {
  all_supported <- supported_metric_types_(.tbl, source)

  if (standard) intersect(STANDARD_METRICS, all_supported)
  else setdiff(all_supported, STANDARD_METRICS)
}

supported_metric_types_ <- function(.tbl, source) {
  UseMethod('supported_metric_types_')
}

#' Convenience method
#'
#' Same as \code{source %in% supported_sources(table)}.
#'
#' @export
supports_source <- function(.tbl, source) {
  source %in% supported_sources(.tbl)
}

#' Convenience method
#'
#' Same as \code{metric_type %in% supported_metric_types(source)}.
#'
#' @export
supports_metric_type <- function(.tbl, source, metric_type) {
  metric_type %in% supported_metric_types(source)
}

#' Adds symbolic source names into the current table
#'
#' Decodes source names in the current table into strings, either by replacing
#' a column in the original table or by creating a new one. May have to copy
#' table contents in-memory if user has no write access to the underlying
#' database.
#'
#' @export
with_source_names <- function(.tbl) {
  UseMethod('with_source_names')
}

#' Adds symbolic metric types into the current table, for all sources
#'
#' Decodes metric types in the current table into strings, either by replacing
#' a column in the original table or by creating a new one. May have to copy
#' table contents in-memory if user has no write access to the underlying
#' database.
#'
#' @export
with_metric_types <- function(.tbl) {
  UseMethod('with_metric_types')
}

#' Returns location-specific data. Supports returning data by city, state,
#' region, or country.
#'
#' If the name of an administrative region is ambiguous (e.g. the city of York
#' exists in both England and the US), will raise an error unless a disambiguating
#' outer administrative region (the country, in this case) is also supplied.
#'
#' Not all tables support location filtering, and not all tables that support
#' location filtering support all location types. See \link{supported_location_types}
#' before attempting to query a table.
#'
#' @export
for_location <- function(.tbl, ...) {
  # TODO: improve parameter handling
  location <- resolve_location(...)
  location_type <- LOCATION_TYPES[location$location_type + 1]
  if (location_type %nin% supported_location_types(.tbl)) {
    stop(g('Table {class(.tbl)[1]} does not support location type {location_type}'))
  }

  for_location_(.tbl, location$location_type, location$location_id)
}

for_location_ <- function(.tbl, location_type, location_id) {
  UseMethod('for_location_')
}

#' In tables which support multiple location types, returns data specific to
#' one of them. See "supported_location_types".
#'
#' @export
for_location_type <- function(.tbl, location_type) {
  if (!supports_location_type(.tbl, location_type)) {
    stop(g('Table {class(.tbl)[1]} does not support location type {location_type}.'))
  }
  for_location_type_(.tbl, location_type)
}

for_location_type_ <- function(.tbl, location_type) {
  UseMethod('for_location_type_')
}

#' Returns a character vector containing the location types supported by this
#' table. It should be a subset of \link{LOCATION_TYPES}.
#'
#' @export
supported_location_types <- function(.tbl) {
  UseMethod('supported_location_types')
}

#' @export
supports_location_type <- function(.tbl, location_type) {
  tolower(location_type) %in% supported_location_types(.tbl)
}

#' @export
with_location_names <- function(.tbl) {
  UseMethod('with_location_names')
}
