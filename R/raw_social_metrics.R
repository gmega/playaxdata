#' Source-specific metric mappings
#'
#' List containing source-specific metrics with their original naming.
#' Some of those get mapped into \code{\link{STANDARD_METRICS}} through
#' the \code{\link{metric_type_mapping}} table.
#'
MAPPINGS <- l(
  spotify = l(
    # From https://github.com/playax/playax/blob/master/app/models/concerns/external_id_spotify.rb
    followers = 0,
    popularity = 1,
    listeners = 2,
    streams = 3
  ),

  # From https://github.com/playax/playax/blob/master/app/models/concerns/external_id_youtube.rb
  youtube = l(
    subscriber_count = 0,
    view_count = 1
  ),

  # From https://github.com/playax/playax/blob/master/app/models/concerns/external_id_knowledge_graph.rb
  knowledgegraph = l(
    track_plays = 0,
    artist_plays = 1
  ),

  facebook = l(
    page_fans = 0,
    talking_about_this = 1
  ),

  instagram = l(
    followers = 0,
    likes = 1,
    comments = 2,
    medias = 3
  )
)

mapping_table <- function() {
  metric_type_mapping %>%
    inner_join(source_name_mapping,
               by = c('source_name' = 'raw_social_metrics_index'), suffix = c('', '.snm')) %>%
    mutate(metric_type_str = unlist(playaxdata:::STANDARD_METRICS)[metric_type_to + 1]) %>%
    rename(
      source_name_str = source_name.snm,
      metric_type = metric_type_from
    ) %>%
    select(-metric_type_to, -period_metrics_index)
}

# Raw social metrics come into a few flavors.

#' @export
raw_social_metrics <- function() new_rsm(
  db_tbl('raw_social_metrics_cs'), 'source_name')

#' @export
city_social_metrics <- function() new_rsm(
  db_tbl('city_social_metrics'), 'source_name_idx')

#' @export
state_social_metrics <- function() new_rsm(
  db_tbl('state_social_metrics'), 'source_name_idx')

#' @export
region_social_metrics <- function() new_rsm(
  db_tbl('region_social_metrics'), 'source_name_idx')

#' @export
collect.rsm <- function(x, ...) new_rsm(
  NextMethod(), attributes(x)$source_name_idx)

new_rsm <- function(.tbl, source_name_idx) {
  class(.tbl) <- c('rsm', class(.tbl))
  attributes(.tbl)$source_name_idx = source_name_idx
  .tbl
}

#' @export
supported_metric_types_.rsm <- function(.tbl, source, metric_type) {
  c(names(MAPPINGS[[source]]),
    table_entry(mapping_table(), source, source_name_str,
                'source')$metric_type_str)
}

#' @export
supported_sources.rsm <- function(.tbl) {
  source_indices <- raw_social_metrics() %>%
    select(source_name) %>%
    distinct %>%
    collect %>%
    pull(source_name)

  source_name_mapping %>%
    filter(raw_social_metrics_index %in% source_indices) %>%
    pull(source_name) %>% tolower
}

#' @export
for_right_holder_.rsm <- function(.tbl, ..., .dots = NULL) {
  external_ids <- db_tbl('right_holder_external_ids') %>%
    for_right_holder(..., .dots = .dots) %>%
    pull(source_id)

  .tbl %>% filter(source_id %in% external_ids)
}

#' @export
for_source.rsm <- function(.tbl, source_name, add_source_names = TRUE) {
  source_name <- tolower(source_name)
  src_index <- source_index(source_name)
  if (src_index == -1) {
    stop('Unknown source name {source_name}')
  }

  attributes(.tbl)$selected_source <- source_name
  source_name_idx <- as.symbol(attributes(.tbl)$source_name_idx)

  .tbl <- .tbl %>% filter(!!source_name_idx == !!src_index)

  if (add_source_names) .tbl %>% mutate(source_name = !!source_name) else .tbl
}

#' @export
for_metric_type.rsm <- function(.tbl, metric_type, add_metric_types = FALSE) {
  metric_index <- if (metric_type %in% STANDARD_METRICS) {
    resolve_sd(.tbl, metric_type)
  } else {
    resolve_ns(.tbl, metric_type)
  }

  .tbl <- if (length(metric_index) > 1) {
    if (has_bug('COLUMNSTORE_IN_BUG')) {
      # TODO implement OR alternative.
      stop(paste('Cannot select multiple metric indices in this version',
           'of columnstore. Please select a source first with for_source.'))
    }
    .tbl %>% filter(metric_type %in% metric_index)
  } else {
    .tbl %>% filter(metric_type == metric_index)
  }

  if (add_metric_types) .tbl %>% mutate(metric_type = !!metric_type) else .tbl
}

resolve_sd <- function(.tbl, metric_type) {
  metric_table <- mapping_table()

  # Because of the COLUMNSTORE_IN_BUG we have to actually attempt to
  # narrow down metric types to a single index when possible.
  source_name <- attributes(.tbl)$selected_source
  if (!is.null(source_name)) {
    metric_table <- table_entry(
      metric_table, source_name, source_name_str, 'source'
    )
  }

  unique(table_entry(metric_table, metric_type,
              metric_type_str, 'metric type')$metric_type)
}

resolve_ns <- function(.tbl, metric_type) {
  source_name <- attributes(.tbl)$selected_source

  if (!is.null(source_name)) {
    stop('Non-standard metric types require a',
         ' source to be selected first with `for_source`.',
         'Check that you\'ve typed your metric type right.')
  }

  type_index <- MAPPINGS[[source_name]][[metric_type]]
  if (is.null(type_index)) {
    stop(glue::glue('Unknown metric type {metric_type} for {source_name}.'))
  }
  type_index
}

#' @export
for_dates_.rsm <- function(.tbl, start, end) {
  .tbl %>% filter(!!start <= metric_date && metric_date <= !!end)
}

#' @export
with_right_holders_.rsm <- function(.tbl, drop_invalid = TRUE) {
  if ('right_holder_id' %nin% colnames(.tbl)) {
    .tbl <- .tbl %>%
      inner_join(db_tbl('right_holder_external_ids'), by = 'source_id',
               suffix = c('', '.rhei'))
  }
  NextMethod()
}

#' @export
with_source_names.rsm <- function(.tbl) {
  check_columns(.tbl, c('metric_type' = 'integer',
                        'source_name' = c('integer', 'character')))

  # It might be that the source name has been already patched into the table
  # by for_source. In this case, we have to join by the string name.
  by_source = if (class(.tbl$source_name) == 'character') {
    c('source_name' = 'source_name_str')
  } else {
    c('source_name')
  }
  by_source <- c(by_source, 'metric_type')

  # FIXME well, we're copying the whole thing into memory. Ideally we should
  # not surprise the user with something like this.
  .tbl %>%
    collect %>%
    inner_join(mapping_table(), by = by_source) %>%
    select(-source_name, -metric_type, -aggregation_function) %>%
    rename(source_name = source_name_str, metric_type = metric_type_str)
}

#' @export
with_metric_types.rsm <- with_source_names.rsm

#' Differences cumulative metrics in `raw_social_metrics_cs`
#'
#' Some metric types in `raw_social_metrics_cs` are cumulative, others are not. This
#' function takes all cumulative metrics and transforms them into non-cumulative
#' by taking lag 1 differences of metrics with themselves.
#'
#' Works only for in-memory tables. Must enrich source names and metric types
#' with `with_source_names` first.
#'
#' @export
diff_metrics <- function(.tbl) {
  check_in_memory(.tbl)
  check_columns(
    .tbl, c('source_name' = 'character', 'metric_type' = 'character')
  )

  mappings <- mapping_table()

  for (source_name in mappings$source_name_str) {
    cumulatives <- mappings %>%
      filter(source_name_str == source_name,
             aggregation_function == 'last_value')
    if (!nrow(cumulatives)) {
      next
    }

    for (metric_type in cumulatives$metric_type_str) {
      .tbl <- diff_metric(.tbl, source_name, metric_type)
    }
  }
  .tbl
}

diff_metric <- function(.tbl, source_name, metric_type) {
  .tbl <- .tbl %>% arrange(metric_date)
  rows <- .tbl$source_name == source_name & .tbl$metric_type == metric_type
  .tbl$value[rows] <- c(NA, diff(.tbl$value[rows]))
  .tbl
}

source_index <- function(source_name) {
  source_name_mapping %>%
    table_entry(source_name, source_name) %>%
    pull(raw_social_metrics_index)
}

