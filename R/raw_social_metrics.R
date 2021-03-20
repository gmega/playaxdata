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

# Mapping of standard metrics into rsm metrics.
std_metrics <- function() {
  metric_type_mapping %>%
    inner_join(source_name_mapping,
               by = c('source_name' = 'raw_social_metrics_index'), suffix = c('', '.snm')) %>%
    mutate(
      metric_type_str = unlist(playaxdata:::STANDARD_METRICS)[metric_type_to + 1],
      source_name.snm = tolower(source_name.snm)
    ) %>%
    rename(
      source_name_str = source_name.snm,
      metric_type = metric_type_from
    ) %>%
    select(-metric_type_to, -period_metrics_index)
}

# Mapping of non-standard metrics.
ns_metrics <- function() {
  lapply(
    names(MAPPINGS),
    function(source_name_str) {
      metrics <- playaxdata:::MAPPINGS[[source_name_str]]
      tibble(source_name = source_indices(source_name_str),
             source_name_str = source_name_str,
             metric_type = unname(unlist(metrics)),
             metric_type_str = names(metrics))
    }
  ) %>% {
    do.call(rbind, .)
  }
}

# All metrics in one table.
rsm_metrics <- function() {
  ns <- ns_metrics() %>% rename(non_standard_name = metric_type_str)
  std <- std_metrics() %>% rename(standard_name = metric_type_str)

  std %>%
    left_join(ns %>% select(-source_name_str),
              by = c('source_name', 'metric_type')) %>%
    rbind(
      ns %>%
        anti_join(std, by = c('source_name', 'metric_type')) %>%
        mutate(standard_name = NA, aggregation_function = NA)
    )
}

#' Global and regionalized per-artist, "raw" metrics
#'
#' `*_social_metrics` are a set of per-artist tables which store "raw", daily
#' data from the various sources ingested by Playax. These are "raw" in the
#' sense that no processing is done on the data, and it is stored as-is.
#'
#' The global, most commonly used table is `raw_social_metrics`. Regionalized
#' variants are available under `state_social_metrics` (per-state
#' metrics), `city_social_metrics` (per-city metrics) and `region_social_metrics`
#' (per-region metrics).
#'
#' @export
raw_social_metrics <- function() new_rsm(
  db_tbl('raw_social_metrics_cs'), 'source_name')

#' @rdname raw_social_metrics
#' @export
city_social_metrics <- function() new_rsm(
  db_tbl('city_social_metrics'), 'source_name_idx')

#' @rdname raw_social_metrics
#' @export
state_social_metrics <- function() new_rsm(
  db_tbl('state_social_metrics'), 'source_name_idx')

#' @rdname raw_social_metrics
#' @export
region_social_metrics <- function() new_rsm(
  db_tbl('region_social_metrics'), 'source_name_idx')

#' @export
collect.rsm <- function(x, ...) mutate(
  new_rsm(NextMethod(), attributes(x)$source_name_idx),
  across(matches('metric_date'), ~as.POSIXct(.))) # freaking DBI bug

new_rsm <- function(.tbl, source_name_idx) {
  class(.tbl) <- c('rsm', class(.tbl))
  attributes(.tbl)$source_name_idx = source_name_idx
  .tbl
}

#' @export
supported_metric_types_.rsm <- function(.tbl, source, metric_type) {
  # FIXME this is not using the underlying table
  tryCatch(
    get_keys(rsm_metrics(), source, source_name_str) %>%
      mutate(supported = coalesce(standard_name, non_standard_name)) %>%
      pull(supported),
    error = function(err) {
      if (startsWith(err$message, 'Invalid source_name_str:')) {
        stop(glue::glue('Source <<{source}>> is supported, but has no ',
                        'mapped metric types.'))
      }
    }
  )
}

#' @export
supported_sources.rsm <- function(.tbl) {
  source_indices <- .tbl %>%
    select(source_name) %>%
    distinct %>%
    collect %>%
    pull(source_name)

  source_name_mapping %>%
    filter(raw_social_metrics_index %in% source_indices) %>%
    pull(source_name) %>% tolower
}

#' @export
for_right_holder_.rsm <- function(.tbl, right_holder_ids) {
  external_ids <- db_tbl('right_holder_external_ids') %>%
    for_right_holder(.dots = right_holder_ids) %>%
    pull(source_id)

  .tbl %>% filter(source_id %in% external_ids)
}

#' @export
for_source.rsm <- function(.tbl, ..., .dots = NULL) {
  source_names <- tolower(get_parlist(..., .dots = NULL))
  src_indices <- source_indices(source_names)

  attributes(.tbl)$selected_sources <- src_indices

  source_name_idx <- as.symbol(attributes(.tbl)$source_name_idx)

  eval(substitute(in_filter(.tbl, source_name_idx, src_indices)))
}

#' @export
for_metric_type.rsm <- function(.tbl, ..., .dots = NULL) {
  metric_types <- get_parlist(..., .dots = .dots)

  # Preselected sources are a problem. They've already
  # inserted filters into the query, and we'll generate redundant
  # clauses here. Yet, I don't know of a better way to do this
  # which does not imply lazy query generation, which has its
  # own set of nasty problems.

  # Do we have sources pre-selected?
  sources <- attributes(.tbl)$selected_sources
  if (is.null(sources)) {
    # We don't. Reverse select sources based on metric type.
    sources <- reverse_select(metric_types)
  }

  # Retrieves unanbiguous source_name, metric_type pairs.
  pairs <- rsm_metrics() %>%
    filter(source_name %in% sources,
           (standard_name %in% tolower(metric_types)) |
             (non_standard_name %in% tolower(metric_types)))

  # Generates where clauses and runs the query.
  .tbl %>% filter(!!generate_clauses(attributes(.tbl)$source_name_idx,
                                     pairs$source_name, pairs$metric_type))
}

# Reverse-selects sources from metric types.
reverse_select <- function(metric_types) {
  rsm_metrics() %>%
    filter((standard_name %in% tolower(metric_types)) |
            (non_standard_name %in% tolower(metric_types))) %>%
    pull(source_name) %>%
    unique
}

generate_clauses <- function(source_name_idx, sources, metric_types, i = 1) {
  source <- sources[i]
  type <- metric_types[i]
  source_name_idx <- as.symbol(source_name_idx)

  expr <- substitute((source_name_idx == source & metric_type == type))
  if (i < length(sources) & i < length(metric_types)) {
    expr <- call('|', expr, generate_clauses(
      source_name_idx, sources, metric_types, i + 1))
  }
  expr
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
  # We can only do this to in-memory tables.
  check_in_memory(.tbl)
  source_name_idx <- attributes(.tbl)$source_name_idx
  check_columns(.tbl, l('metric_type' = 'integer',
                         !!source_name_idx := 'integer'))

  join_spec <- c('metric_type')
  join_spec[source_name_idx] = 'source_name'

  .tbl %>%
    left_join(
      rsm_metrics() %>%
        mutate(metric_type_str = coalesce(standard_name, non_standard_name)) %>%
        select(metric_type, source_name, metric_type_str, source_name_str),
      by = join_spec
    ) %>%
    select(-source_name, -metric_type) %>%
    rename(
      source_name = source_name_str,
      metric_type = metric_type_str
    )
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
    .tbl, list('source_name' = 'character',
            'metric_type' = 'character',
            'metric_date' = list('POSIXct', 'Date'))
  )

  mappings <- std_metrics()

  for (source_name in unique(mappings$source_name_str)) {
    cumulatives <- mappings %>%
      filter(source_name_str == !!source_name,
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
  .tbl %>%
    arrange(metric_date) %>%
    group_by(source_name, metric_type, source_id) %>%
    mutate(
      value = if_else(
        source_name == !!source_name & metric_type == !!metric_type,
        c(NA, diff(value)),
        value
      )
    )
}

source_indices <- function(source_names) {
  source_name_mapping %>%
    get_keys(source_names, source_name, unique = TRUE) %>%
    pull(raw_social_metrics_index)
}

