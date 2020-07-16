MAPPINGS <- l(
  spotify = l(
    # From https://github.com/playax/playax/blob/master/app/models/concerns/external_id_spotify.rb
    followers = 0,
    popularity = 1,
    listeners = 2,
    streams = 3,

    # Mapped
    plays = streams,
    active_audience = popularity,

    cumulative = c('followers')
  ),

  # From https://github.com/playax/playax/blob/master/app/models/concerns/external_id_youtube.rb
  youtube = l(
    subscriber_count = 0,
    view_count = 1,

    # Mapped
    followers = subscriber_count,
    plays = view_count,

    cumulative = c('plays', 'followers')
  ),

  # From https://github.com/playax/playax/blob/master/app/models/concerns/external_id_knowledge_graph.rb
  knowledgegraph = l(
    track_plays = 0,
    artist_plays = 1,

    # Mapped
    plays = artist_plays
  ),

  facebook = l(
    page_fans = 0,
    talking_about_this = 1,

    # Mapped
    likes = page_fans,
    active_audience = talking_about_this,

    cumulative = c('likes', 'followers')
  ),

  instagram = l(
    followers = 0,
    likes = 1,
    comments = 2,
    medias = 3,

    # Mapped
    # likes and followers are already mapped

    cumulative = c('followers')
  )
)

mapping_table <- function() {
  do.call(
    rbind,
    lapply(names(MAPPINGS), function(source_name) {
      entry <- MAPPINGS[[source_name]]
      metrics <- entry[names(entry) %in% STANDARD_METRICS]
      tibble(
        source_name_str = source_name,
        metric_type_str = names(metrics),
        source_name = source_index(source_name),
        metric_type = unname(unlist(metrics))
      )
    })
  )
}

#' @export
raw_social_metrics <- function() {
  new_rsm(db_tbl('raw_social_metrics_cs'))
}

#' @export
collect.rsm <- function(x, ...) new_rsm(NextMethod())

new_rsm <- function(.tbl) {
  class(.tbl) <- c('rsm', class(.tbl))
  .tbl
}

#' @export
supported_metric_types_.rsm <- function(.tbl, source, metric_type) {
  names(MAPPINGS[[source]])
}

#' @export
supported_sources.rsm <- function(.tbl) {
  names(MAPPINGS)
}

#' @export
for_source.rsm <- function(.tbl, source_name, add_source_names = TRUE) {
  source_name <- tolower(source_name)
  src_index <- source_index(source_name)
  if (src_index == -1) {
    stop('Unknown source name {source_name}')
  }

  attributes(.tbl)$selected_source <- source_name
  .tbl <- .tbl %>% filter(source_name == !!src_index)

  if (add_source_names) .tbl %>% mutate(source_name = !!source_name) else .tbl
}

#' @export
for_metric_type.rsm <- function(.tbl, metric_type, add_metric_types = FALSE) {
  source_name <- attributes(.tbl)$selected_source
  if (is.null(source_name)) {
    stop('Must select a source with for_source first')
  }
  type_index <- MAPPINGS[[source_name]][[metric_type]]
  if (is.null(type_index)) {
    stop(glue::glue('Unknown metric type {metric_type}'))
  }

  .tbl <- .tbl %>% filter(metric_type == !!type_index)
  if (add_metric_types) .tbl %>% mutate(metric_type = !!metric_type) else .tbl
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
  # FIXME well, we're copying the whole thing into memory. Ideally we should
  # not surprise the user with something like this.
  .tbl %>%
    collect %>%
    inner_join(mapping_table(), by = c('source_name', 'metric_type')) %>%
    select(-source_name, -metric_type) %>%
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

  for (source_name in names(MAPPINGS)) {
    metric_spec <- MAPPINGS[[source_name]]
    cumulatives <- metric_spec$cumulatives
    if (is.null(cumulatives)) {
      next
    }
    for (metric_type in cumulatives) {
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

