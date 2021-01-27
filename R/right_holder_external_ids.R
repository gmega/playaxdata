#' External IDs for right holders. Those will usually correspond to the IDs
#' that a right holder may take in various platforms (YouTube, Instagram, etc).
#'
#' @export
right_holder_external_ids <- function() new_rhei(
  db_tbl('right_holder_external_ids'))

new_rhei <- function(.tbl) {
  class(.tbl) <- c('rhei', class(.tbl))
  .tbl
}

#' @export
supported_sources.rhei <- function(.tbl) {
  .tbl %>%
    select(source_name) %>%
    distinct %>%
    collect %>%
    pull(source_name) %>%
    sort
}

#' @export
for_source.rhei <- function(.tbl, ..., .dots = NULL) {
  sources <- get_parlist(..., .dots = .dots)
  .tbl %>% in_filter(tolower(source_name), tolower(sources))
}
