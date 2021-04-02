# From https://github.com/playax/playax/blob/master/app/models/concerns/geopolitical_location.rb
# ORDER MATTERS, don't change it
LOCATION_TABLES <- l(
  city = function() cities %>% rename(name = city),
  state = function() states,
  region = function() regions,
  country = function() countries
)

#' The list of possible location types supported by tables which support location
#' filtering. Not all tables support all location types (see \link{supported_location_types}).
#'
#' @export
LOCATION_TYPES <- names(LOCATION_TABLES)

resolve_location_type <- function(location_type) {
  (LOCATION_TYPES %>% item_index(location_type)) - 1
}

resolve_location <- function(...) {
  location_spec <- list(...)
  for (scope in LOCATION_TYPES) {
    if (scope %in% names(location_spec)) {
      return(list(
        location_type = resolve_location_type(scope),
        location_id = do.call(get(glue::glue("resolve_{scope}")), location_spec)$id
      ))
    }
  }
  stop('Bad location spec.')
}

resolve_city <- function(city, state = NULL) {
  entries <- cities %>% get_key(city, city)
  if (nrow(entries) > 1) {
    if (is.null(state)) {
      stop(glue::glue("Ambiguous city name {city}. Must specify state."))
    }
    state <- resolve_state(state)
    entries <- entries %>% filter(state_id == state$state_id)
  }

  entries
}

resolve_state <- function(state) {
  states %>% get_key(state, name)
}

resolve_region <- function(region) {
  regions %>% get_key(region, name)
}

resolve_country <- function(country) {
  countries %>% get_key(country, name)
}

location_labels <- function() {
  parts <- lapply(seq_along(LOCATION_TYPES), function(i) {
    LOCATION_TABLES[[i]]() %>%
      transmute(
        location_type = i - 1,
        location_id = id,
        location_name = name,
        location_type_name = LOCATION_TYPES[[i]]
      )
  })

  do.call(rbind, parts)
}
