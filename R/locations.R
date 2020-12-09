# From https://github.com/playax/playax/blob/master/app/models/concerns/geopolitical_location.rb
LOCATION_TYPES <- c('city', 'state', 'region', 'country')

resolve_location <- function(...) {
  location_spec <- list(...)
  for (scope in LOCATION_TYPES) {
    if (scope %in% names(location_spec)) {
      return(list(
        location_type = (LOCATION_TYPES %>% item_index(scope)) - 1,
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
