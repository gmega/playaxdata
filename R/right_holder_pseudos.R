#' Right holder pseudonyms
#'
#' Table of right holder pseudonyms, or artistic names. Right holders may have
#' more than one registered pseudonym and, for some of them, we may know which
#' one is the most popular one. We say that this is the right holder's _main_
#' pseudonym.
#'
#' This function accepts a single parameter, `mode`, which determines which
#' pseudonyms to return. Valid options are:
#' \describe{
#'    \item{`any`}{Returns one single main pseudonym for each right holder, if
#'                 available, or any of the non-main pseudonyms otherwise.}
#'    \item{`main`}{Returns only main pseudonyms, leaving artists without a main
#'                  pseudonym out. This may return multiple pseudonyms for some
#'                  right holders, if more than one pseudonym is marked as main.}
#'    \item{`all`}{returns _all_ pseudonyms for all right holders. This means
#'                 there may be multiple results for some right holders.}
#' }
#'
#' Mode `any` is the ideal mode for enriching tables with right holder
#' pseudonyms, as it guarantees that _i)_ there will be only one pseudonym
#' per right holde; _ii)_ that this will be the most reasonable choice based
#' on the data we have.
#'
#' @param mode which pseudonyms to return.
#'
#' @export
right_holder_pseudos <- function(mode = c('any', 'main', 'all')) {
  mode <- match.arg(mode)

  base <- if (mode == 'all') {
    db_tbl('right_holder_pseudos')
  } else if (mode == 'main') {
    db_tbl('right_holder_pseudos') %>% filter(main == 1)
  } else {
    pseudos_unique()
  }

  new_right_holder_pseudos(base)
}

pseudos_unique <- function() {
  db_tbl_sql(
  'SELECT id, right_holder_id, name, main, created_at, updated_at
   FROM (
     SELECT *, ROW_NUMBER() OVER (PARTITION BY right_holder_id ORDER BY main DESC) rownumber
     FROM right_holder_pseudos
   ) unique_pseudos
   WHERE rownumber = 1'
  )
}

new_right_holder_pseudos <- function(.tbl) {
  class(.tbl) <- c('right_holder_pseudos', class(.tbl))
  .tbl
}

#' Enriches table with right holder pseudonyms
#'
#' Enriches the current table with data from the \code{\link{right_holder_pseudos}}
#' table, plugging in pseudonyms.
#'
#' @param mode the pseudonyms to join with. See \code{\link{right_holder_pseudos}}
#'             for mode details.
#'
#' @export
with_pseudos <- function(.tbl, mode = c('any', 'main', 'all')) {
  mode <- match.arg(mode)
  ensure_right_holders(.tbl) %>%
    left_join(
      right_holder_pseudos(mode),
      by = 'right_holder_id',
      suffix = c('', '.pseudos'),
      copy = TRUE # allows enriching in-memory tables with pseudos
    )
}

#' As \code{\link{find_right_holder}}, but accepts multiple right holder
#' pseudonyms.
#'
#' @param ... a set of right holder names to resolve.
#' @param match mode. 'any' will return any matching pseudonym; 'all' will
#'             return all pseudonyms, 'main' will return only the main pseudonym,
#'             or error out if there is more than one.
#' @param if_absent how to behave when a pseudonym is missing. Can be either
#'                  'raise_error' (default), which will raise an error; or
#'                  'return_NA', which will return NAs for pseudonyms that
#'                  cannot be matched.
#'
#' @return a \code{\link{data.frame}} containing a `pseudo` and a `right_holder_id`
#'         column. Depending on the match mode, may return multiple rows for
#'         a given pseudo.
#'
#' @seealso find_right_holder
#'
#' @export
find_right_holders <- function(..., .dots = NULL,
                               mode = c('any', 'all', 'main'),
                               if_absent = c('raise_error', 'return_NA'),
                               strategies = NULL) {
  pseudos <- get_parlist(..., .dots = .dots)
  # This is SLOW. Ideally we should carve up a multi-right-holder version of
  # find_right_holder and treat the single right holder function as the
  # special case.
  lapply(
    pseudos,
    function(pseudo) {
      tibble(
        pseudo = pseudo,
        right_holder_id = find_right_holder(pseudo, mode, if_absent, strategies)
      )
    }
  ) %>% {
    do.call(rbind, .)
  }
}

#' Pre-packaged strategies which might be supplied to find_right_holder(s) to
#' increase chances of a match success by transforming the input.
#'
#' @export
PSEUDONYM_LOOKUP_STRATEGIES <- l(
  replace_ampersand = function(ampersand_translation) {
    l(
      applicable = function(name) grepl('&', name),
      apply = function(name) gsub('&', ampersand_translation, name)
    )
  }
)

#' Finds a right_holder ID by its pseudonymn.
#'
#' @param name a string representing the artist's name.
#' @param mode match mode. 'any' will return any matching pseudonym; 'all' will
#'             return all pseudonyms, 'main' will return only the main pseudonym,
#'             or error out if there is more than one.
#' @param if_absent how to behave when a pseudonym is missing. Can be either
#'                  'raise_error' (default), which will raise an error; or
#'                  'return_NA', which will return NAs for pseudonyms that
#'                  cannot be matched.
#'
#' @return an integer vector representing the right holder IDs associated with this
#' name.
#'
#' @export
find_right_holder <- function(name,
                              mode = c('any', 'all', 'main'),
                              if_absent = c('raise_error', 'return_NA'),
                              strategies = NULL) {

  if_absent <- match.arg(if_absent)

  strategies <- c(
    l(identity = l(applicable = function(name) TRUE, apply = identity)),
    strategies
  )

  name <- stringr::str_squish(name)

  match <- NA
  for (strategy in strategies) {
    match <- if (strategy$applicable(name))
      find_right_holder0(strategy$apply(name), mode = mode) else NA

    if (!is.na(match)) {
      break
    }
  }

  if (is.na(match) & if_absent == 'raise_error') {
    stop(glue::glue('No matches for pseudonymn {name}.'))
  }

  match
}

find_right_holder0 <- function(name,
                               mode = c('any', 'all', 'main')) {
  mode <- match.arg(mode)
  cache <- .globals$rh_cache
  if (name %in% names(cache)) {
    entry <- cache[[name]]
    if (mode == entry$mode) {
      return(entry$id)
    }
  }

  pseudos <- db_tbl('right_holder_pseudos') %>%
    filter(name %LIKE% !!name) %>%
    inner_join(
      db_tbl('right_holders') %>%
        # non-null group_ids mean that the right holder is either messed up
        # or is part of a band which is already represented by another primary
        # id.
        filter(is.null(group_id)) %>%
        select(id),
      by = c('right_holder_id' = 'id')
    ) %>%
    select(right_holder_id, name, main) %>%
    collect

  # We got nothing.
  if(nrow(pseudos) == 0) {
    return(NA)
  }

  # 'any' will prefer the main pseudonym, but may return a secondary one
  # if no non-mains are found.
  if (mode == 'any') {
    pseudos <- pseudos %>% arrange(desc(main)) %>% slice(1)
  } else if (mode == 'main') {
    pseudos <- pseudos %>% filter(main == 1)
    # 'main' will complain if there's more than one pseudonym.
    if (nrow(pseudos) > 1) {
      stop(glue::glue(
        'Ambiguous pseudonymn {name}. Set mode = "all" to return multiple.'))
    }
    # clearly we also have to complain if there isn't a main pseudonyms
    # registered.
    if (nrow(pseudos) == 0) {
      stop(glue::glue('No main pseudonym registered for {name}.'))
    }
  }

  .globals$rh_cache[[name]] <- list(mode=mode, id=pseudos$right_holder_id)

  pseudos$right_holder_id
}

resolve_right_holders <- function(..., .dots = NULL) {
  right_holders <- get_parlist(..., .dots = .dots)

  if (all(sapply(right_holders, is.character))) {
    find_right_holders(.dots = right_holders, mode = 'any')$right_holder_id
  } else if(all(sapply(right_holders, is.numeric))) {
    unlist(right_holders)
  } else {
    stop(glue::glue('Right holders must be string or numeric, not {right_holder}.'))
  }
}
