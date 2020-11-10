.globals$rh_cache <- list()

#' Returns the table of all right holders, with information such
#' as their name, genre, and language.
#'
#' @export
right_holders <- function() db_tbl('right_holders')

#' Enriches the current table with right holder information. By default,
#' drops all rows that cannot be matched to any right holder, or which are
#' associated to invalid right holders.
#'
#' @param if_missing (deprecated) enriches the table only if a certain column
#'                   is absent. Defaults to `NULL`.
#'
#' @param drop_invalid drops rows which cannot be matched to any right holder,
#'                     and right holders which are declared as not valid. Defaults
#'                     to TRUE.
#'
#' @export
with_right_holders <- function(.tbl, if_missing = NULL, drop_invalid = TRUE) {
  required <- if (is.null(if_missing)) '' else if_missing
  if (required %nin% colnames(.tbl)) {
    .tbl <- with_right_holders_(.tbl, drop_invalid)
  }
  .tbl
}

with_right_holders_ <- function(.tbl, drop_invalid = TRUE) {
  UseMethod('with_right_holders_')
}

#' @export
with_right_holders_.default <- function(.tbl, drop_invalid = TRUE) {
  # The default implementation looks for a right_holder_id.
  if (!('right_holder_id' %in% colnames(.tbl))) {
    stop('Cannot add right holder
         info to a table lacking a right_holder_id column.')
  }

  .tbl <- .tbl %>%
    join_mode(drop_invalid)(
      right_holders(),
      by = c('right_holder_id' = 'id'),
      suffix = c('', '.rhs')
    )

  if (!drop_invalid) .tbl else .tbl %>% filter(is.null(group_id))
}

#' Enriches a table containing right holder information column with root
#' genres, placed under a `root_genre` column.
#'
#' @export
with_genres <- function(.tbl) {
  .tbl %>%
    with_right_holders(if_missing = 'genre') %>%
    left_join(db_tbl('genres'), by = 'genre')
}

#' Enriches a table containing right holder information with a "language"
#' column, representing the artist's main language.
#'
#' @export
with_language <- function(.tbl) with_right_holders(.tbl, 'language')

#' Enriches a table containing right holder information with associated
#' pseudonyms.
#'
#' @param main for right holders having more than one pseudonym, returns
#' only the one registered as its "main" pseudonym, if any. Note that data
#' errors mean that it may happen that an artist has multiple "main"
#' pseudonyms.
#'
#' @export
with_pseudos <- function(.tbl, main = TRUE) {
  .tbl <- ensure_right_holders(.tbl) %>%
    left_join(
      db_tbl('right_holder_pseudos'),
      by = 'right_holder_id',
      suffix = c('', '.pseudos')
    )

  if (main) .tbl %>% filter(main == 1) else .tbl
}

#' As \code{\link{find_right_holder}}, but accepts multiple right holder
#' pseudonyms.
#'
#' @param ... a set of right holder names to resolve.
#' @param match mode. 'any' will return any matching pseudonym; 'all' will
#'             return all pseudonyms, 'main' will return only the main pseudonym,
#'             or error out if there is more than one.
#'
#' @return a \code{\link{data.frame}} containing a `pseudo` and a `right_holder_id`
#'         column. Depending on the match mode, may return multiple rows for
#'         a given pseudo.
#'
#' @seealso find_right_holder
#'
#' @export
find_right_holders <- function(..., .dots = NULL, mode = c('any', 'all', 'main')) {
  pseudos <- get_parlist(..., .dots = .dots)
  # This is SLOW. Ideally we should carve up a multi-right-holder version of
  # find_right_holder and treat the single right holder function as the
  # special case.
  lapply(
    pseudos,
    function(pseudo) {
      tibble(
        pseudo = pseudo,
        right_holder_id = find_right_holder(pseudo, mode)
      )
    }
  ) %>% {
    do.call(rbind, .)
  }
}

#' Finds a right_holder ID by its pseudonymn.
#'
#' @param name a string representing the artist's name.
#' @param mode match mode. 'any' will return any matching pseudonym; 'all' will
#'             return all pseudonyms, 'main' will return only the main pseudonym,
#'             or error out if there is more than one.
#'
#' @return an integer vector representing the right holder IDs associated with this
#' name.
#'
#' @export
find_right_holder <- function(name, mode = c('any', 'all', 'main')) {
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
    stop(glue::glue('No matches for pseudonymn {name}.'))
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

ensure_right_holders <- function(.tbl) {
  if ('right_holder_id' %nin% colnames(.tbl)) {
    stop('Operation requires right_holder_id. Apply with_right_holders first.')
  }
  .tbl
}
