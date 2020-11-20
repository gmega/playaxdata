setup_db_access <- function() {
  # TODO improve this so we get the info from local config files.
  default_db <- tryCatch(megautils::default_db(), error = function(x) NULL)
  if (!is.null(default_db)) {
    return()
  }

  megautils::db_register(
    name = 'playax-columnstore',
    driver = RMySQL::MySQL(),
    host = '127.0.0.1',
    port = 3307,
    user = 'giuliano',
    db = 'playax',
    service = 'playax-columnstore'
  )

  megautils::set_default_db('playax-columnstore')
}
