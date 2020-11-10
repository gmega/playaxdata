context('mariadb')

setup({ setup_db_access() })

test_that('in_filter works in buggy versions of MariaDB', {
  # force-enables the bug
  playaxdata:::force_bug('COLUMNSTORE_IN_BUG')

  ids <- c(1, 2, 3)

  expected_query <- c(
    "<SQL>",
    "SELECT *",
    "FROM `right_holders`",
    "WHERE (`id` = 1.0 OR `id` = 2.0 OR `id` = 3.0)"
  )

  query <- capture.output(
    right_holders() %>%
      in_filter(id, ids) %>%
      show_query,
    type = 'output'
  )

  expect_equal(expected_query, query)
})

test_that('in_filter works in correct versions of MariaDB', {
  # force-enables the bug
  playaxdata:::force_bug('COLUMNSTORE_IN_BUG', status = FALSE)

  ids <- c(1, 2, 3)

  expected_query <- c(
    "<SQL>",
    "SELECT *",
    "FROM `right_holders`",
    "WHERE (`id` IN (1.0, 2.0, 3.0))"
  )

  query <- capture.output(
    right_holders() %>%
      in_filter(id, ids) %>%
      show_query,
    type = 'output'
  )

  expect_equal(query, expected_query)
})

teardown({ playaxdata:::force_bug('COLUMNSTORE_IN_BUG', NULL) })
