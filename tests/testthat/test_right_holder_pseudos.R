context('right_holder_pseudos')

setup({ setup_db_access() })

test_that('"main" mode works', {
  pseudos <- right_holder_pseudos(mode = 'main') %>%
    filter(right_holder_id <= 500) %>%
    collect

  # Should only have "main" pseudonyms
  expect_true(all(pseudos$main == 1))

  # Should have gotten all of them, or
  # overshot the target.
  expect_gte(nrow(pseudos), 500)
})

test_that('"any" mode works', {
  pseudos <- right_holder_pseudos(mode = 'any') %>%
    filter(right_holder_id < 500) %>%
    collect

  # Should have one non-main pseudonym (this is sort of brittle, as
  # we're relying on messed up data being present without creating mock
  # data).
  expect_equal(sort(unique(pseudos$main)), c(0, 1))

  # Should have at most one pseudonym per right holder.
  expect_equal(
    pseudos %>%
      group_by(right_holder_id) %>%
      count %>%
      pull(n) %>%
      max,
    1
  )

  # Should have fetched 500 or less pseudonyms.
  expect_lte(nrow(pseudos), 500)
})

test_that('"all" mode works', {
  pseudos <- right_holder_pseudos(mode = 'all') %>%
    filter(right_holder_id < 500) %>%
    collect

  # Should have both main and non-main pseudonyms.
  expect_equal(sort(unique(pseudos$main)), c(0, 1))

  # Should have more than 500 pseudos.
  expect_gte(nrow(pseudos), 500)
})
