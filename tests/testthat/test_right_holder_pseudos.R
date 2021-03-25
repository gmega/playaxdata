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

test_that('missing pseudos raise error if no policy is specified', {
  expect_error(
    find_right_holders('Marília Mendonça', 'Gusttavo Lima', 'JAHJ$*!&#$UHJF'),
    'No matches for pseudonymn JAHJ$*!&#$UHJF.',
    fixed = TRUE
  )
})

test_that('missing pseudos result in NAs if policy is "return_NA"', {
  expect_equal(
    find_right_holders(
      'Marília Mendonça', 'Gusttavo Lima', 'JAHJ$*!&#$UHJF',
      if_absent = 'return_NA'
    ) %>% arrange(pseudo) %>% pull(right_holder_id),
    c(508, NA, 43744)
  )
})

test_that('input modification strategies work', {
  expect_equal(
    find_right_holder('Christian & Ralf', if_absent = 'return_NA'),
    NA
  )

  expect_equal(
    find_right_holder('Christian & Ralf', strategies =
                        l(PSEUDONYM_LOOKUP_STRATEGIES$replace_ampersand('e'))),
    88396
  )
})
