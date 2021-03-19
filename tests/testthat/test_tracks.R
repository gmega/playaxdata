context('tracks')

setup({ setup_db_access() })

test_that('fetches tracks for a right holder', {
  mm_tracks <- tracks() %>%
    for_right_holder('Marília Mendonça') %>%
    collect

  titles <- tolower(mm_tracks$title)

  expect_true(all(grepl('mar(í|i)lia mendonça', tolower(mm_tracks$artist))))
  expect_true(length(unique(titles)) > 100)

  famous_titles <- c(
    'amante não tem lar',
    'eu sei de cor',
    'ausência',
    'graveto'
  )

  found <- sapply(famous_titles, function(title) any(grepl(title, titles)))

  expect_true(all(found))
})

test_that('enriches existing table with track info', {
  some_tracks <- db_tbl('track_right_holders') %>%
    filter(role == 'Interpreter') %>%
    head(10)

  ids <- some_tracks %>% pull(track_id)

  some_tracks <- some_tracks %>% with_tracks() %>% collect

  # There must be more than 2 distinct track ids.
  expect_gt(length(unique(ids)), 5)

  # IDs must have been preserved.
  expect_true(all(sort(some_tracks$track_id) == sort(ids)))

  # Should have as many titles as track ids.
  expect_equal(length(unique(some_tracks$title)), length(unique(ids)))
})
