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
