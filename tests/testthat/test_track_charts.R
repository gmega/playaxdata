context('track_charts')

setup({ setup_db_access() })

test_that('date filters work', {
  charts <- day_track_charts() %>%
    for_dates('2020-01-01', '2020-01-03') %>%
    collect

  expect_equal(sort(unique(charts$chart_date)), as.POSIXct(c(
    '2020-01-01', '2020-01-02', '2020-01-03'
  )))
})

test_that('track reconciliation works', {
  charts <- day_track_charts() %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    with_matched_tracks() %>%
    collect

  track_ids <- charts$`track$id`

  # Somewhat blunt condition: if we're matching tracks, then we should
  # see some ids.
  expect_true(sum(!is.na(track_ids)) > 100)
})

test_that('filtering charts by source works', {
  charts <- day_track_charts() %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_source('itunes', 'spotify') %>%
    collect %>%
    with_source_names()

  expect_equal(
    sort(unique(charts$source_name)), c('ITunes', 'Spotify')
  )
})

test_that('filtering charts by right holder works', {
  charts <- day_track_charts() %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_right_holder('Marília Mendonça') %>%
    collect

  expected_tracks <- tolower(c(
    'Dois Enganados',
    'Estrelinha',
    'Eu Sei De Cor',
    'Infiel',
    'Passa Mal',
    'Some Que Ele Vem Atras'
  ))

  expect_equal(
    charts %>%
      mutate(title.tracks = tolower(title.tracks)) %>%
      filter(!grepl('ao vivo', title.tracks)) %>%
      pull(title.tracks) %>%
      unique %>%
      sort,
    expected_tracks
  )
})
