context('web_metrics')

rh_name <- 'Ludmilla'

setup({ setup_db_access() })

test_that('multiple metric filter works', {
  metric_types <- web_metrics(with_right_holders = TRUE) %>%
    for_right_holder('Ludmilla') %>%
    for_metric_type('plays', 'active_audience') %>%
    for_dates('2020-01-01', '2020-01-05') %>%
    pull(metric_type) %>%
    unique %>%
    sort

  expect_equal(
    metric_types,
    sort(playaxdata:::match_metrics(c('plays', 'active_audience')))
  )
})

test_that('multiple source filter works', {
  source_types <- web_metrics() %>%
    for_dates('2020-01-01', '2020-01-02') %>%
    for_source('spotify', 'deezer') %>%
    collect %>%
    with_source_names() %>%
    pull(source_name) %>%
    unique %>%
    sort

  expect_equal(
    source_types,
    c('Deezer', 'Spotify')
  )
})
