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
