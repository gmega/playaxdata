context('location_social_metrics')

setup({ playaxdata:::setup_db_access() })

test_that('source filters work', {
  data <- city_social_metrics() %>%
    for_right_holder('MC Kevinho') %>%
    for_source('spotify') %>%
    for_dates(start = '2021-01-01', end = '2021-01-25') %>%
    collect

  ids <- right_holder_external_ids() %>%
    for_right_holder('MC Kevinho') %>%
    for_source('Spotify') %>%
    collect

  expect_gt(nrow(data), 1)
  expect_equal(unique(data$source_name), 'Spotify')

  expect_true(all(data$source_id %in% ids$source_id))
})

test_that('metric type filters work', {
  data <- city_social_metrics() %>%
    for_right_holder('MC Kevinho') %>%
    for_source('spotify') %>%
    for_dates(start = '2021-01-01', end = '2021-01-25') %>%
    for_metric_type('plays') %>%
    collect %>%
    with_metric_types()

  expect_gt(nrow(data), 1)
  expect_equal(unique(data$source_name), 'spotify')
  expect_equal(unique(data$metric_type), 'plays')

})
