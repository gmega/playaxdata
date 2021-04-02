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

test_that('city filters work for city table', {
  metrics <- city_social_metrics() %>%
    for_location(city = 'Santos', state = 'São Paulo') %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_right_holder('Diogo Nogueira') %>%
    collect

  expect_gte(nrow(metrics), 0)
  expect_equal(unique(metrics$city_id), 2452)
})

test_that('state filters work for state table', {
  metrics <- state_social_metrics() %>%
    for_location(state = 'São Paulo') %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_right_holder('Diogo Nogueira') %>%
    collect

  expect_gte(nrow(metrics), 0)
  expect_equal(unique(metrics$state_id), 25)
})

test_that('state filters work for state table', {
  metrics <- region_social_metrics() %>%
    for_location(region = 'Sudeste') %>%
    for_dates('2020-01-01', '2020-01-31') %>%
    for_right_holder('Diogo Nogueira') %>%
    for_source('Spotify') %>%
    collect

  expect_gte(nrow(metrics), 0)
  expect_equal(unique(metrics$source_name), 'Spotify')
  expect_equal(unique(metrics$region_id), 4)
})
