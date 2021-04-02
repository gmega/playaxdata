context('period_metrics')

rh_name <- 'Marília Mendonça'

setup({ setup_db_access() })

test_that('with_source_types attaches source types', {
  metrics <- week_metrics() %>%
    for_right_holder(rh_name) %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    collect

  expect_equal(class(metrics$source_type), 'integer')

  metrics <- metrics %>% with_source_names() %>% filter(!is.na(source_type))

  # We should have at least some rows left.
  expect_gt(nrow(metrics), 50)

  expect_true(all(metrics$source_type %in% c(source_name_mapping$source_name)))
})


test_that('with_metric_types attaches source types', {
  metrics <- week_metrics() %>%
    for_right_holder(rh_name) %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    collect

  expect_equal(class(metrics$metric_type), 'integer')

  metrics <- metrics %>% with_metric_types()

  # We should have at least some rows left.
  expect_gt(nrow(metrics), 50)

  expect_true(all(metrics$metric_type %in% unname(unlist(playaxdata:::STANDARD_METRICS))))
})

test_that('collect.period_metrics does not attempt to
          convert dates when column is absent', {
  rhid <- week_metrics() %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_right_holder('Marília Mendonça') %>%
    pull(right_holder_id) %>%
    unique

  expect_equal(rhid, find_right_holder(rh_name))
})

test_that('multiple right holder queries work', {
  rhids <- week_metrics() %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_right_holder('Marília Mendonça', 'Barões da Pisadinha') %>%
    pull(right_holder_id) %>%
    unique %>%
    sort

  expected_rhids <- c(43744, 668344)

  expect_equal(rhids, expected_rhids)
})

test_that('date column is of Date type', {
  weeks <- week_metrics() %>%
    head(10) %>%
    collect %>%
    pull(date)

  expect_s3_class(weeks, 'Date')

  months <- month_metrics() %>%
    head(10) %>%
    collect %>%
    pull(date)


  expect_s3_class(months, 'Date')
})

test_that('multiple metric type filters work', {
  metrics <- week_metrics() %>%
    for_right_holder(rh_name) %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_metric_type('plays', 'followers') %>%
    collect %>%
    with_metric_types()

  expect_equal(
    metrics %>% pull(metric_type) %>% unique %>% sort,
    c('followers', 'plays')
  )
})

test_that('multiple source filters work', {
  metrics <- week_metrics() %>%
    for_right_holder(rh_name) %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_source('youtube', 'spotify', 'knowledgegraph') %>%
    collect %>%
    with_source_names()

  expect_equal(
    metrics %>% pull(source_type) %>% unique %>% sort %>% tolower,
    c('knowledgegraph', 'spotify', 'youtube')
  )
})

test_that('supported_metric_types returns meaningful results', {
  metric_types <- unlist(month_metrics() %>% supported_metric_types('spotify'))
  expect_equal(sort(metric_types), c('active_audience', 'followers', 'plays'))
})

test_that('specific city filters work', {
  metrics <- week_metrics() %>%
    for_location(city = 'São Paulo') %>%
    for_dates('2020-01-01', '2020-01-10') %>%
    for_right_holder(rh_name) %>%
    collect %>%
    with_source_names()

  expected_location <- playaxdata:::resolve_location(city = 'São Paulo')

  # should see at least radio and youtube
  expect_true(all(c('Radio', 'KnowledgeGraph') %in% metrics$source_type))

  # should see only São Paulo
  expect_equal(unique(metrics$location_type), expected_location$location_type)
  expect_equal(unique(metrics$location_id), expected_location$location_id)
})

test_that('location type filters work', {
  metrics <- week_metrics() %>%
    for_location_type(location_type = 'city') %>%
    for_dates('2020-01-01', '2020-01-10') %>%
    for_right_holder(rh_name) %>%
    collect

  expect_equal(unique(metrics$location_type),
               playaxdata:::resolve_location_type('city'))
})

test_that('location label enrichments work', {
  metrics <- week_metrics() %>%
    for_right_holder('Anitta') %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    collect %>%
    with_location_names()

  # we test by sampling ;-)
  expect_true(
    all(
      c('Rio de Janeiro', 'Maceio', 'São Paulo',
        'Rio Grande do Norte', 'Nordeste') %in%
        metrics$location_id
    )
  )
})

