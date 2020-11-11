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

  expect_true(all(metrics$metric_type %in% unname(unlist(STANDARD_METRICS))))
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
