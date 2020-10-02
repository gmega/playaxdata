context('raw_social_metrics')

rh_name <- 'Marília Mendonça'

setup({ setup_db_access() })

test_that('date constraints work', {
  dates <- day_metrics() %>%
    for_dates('2020-01-01', '2020-01-02') %>%
    for_right_holder(rh_name) %>%
    pull(metric_date) %>%
    unique %>%
    sort

  expect_equal(
    as.POSIXct(c('2020-01-01', '2020-01-02')),
    dates
  )
})

test_that('metric name resolution works', {
  metrics <- day_metrics() %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    for_right_holder(rh_name) %>%
    collect %>%
    arrange(id)

  # with_source_names will only preserve (source_name, metric_type) pairs
  # it can map. So we have to drop everything that is unmappable from our
  # expected results.
  expected_indices <- metrics %>%
    inner_join(metric_type_mapping %>%
                 select(source_name, metric_type = metric_type_from),
               by = c('source_name', 'metric_type')) %>%
    pull(source_name)

  # now, fetch expected names from source_name_mapping
  expected_names <- sapply(expected_indices, function(i)
    source_name_mapping %>%
      filter(raw_social_metrics_index == i) %>%
      pull(source_name) %>%
      tolower
  )

  expect_equal(
    metrics %>% with_source_names() %>% arrange(id) %>% pull(source_name),
    expected_names
  )
})

test_that('diff_metrics diffs cumulative for single right holder', {
  metrics <- day_metrics() %>%
    for_dates('2020-01-01', '2020-01-05') %>%
    for_source('youtube') %>%
    for_metric_type('plays') %>%
    for_right_holder(rh_name) %>%
    collect %>%
    arrange(metric_date)

  expected_values <- c(NA, diff(metrics$value))
  actual_values <- metrics %>%
    with_metric_types %>%
    diff_metrics %>%
    pull(value)

  expect_equal(
    expected_values,
    actual_values
  )
})

test_that('diff_metrics diffs cumulative metrics for multiple right holders', {
  metrics <- day_metrics() %>%
    for_dates('2020-01-01', '2020-01-05') %>%
    for_source('youtube') %>%
    for_metric_type('plays') %>%
    with_right_holders() %>%
    filter(right_holder_id == 43744 | right_holder_id == 508) %>%
    collect

  pull_values <- function(rh_id) {
    metrics %>%
      for_right_holder(rh_id) %>%
      arrange(metric_date) %>%
      pull(value)
  }

  # Manually Deaccumulates youtube plays for both artists.
  yt_43744 <- c(NA, pull_values(43744) %>% diff)
  yt_508 <-  c(NA, pull_values(508) %>% diff)

  metrics <- metrics %>% with_source_names() %>% diff_metrics()

  expect_equal(yt_43744, pull_values(43744))
  expect_equal(yt_508, pull_values(508))
})
