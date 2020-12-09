context('raw_social_metrics')

rh_name <- 'Marília Mendonça'

setup({ playaxdata:::setup_db_access() })

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

  expected_names <- metrics %>%
    left_join(playaxdata:::rsm_metrics() %>%
                mutate(metric_name =
                         coalesce(standard_name, non_standard_name)) %>%
                select(source_name, metric_type, source_name_str),
               by = c('source_name', 'metric_type')) %>%
    arrange(id) %>%
    pull(source_name_str) %>%
    unname

  expect_equal(
    metrics %>%
      with_source_names() %>%
      arrange(id) %>%
      pull(source_name),
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

test_that('supported_sources returns meaningful results', {
  sources <- day_metrics() %>% supported_sources()
  # There could be more sources, but we expect to see at least those.
  expect_true(all(
    c('deezer', 'facebook', 'instagram', 'napster',
      'shazam', 'spotify', 'twitter', 'youtube') %in% sources)
  )
})

test_that('supported_sources actually uses the underlying table', {
  sources <- raw_social_metrics() %>%
    for_right_holder('Barões da Pisadinha') %>%
    for_dates('2019-01-01', '2019-01-01') %>%
    supported_sources

  expect_equal(sources, 'spotify')

  sources <- raw_social_metrics() %>%
    for_right_holder('Barões da Pisadinha') %>%
    for_dates('2020-01-01', '2020-01-01') %>%
    supported_sources

  expect_equal(sources, c('deezer', 'spotify', 'twitter'))
})

test_that('supported_metric_types returns meaningful results', {
  expect_equal(
    sort(day_metrics() %>% supported_metric_types('spotify')),
    c('active_audience', 'followers', 'plays')
  )
})

test_that('supported_metric_types returns an error for supported sources
          without mapped metrics', {
  # I'll use napster as we'll probably never bother to map this. :-)
  expect_error(
    day_metrics() %>% supported_metric_types('napster'),
    'Source <<napster>> is supported, but has no mapped metric types.'
  )
})

test_that('metric type selection works without source', {
  metrics <- day_metrics() %>%
    for_right_holder(rh_name) %>%
    for_metric_type('plays') %>%
    for_dates('2020-05-01', '2020-05-01') %>%
    collect

  plays <- item_index(playaxdata:::STANDARD_METRICS, 'plays')
  play_types <- playaxdata:::std_metrics() %>%
    filter(metric_type_str == 'plays') %>%
    pull(metric_type) %>%
    unique %>%
    sort

  expect_equal(sort(unique(metrics$metric_type)), play_types)
})

test_that('metric type selection does not scoop unintended metrics', {
  metrics <- day_metrics() %>%
    for_right_holder(rh_name) %>%
    for_metric_type('plays') %>%
    for_dates('2020-05-01', '2020-05-01') %>%
    collect

  play_indices <- playaxdata:::std_metrics() %>%
    filter(metric_type_str == 'plays') %>%
    pull(metric_type) %>%
    unique

  ambiguous <- playaxdata:::std_metrics() %>%
    filter(metric_type %in% play_indices, metric_type_str != 'plays') %>%
    head(1)

  # Should have at least one ambiguous metric.
  expect_true(nrow(ambiguous) >= 1)

  wrong_fetches <- metrics %>%
    filter(metric_type == ambiguous$metric_type,
           source_name == ambiguous$source_name) %>%
    nrow

  expect_equal(wrong_fetches, 0)
})

test_that('multiple source selection works', {
  sources <- day_metrics() %>%
    for_source('facebook', 'knowledgegraph') %>%
    for_right_holder(rh_name) %>%
    for_dates('2020-05-01', '2020-05-01') %>%
    collect %>%
    with_source_names() %>%
    pull(source_name) %>%
    unique %>%
    sort

  expect_equal(sources, c('facebook', 'knowledgegraph'))
})
