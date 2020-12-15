context('misc')

setup({ setup_db_access() })

period_metrics_schema <- tibble(
  right_holder_id = 'integer',
  source_type = 'integer',
  metric_type = 'integer',
  date = 'Date',
  value = 'numeric',
  value_delta = 'numeric',
  value_growth = 'numeric',
  position = 'integer',
  position_delta = 'integer',
  id = 'numeric',
  location_type = 'integer',
  location_id = 'integer',
  created_at = 'character',
  updated_at = 'Date'
)

test_that('classes are correctly identified for database tables', {
  expect_equal(
    playaxdata:::col_classes(month_metrics()) %>% select(order(colnames(.))),
    period_metrics_schema %>% select(order(colnames(.)))
  )
})

test_that('classes are correctly identified for in-memory tables', {
  expect_equal(
    playaxdata:::col_classes(month_metrics() %>%
                               head(10) %>%
                               collect %>%
                               select(order(colnames(.)))),
    period_metrics_schema %>% select(order(colnames(.)))
  )
})
