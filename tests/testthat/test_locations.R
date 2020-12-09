context('locations')

test_that('resolve_location resolves country', {
  expect_equal(
    playaxdata:::resolve_location(country = 'Brazil'),
    list(location_type = 3, location_id = 32)
  )
})

test_that('resolve_location resolves region', {
  expect_equal(
    playaxdata:::resolve_location(region = 'Sudeste'),
    list(location_type = 2, location_id = 4)
  )
})

test_that('resolve_location resolves state', {
  expect_equal(
    playaxdata:::resolve_location(state = 'São Paulo'),
    list(location_type = 1, location_id = 25)
  )
})

test_that('resolve_location resolves city', {
  expect_equal(
    playaxdata:::resolve_location(city = 'São Paulo'),
    list(location_type = 0, location_id = 2466)
  )
})

test_that('resolve_location complains when city name is ambiguous', {
  expect_error(
    playaxdata:::resolve_location(city = 'Santa Cruz'),
    'Ambiguous city name Santa Cruz. Must specify state.'
  )
})

test_that('resolve_location works for ambiguous city names if state is specified', {
  expect_equal(
    playaxdata:::resolve_location(city = 'Santa Cruz', state = 'Paraíba'),
    list(location_type = 0, location_id = 1198)
  )
})
