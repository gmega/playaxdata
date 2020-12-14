context('right_holders')

setup({ playaxdata:::setup_db_access() })

test_that('right holder filter works', {
  expect_equal(
    find_right_holder('Marília Mendonça'),
    right_holders() %>% for_right_holder('Marília Mendonça') %>% pull(id)
  )
})
