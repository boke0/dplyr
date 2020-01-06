test_that("can selectively ungroup", {
  gf <- tibble(x = 1, y = 2) %>% group_by(x, y)

  expect_equal(gf %>% ungroup() %>% group_vars(), character())
  expect_equal(gf %>% ungroup(everything()) %>% group_vars(), character())
  expect_equal(gf %>% ungroup(x) %>% group_vars(), "y")
})

# Errors ------------------------------------------------------------------

test_that("selective ungroup() give meaningful errors", {
  verify_output(test_path("test-grouped-df-errors.txt"), {
    tibble(x = 1, y = 2) %>%
      group_by(x, y) %>%
      ungroup(z)

    tibble(x = 1) %>%
      ungroup(x)
  })
})
