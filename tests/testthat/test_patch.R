context("patch")

require(testthat)
require(tibble)
require(dplyr)

mtcars2 <-
  rownames_to_column(mtcars, "model") %>%
  head(3) %>% select(model, mpg, cyl, hp, drat, wt)
patches <- frame_data(
  ~ model, ~ mpg, ~ wt,
  "Mazda RX4",   500,  200
)

test_that("patch passes a simple test", {
  mtcars3 <- mtcars2 %>% patch(patches, by="model")


  expect_equal( nrow(mtcars3), nrow(mtcars2) )

  rx4 <- mtcars3 %>% filter(model=="Mazda RX4")

  expect_equal(rx4$mpg, 500)
  expect_equal(rx4$wt,  200)
} )

test_that("patches must be one to one w/r/t thing being patched", {
  bad_patch <- rbind(patches, patches)
  bad_patch$mpg <- c(500,100)
  expect_error(
    mtcars2 %>% patch(bad_patch, by="model")
  )
})

test_that("patch only works on selected columns when specified", {
 mtcars3 <- mtcars2 %>% patch(patches, wt, by="model")

 rx4_orig <- mtcars2 %>% filter(model == "Mazda RX4")
 rx4      <- mtcars3 %>% filter(model == "Mazda RX4")

 expect_equal(rx4$mpg, rx4_orig$mpg)
 expect_equal(rx4$wt,  200)
})

