A simple dpylr verb that pushes changes onto an existing tibble.

### Patch by key

    mtcars2 <- rownames_to_column(mtcars, "model") %>%
      head(3) %>% select(model, mpg, cyl, hp, drat, wt)

    mtcars2

    ##           model  mpg cyl  hp drat    wt
    ## 1     Mazda RX4 21.0   6 110 3.90 2.620
    ## 2 Mazda RX4 Wag 21.0   6 110 3.90 2.875
    ## 3    Datsun 710 22.8   4  93 3.85 2.320

    patches <- frame_data(
      ~ model, ~ mpg, ~ cyl, ~ wt,
      "Mazda RX4", 500, NA, 200,
      "Datsun 710", NA, 8, NA
    )

    patches

    ## # A tibble: 2 x 4
    ##        model   mpg   cyl    wt
    ##        <chr> <dbl> <dbl> <dbl>
    ## 1  Mazda RX4   500    NA   200
    ## 2 Datsun 710    NA     8    NA

    mtcars2 %>% patch(patches, by="model")

    ## Patching, Columns = "model"

    ##           model   mpg cyl  hp drat      wt
    ## 1     Mazda RX4 500.0   6 110 3.90 200.000
    ## 2 Mazda RX4 Wag  21.0   6 110 3.90   2.875
    ## 3    Datsun 710  22.8   8  93 3.85   2.320

### Patch only `cyl`

    mtcars2 %>% patch(patches, cyl, by="model")

    ##           model  mpg cyl  hp drat    wt
    ## 1     Mazda RX4 21.0   6 110 3.90 2.620
    ## 2 Mazda RX4 Wag 21.0   6 110 3.90 2.875
    ## 3    Datsun 710 22.8   8  93 3.85 2.320
