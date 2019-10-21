# expectdata
Test Expectations of a Data Frame

Expectdata is an R package that makes it easy to check assumptions about a data frame before conducting analyses. Below is a concise tour of some of the things expectdata can do for you.

Check for unexpected duplication
--------------------------------

``` r
library(expectdata)
expect_no_duplicates(mtcars, "cyl")
#> [1] "top duplicates..."
#> # A tibble: 3 x 2
#> # Groups:   cyl [3]
#>     cyl     n
#>   <dbl> <int>
#> 1     8    14
#> 2     4    11
#> 3     6     7
#> Error: Duplicates detected in column: cyl
```

The default `return_df == TRUE` option allows for using these function as part of a dplyr piped expression that is stopped when data assumptions are not kept.

``` r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
mtcars %>% 
  filter(cyl == 4) %>% 
  expect_no_duplicates("wt", return_df = TRUE) %>% 
  ggplot(aes(x = wt, y = hp, color = mpg, size = mpg)) +
  geom_point()
#> [1] "no wt duplicates...OK"
```

![](/private/var/folders/sn/drzwy6y91z11g0l94pkv897j6nq3zl/T/Rtmpx7o5h4/preview-10032104d601f.dir/expect_data_files/figure-markdown_github/unnamed-chunk-2-1.png)

If there are no expectations violated, an "OK" message is printed.

After joining two data sets you may want to verify that no unintended duplication occurred. Expectdata allows comparing pre- and post- processing to ensure they have the same number of rows before continuing.

``` r
expect_same_number_of_rows(mtcars, mtcars, return_df = FALSE)
#> [1] "Same number of rows...OK"
expect_same_number_of_rows(mtcars, iris, show_fails = FALSE, stop_if_fail = FALSE, return_df = FALSE)
#> Warning: Different number of rows: 32 vs: 150

# can also compare to no df2 to check is zero rows
expect_same_number_of_rows(mtcars, show_fails = FALSE, stop_if_fail = FALSE, return_df = FALSE) 
#> Warning: Different number of rows: 32 vs: 0
```

Can see how the `stop_if_fail = FALSE` option will turn failed expectations into warnings instead of errors.

Check for existance of problematic rows
---------------------------------------

Comparing a data frame to an empty, zero-length data frame can also be done more explicitly. If the expectations fail, cases can be shown to begin the next step of exploring why these showed up.

``` r
expect_zero_rows(mtcars[mtcars$cyl == 0, ], return_df = TRUE)
#> [1] "No rows found as expected...OK"
#>  [1] mpg  cyl  disp hp   drat wt   qsec vs   am   gear carb
#> <0 rows> (or 0-length row.names)
expect_zero_rows(mtcars$cyl[mtcars$cyl == 0])
#> [1] "No rows found as expected...OK"
#> numeric(0)
expect_zero_rows(mtcars, show_fails = TRUE)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#> Error: Different number of rows: 32 vs: 0
```

This works well at the end of a pipeline that starts with a data frame, runs some logic to filter to cases that should not exist, then runs `expect_zero_rows()` to check no cases exist.

``` r
# verify no cars have zero cylindars
mtcars %>% 
  filter(cyl == 0) %>% 
  expect_zero_rows(return_df = FALSE)
#> [1] "No rows found as expected...OK"
```

Can also check for NAs in a vector, specific columns of a data frame, or a whole data frame.

``` r
expect_no_nas(mtcars, "cyl", return_df = FALSE)
#> [1] "Detected 0 NAs...OK"
expect_no_nas(mtcars, return_df = FALSE)
#> [1] "Detected 0 NAs...OK"
expect_no_nas(c(0, 3, 4, 5))
#> [1] "Detected 0 NAs...OK"
#> [1] 0 3 4 5
expect_no_nas(c(0, 3, NA, 5))
#> Error: Detected 1 NAs
```
