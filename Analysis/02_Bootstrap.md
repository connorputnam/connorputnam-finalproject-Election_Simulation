02\_Bootstrap
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
polls <- read.csv(here::here("Data","senate_polls.csv"))
polls <- polls %>%
  filter(state == "Georgia") %>%
  filter(race_id == "7780" | race_id == "6271") %>%
  group_by(candidate_name)
```

# Bootstrapping

``` r
boot_data <- polls %>% 
  group_by(candidate_name, candidate_party, cycle) %>%
  filter(candidate_name == "David A. Perdue" | candidate_name == "Jon Ossoff")

boot_data <- boot_data %>%
  mutate(candidate_party = recode(candidate_party, 
                                  `REP` = "R", 
                                  `DEM` = "D")) %>%
  filter(candidate_party == "R" | candidate_party == "D") %>% #2020 election will not have independents, only top two advance
  group_by(question_id) %>%
  #arrange(question_id, candidate_party) %>%
  select(question_id, poll_id, fte_grade, sample_size, candidate_name, candidate_party, pct) %>%
  filter(question_id != 123442 & question_id != 123443) %>%
  mutate(spread = pct - pct[candidate_party == "D"]) %>%
  mutate(spread2 = pct - pct[candidate_party == "R"]) %>%
  mutate(actual_spread = spread + spread2) %>%
  select(-spread, -spread2)
boot_data
```

    ## # A tibble: 114 x 8
    ## # Groups:   question_id [57]
    ##    question_id poll_id fte_grade sample_size candidate_name candidate_party
    ##          <int>   <int> <fct>           <int> <fct>          <fct>          
    ##  1      135283   72146 A+                504 Jon Ossoff     D              
    ##  2      135283   72146 A+                504 David A. Perd… R              
    ##  3      135284   72146 A+                504 Jon Ossoff     D              
    ##  4      135284   72146 A+                504 David A. Perd… R              
    ##  5      135285   72146 A+                504 Jon Ossoff     D              
    ##  6      135285   72146 A+                504 David A. Perd… R              
    ##  7      135052   72040 B/C              1041 Jon Ossoff     D              
    ##  8      135052   72040 B/C              1041 David A. Perd… R              
    ##  9      134751   71888 B                1090 Jon Ossoff     D              
    ## 10      134751   71888 B                1090 David A. Perd… R              
    ## # … with 104 more rows, and 2 more variables: pct <dbl>, actual_spread <dbl>

``` r
ggplot(boot_data, aes(actual_spread)) +
  geom_histogram(bins = 15)
```

![](02_Bootstrap_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
boot_spread <- map(1:10000, ~sample(boot_data$actual_spread, size = length(boot_data), replace = TRUE)) %>%
  map_dbl(mean)

boot_spread <- melt(boot_spread)
ggplot(boot_spread, aes(value)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, args = c(mean = mean(boot_spread$value), sd = sd(boot_spread$value))) +
  geom_vline(xintercept = mean(boot_spread$value)) +
  labs(title = "Bootstrapped Spreads")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02_Bootstrap_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
