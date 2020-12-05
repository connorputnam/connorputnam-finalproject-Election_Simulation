Johnson Distribution
================

``` r
polls <- read.csv(here::here("Data" , "senate_polls.csv"))
polls <- georgia::initial_poll_cleaning(polls)
```

# Johnson vs Normal

``` r
n <- 100000
parmamter_data <- polls %>% 
  group_by(candidate_name) %>%
  select(candidate_name, pct) %>%
  filter(candidate_name == "David A. Perdue" | candidate_name == "Jon Ossoff")
paramters <- JohnsonFit(parmamter_data$pct)

johnson_dist <- rJohnson(n = n, 
                         parms = list(gamma = 0, 
                                      delta = .5, 
                                      xi = 0, #no really sure what this parameter is doing...
                                      lambda = 2, 
                                      type = "SN"))

normal_dist <- rnorm(n = n, mean = 0, sd = 1)

overlay <- melt(as.data.frame(cbind(johnson_dist, normal_dist)))
```

    ## No id variables; using all as measure variables

``` r
ggplot(overlay, aes(value, fill = variable)) +
  geom_density(alpha = 0.5)
```

![](01_Johnson_Distribution_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#fat tails!
```
