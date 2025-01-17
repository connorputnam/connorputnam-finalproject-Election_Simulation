Weight Polling
================

``` r
polls <- read.csv(here::here("Data" , "senate_polls.csv"))
polls <- georgia::initial_poll_cleaning(polls)
```

# Bootstrapping with Weights

Lastly, I wanted to see the effects of the different poll rating. To do
this I bootstrapped the polls in regards to the assigned weights for
each pollster score. The function `boot_data_weights` can take in custom
weights, as long as those assigned weights are decimals that add up to
one.

``` r
boot_data <- read.csv(here::here("Data", "bootData.csv"))
boot_spread <- map(1:10000, ~sample(boot_data$actual_spread, 
                                    size = length(boot_data), 
                                    replace = TRUE)) %>%
  map_dbl(mean)
boot_spread <- melt(boot_spread)
```

``` r
boot_data_weights <- function(A_plus, A, A_minus, A_B, B_plus, B, B_C, Cplus, C, C_D, No_rating, N_A){
  boot_data %>% group_by(fte_grade) %>%
                  mutate(Weights = case_when(fte_grade == "A+" ~ A_plus,
                             fte_grade == "A" ~ A,
                             fte_grade == "A-" ~ A_minus,
                             fte_grade == "A/B" ~ A_B,
                             fte_grade == "B+" ~ B_plus,
                             fte_grade == "B" ~ B,
                             fte_grade == "B/C" ~ B_C,
                             fte_grade == "C+" ~ Cplus,
                             fte_grade == "C" ~ C,
                             fte_grade == "C/D" ~ C_D,
                             fte_grade == "No Rating" ~ No_rating)) %>%
                  replace(is.na(.), N_A) %>%
                  group_by(Weights)
}

#Here you can play around with the weights

boot_data_weights <- boot_data_weights(A_plus = 0.2, A = 0.15, A_minus = 0.15, A_B = 0.1, 
                  B_plus = 0.1, B = 0.05, B_C = 0.05, Cplus = 0.05, 
                  C = 0.05, C_D = 0.05, No_rating = 0.025, N_A = 0.025)


check <- sample(boot_data_weights$fte_grade, size = 10000, 
                replace = TRUE, 
                prob = boot_data_weights$Weights)
check <- reshape2::melt(check)
check <- check %>%
  count(value)

ggplot(check, aes(value, n, fill = value)) +
  geom_bar(stat = "identity", color = "black") +
  theme_fivethirtyeight() +
  ggtitle("Distribution of Ratings")
```

![](05_weighted_sampling_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#looks like a good distribution
```

``` r
#Following is a similar process as in `03_candidate_probabilities.Rmd`
#Creating the bootstrap for the data based on the different inputed weights
boot_spread_weights <- map(1:10000, ~sample(boot_data$actual_spread, 
                                            size = length(boot_data$actual_spread), 
                                            replace = TRUE, 
                                            prob = boot_data_weights$Weights)) %>%
  map_dbl(mean)

boot_spread_weights <- melt(boot_spread_weights)
ggplot(boot_spread_weights, aes(value)) +
  geom_histogram(aes(y=..density..), fill = "steelblue", color = "black") +
  stat_function(fun = dnorm, args = c(mean = mean(boot_spread_weights$value), 
                                      sd = sd(boot_spread_weights$value))) +
  geom_vline(xintercept = mean(boot_spread_weights$value)) +
  labs(title = "Bootstrapped Spreads")
```

![](05_weighted_sampling_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
fit <- MASS:: fitdistr(boot_spread_weights$value, "normal")
  
n <- 100000

empty_vec <- rep((boot_data_weights %>%
                    group_by(fte_grade) %>%
                    filter(candidate_name == "David A. Perdue") %>%
                    summarise(average_spread = weighted.mean(actual_spread, Weights)) %>%
                    
                    dplyr::select(average_spread)), n) %>%
  flatten_dbl()

boot_spread_weights <- boot_spread_weights %>% flatten_dbl()

#This is where the actual predictions are now taking place

Perdue <- (sd(boot_spread_weights) * rnorm(n, mean(boot_spread_weights), 
                                                     sd(boot_spread_weights))) + empty_vec 

# in terms of ossof
empty_vec_ossof <- rep((boot_data_weights %>%
                          group_by(fte_grade) %>%
                          filter(candidate_name == "Jon Ossoff") %>%
                          summarise(average_spread = weighted.mean(actual_spread, Weights)) %>%
                          
                          dplyr::select(average_spread)), n) %>%
  flatten_dbl()

#This is where the actual predictions are now taking place


Ossof <- (sd(boot_spread_weights) * rnorm(n, mean(boot_spread_weights), 
                                                     sd(boot_spread_weights))) + empty_vec_ossof 


combined_probs <- melt(as.data.frame(cbind(Perdue, Ossof)))
combined_probs <- combined_probs %>% 
  group_by(variable) %>%
  mutate_at(vars(variable), as.character) %>%
  mutate(wining_color = case_when((variable == "Perdue" & value > 0) ~ "win",
                                  (variable == "Perdue" & value < 0) ~ "lose",
                                  (variable == "Ossof" & value > 0) ~ "win",
                                  (variable == "Ossof" & value < 0) ~ "lose"))
n <- 1000000

combined_probs <- combined_probs %>%
  mutate(prob_winning = case_when((variable == "Perdue" & 
                                     wining_color == "win" ~ 
                                     length(which(Perdue > 0)) / n),
                                  (variable == "Perdue" & 
                                     wining_color == "lose" ~ 
                                     length(which(Perdue < 0)) / n),
                                  (variable == "Ossof" & 
                                     wining_color == "win" ~ 
                                     length(which(Ossof > 0)) / n),
                                  (variable == "Ossof" & 
                                     wining_color == "lose" ~ 
                                     length(which(Ossof < 0)) / n)))
```

Here is an example of the weighted distribution of probabilities for
Perdue.

``` r
georgia::probability_winning_plot("Perdue")
```

![](05_weighted_sampling_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
