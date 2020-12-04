#' Title
#'
#' @param candidate 
#'
#' @return
#' @export
#'
#' @examples
probability_winning_plot <- function(candidate){
  ggplot((combined_probs %>% filter(variable == sprintf(candidate))), aes(value)) +
    geom_histogram(aes(fill = wining_color), color = "black", bins = 30) +
    ggthemes::theme_fivethirtyeight() +
    theme(axis.title = element_text()) + xlab("Spread") + ylab("Number of Observations") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(breaks = c(-5:5)) +
    labs(title = sprintf("Probability of Winning vs Losing for %s", candidate)) +
    scale_fill_manual(values = c("#D55E00", "#009E73")) +
    annotate(geom = "text", x = (as.numeric(combined_probs %>% 
                                              filter(variable == candidate) %>% 
                                              summarise(average = mean(value)) %>% 
                                              flatten_chr())[2]) + 4, 
             y = 8100, 
             label = sprintf("Winning : %s", 
                             combined_probs %>% 
                               filter(wining_color == "win" & variable == candidate) %>% 
                               summarise(label_percent(accuracy = 0.01)(mean(prob_winning))) %>% 
                               select(-variable) %>% flatten_chr()),
             color = "#009E73") +
    annotate(geom = "text", x = (as.numeric(combined_probs %>% 
                                              filter(variable == candidate) %>% 
                                              summarise(average = mean(value)) %>% 
                                              flatten_chr())[2]) - 4,
             y = 8100, label = sprintf("Losing : %s", 
                                       combined_probs %>% 
                                         filter(wining_color == "lose" & variable == candidate) %>% 
                                         summarise(label_percent(accuracy = 0.01)(mean(prob_winning))) %>% 
                                         select(-variable) %>% flatten_chr()), 
             color = "#D55E00")
}
