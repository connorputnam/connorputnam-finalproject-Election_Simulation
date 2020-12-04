
#' @title Probability of Winning
#' @description Plots the probabilities for Georgia Senate Runoff candidates David Perdue and Jon Ossoff using bootstrapping methods
#' @param candidate Enter the either "Perdue" or "Ossoff"
#' @return Returns a plot
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[ggthemes]{theme_fivethirtyeight}}
#' @rdname probability_winning_plot
#' @export 
#' @importFrom ggthemes theme_fivethirtyeight
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



#' @title DataSet for probability_winning_plot function
#' @description Allows the user to choose between the actual and fake datasets
#' @param data_set The data the user chooses
#' @return polling data
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @rdname probability_dataset

probability_dataset <- function(data_set){

#combined_probs <- melt(as.data.frame(cbind(Perdue, Ossof)))
combined_probs <- data_set %>% 
  group_by(variable) %>%
  mutate_at(vars(variable), as.character) %>%
  mutate(wining_color = case_when((variable == "Perdue" & value > 0) ~ "win",
                                  (variable == "Perdue" & value < 0) ~ "lose",
                                  (variable == "Ossof" & value > 0) ~ "win",
                                  (variable == "Ossof" & value < 0) ~ "lose"))
#mutate(color_perdue = ifelse(value > 0, "red", "blue")) %>%
#mutate(color_ossof = ifelse(variable 0 & value > 0, "blue", "red"))
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

}


#' @title Initial Poll Cleaning
#' @description Takes the 538 polling csv and maks it useable for this repo
#' @param data the .csv data
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @rdname initial_poll_cleaning

initial_poll_cleaning <- function(data){
  data %>%
  filter(state == "Georgia") %>%
  filter(race_id == "7780" | race_id == "6271") %>%
  group_by(candidate_name)
}
