
# Predicting the 2021 Georgia Senate Runoff
## Connor Putnam
<!-- badges: start -->
<!-- badges: end -->

The goal of this project is to simulate and obtain probabilities for the January 5, 2021 Georgia Senate Runoff Election between incumbent Republican Senator David Purdue and Democratic challenger Jon Ossoff. The data for this project was sourced from the fivethirtyeight.com database. https://data.fivethirtyeight.com/

The code is split into six main files under the `Analysis` folder. Additionally the script `06_functions.R` creates the `georgia` package and that script needs to be ran first. After that the following six files can be ran independently of one another. But I recommend following the order outlined below in order to get a better narrative of the project.

[ ] **Exploratory Data Analysis** `00_EDA.Rmd`
  
  * This file will explores some visuals regarding the polling data, which helped influence the later approaches.
  
[ ] **Johnson vs Normal** `01_Johnson_Distribution.Rmd`
  
  * Duo to the partisan nature of the United States electorate, this file explores the possibility that prediction should be based on a distribution with "fatter" tails. 
  
[ ] **Boot Strapping the Polling Data** `02_Bootstrap.Rmd`
  
  * With a limited number of polling done before the election bootstrapping helped achieve an adequate sample size.
  
[ ] **Computing Probabilities of a Candidate Winning** `03_candidate_probabilities.Rmd`
  
  * The end product, involving a visualization and computation of the bootstrapped polling spread and the resulting assigned probabilities. These will change slightly each time the code is ran, but most likely not by more than a few percentage points.
  
[ ] **Testing the Bootstrapped Probabilities with Fake Data** `04_fake_polling.Rmd`
  
  * Fake data was created in order to the check the model for unreasonable outcomes. i.e. if the polling data shows one candidate with a   
  double digit pointing lead then this candidate should have a rather high probability of winning.
  
[ ] **Testing the Bootstrapped Probabilities with Weighted Sampling** `05_weighted_sampling.Rmd`
  
  * Grades are assigned to each poll, based on aspects like sample size and partisan lean of a pollster. This code attempted to take that 
  into consideration when running the simulation.

The data needed for this project can be found in the folder labeled `Data`

The packages needed to run this repo are as follows:

  * `tidyverse`
  * `SuppDists`
  * `reshape2`
  * `devtools`
  * `ggthemes`
  * `scales`
  * `MASS`
  * `here`
  * `georgia`, which is not part of CRAN but built in this repo, in the script file `06_functions.R` in the `R` folder.
  
The functions that are part of the `georgia` package have help documentation on how they should be ran.

Lastly my presentation slides and project report can be found under the folder `Report and Presentation`. 