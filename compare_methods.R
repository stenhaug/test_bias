# get started -------------------------------------------------------------
library(tidyverse)
library(MASS)
library(difR)
library(mirt); mirtCluster()
library(rstan); rstan_options(auto_write = TRUE); options(mc.cores = parallel::detectCores())

# specifications ----------------------------------------------------------
nitems <- 30
runs_per_p <- 1 # low for testing

y <- mvrnorm(4000, mu = c(0, 0), rethinking::rlkjcorr(1 , 2, eta = 2)) # this is a bit weird

compare %>% write_rds("~/Desktop/compare.rds")
compare <- read_rds("~/Desktop/compare.rds")

# create data -------------------------------------------------------------
compare_methods <-
   crossing(
      d = 1,
      n = 1000,
      p_f = 0.5,
      th_f_mean = -1,
      n_j = nitems,
      p = seq(0, 0.6, 0.2),
   ) %>%
   slice(rep(1:nrow(.), run_per_p)) %>% # controls runs
   mutate(
      b_offset = map2(p, n_j, b_offset_mixture),
      data = list(d, n, p_f, th_f_mean, n_j, b_offset) %>% pmap(sim_biased_test),
      fit1 = map(data, ~stan_fit(.$resp)), # first fit
      fit_bf1 = fit1 %>% map(bayes_factor_df, items = 1:nitems),
      anchors_unrefined = fit_bf1 %>% map(get_anchors, threshold = 5), # note setting threshold
      gap_dtf_unrefined =
         map2(map(data, "resp"), anchors_unrefined, rasch_with_anchors) %>% map_dbl(~ get_achieve_gap(model_output(.))),
      resp_refined = map2(map(data, "resp"), fit_bf1, just_invariant_items, threshold = 2)
   )

c5 <- c4 %>% filter(!is.na(data_refined)) # get rid of rows where we removed all responses
c6 <- c5 %>% dplyr::select(-fit1) # remove fit
c7 <- c6 %>% mutate(gap_dif_unrefined = data_refined %>% map_dbl(get_achieve_gap_no_anchors))
c8 <- c7 %>% mutate(fit2 = data_refined %>% map(~ stan_fit(.$resp))) # now create second fit object with purified responses
c9 <- c8 %>% mutate(items = data_refined %>% map(get_items_data))
c10 <- c9 %>% mutate(fitbf2 = map2(fit2, items, get_stan_dif2)) # estimate bayes factor for each
c11 <- c10 %>% mutate(anchors = map2(fitbf2, 3 * threshold, get_anchors)) # use the threshold to choose the anchors
c12 <- c11 %>% mutate(gap_dtf_refined = map2_dbl(data, anchors, get_achieve_gap_use_anchors)) # use the anchors on the original data to estimate a gap
c13 <- c12 %>% dplyr::select(-fit2)
c13 %>% write_rds("/scratch/users/stenhaug/a1_compare_gaps.rds")

get_items_data <- function(data){
   names(data$resp)[-1] %>% str_replace("X", "") %>% as.numeric()
}
