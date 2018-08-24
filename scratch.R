d <- 1 # scaling constant
n <- 1000 # students
p_f <- 0.5 # percent in focal group
th_f_mean <- 0 # mean ability of focal group
n_j <- 20 # number of items
b_offset <- c(rep(2, 10), rep(0, 10))
p <- 0.4 # percent of items with bias

data <- sim_biased_test(d, n, p_f, th_f_mean, n_j, b_offset)

sim_biased_test(d, n, p_f, th_f_mean, n_j, b_offset) %>%
   .$resp %>%
   rasch_all_as_anchors() %>%
   model_output() %>%
   print(n = 30)
