##
##    Machine learning concepts
##
##    Problem set 1


##    Gyongyver Kamenar

library(tidyverse)
# Problem 1 ---------------------------------------------------------------

# A)

LocalAveraging<-function(sample_x, sample_y, x, h){
   sum(sample_y * ifelse( abs(sample_x - x)<=(h/2),abs(sample_x - x),0)) / sum(ifelse( abs(sample_x - x)<=(h/2),abs(sample_x - x),0))
}

# B)
set.seed(2323)
sample_x <- runif(300, min=0, max = 2)
set.seed(2323)
e <- rnorm(300)
sample_y <- sample_x^3 - 3.5 * sample_x^2 + 3 * sample_x + e
hist(sample_y)

# C)
LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=1, h=1)
# Calc
bandwith_values <- seq(0.05,2,0.01)

# x=1
map_dbl(bandwith_values,  ~{ LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=1, h=.x)} )
# x=0.1
map_dbl(bandwith_values,  ~{ LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=0.1, h=.x)} )

# D)

# do not set seed
SimulateLocalAverage<-function(x_point, bandwidth, sample_size){
  # Generate sample
  sample_x <- runif(sample_size, min=0, max = 2)
  e <- rnorm(sample_size)
  sample_y <- sample_x^3 - 3.5 * sample_x^2 + 3 * sample_x + e
  # Local averaging
  LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=x_point, h=bandwidth)
}

# Simulate 1000 times
n <-1000
#Loc_avg<-map_dbl( bandwith_values, ~{SimulateLocalAverage(x_point = 1,bandwidth =.x, sample_size = 300) })

results_df_x1 <-map_dfc(seq_len(n), ~{
  loc_avg<-map_dbl( bandwith_values, ~{SimulateLocalAverage(x_point = 1,bandwidth =.x, sample_size = 300) })
  tibble(simulation = loc_avg)
} )
results_df_x1<-cbind(bandwith_values, results_df_x1)

results_df_x01 <-map_dfc(seq_len(n), ~{
  loc_avg<-map_dbl( bandwith_values, ~{SimulateLocalAverage(x_point =0.1,bandwidth =.x, sample_size = 300) })
  tibble(simulation = loc_avg)
} )

results_df_x01<-cbind(bandwith_values,results_df_x01)


# E) 



# Problem 2 ---------------------------------------------------------------


