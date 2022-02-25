##
##    Machine learning concepts
##
##    Problem set 1


##    Gyongyver Kamenar

library(tidyverse)
# Problem 1 ---------------------------------------------------------------

# A)
LocalAveraging<-function(sample_x, sample_y, x, h){
  sum(sample_y * ifelse( abs(sample_x - x)<=(h/2),1,0)) / sum(ifelse( abs(sample_x - x)<=(h/2),1,0))
}

LocalAveraging0<-function(sample_x, sample_y, x, h){
   sum(sample_y * ifelse( abs(sample_x - x)<=(h/2),0,abs(sample_x - x))) / sum(ifelse( abs(sample_x - x)<=(h/2),0,abs(sample_x - x)))
}

LocalAveraging2<-function(sample_x, sample_y, x, h){
  sum(sample_y * ifelse( norm(as.matrix(sample_x - x), type = "f")<=(h/2),1,0))/ sum(ifelse( norm(as.matrix(sample_x - x), type="f")<=(h/2),1,0))
}

# B)
set.seed(2323)
sample_x <- runif(300, min=0, max = 2)
#set.seed(2323)
e <- rnorm(300)
sample_y <- sample_x^3 - 3.5 * sample_x^2 + 3 * sample_x + e
hist(sample_y)
optimal_y <- sample_x^3 - 3.5 * sample_x^2 + 3 * sample_x


# C)

LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=1, h=1)

# Calc
bandwidth_values <- seq(0.05,2,0.05)

# x=1
map_dbl(bandwidth_values,  ~{ LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=1, h=.x)} )
# x=0.1
map_dbl(bandwidth_values,  ~{ LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=0.1, h=.x)} )

# D)
300*40*1000
# do not set seed
SimulateLocalAverage<-function(x_point=1, bandwidth=seq(0.05,2,0.05), sample_size=300){
  # Generate sample
  sample_x <- runif(sample_size, min=0, max = 2)
  e <- rnorm(sample_size)
  sample_y <- sample_x^3 - 3.5 * sample_x^2 + 3 * sample_x + e
  optimal_y <- sample_x^3 - 3.5 * sample_x^2 + 3 * sample_x
  # Local averaging
  #loc_avg <- LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=x_point, h=bandwidth)
  
  map_df(bandwidth, ~{
    tibble(
    bandwidth = .x,
    loc_avg = as.numeric(LocalAveraging(sample_x = sample_x, sample_y = sample_y, x=x_point, h=.x )),
    error = as.numeric((x_point^3 - 3.5 * x_point^2 + 3 * x_point) - loc_avg)
    )
  })
}

# Simulate 1000 times
n <-3
# x_point = 1
#Loc_avg<-map_dbl( bandwith_values, ~{SimulateLocalAverage(x_point = 1,bandwidth =.x, sample_size = 300) })
map_df(seq(n), SimulateLocalAverage)
results_df_x1 <-map_df(seq_len(n), SimulateLocalAverage)
results_df_x1 %>% group_by(bandwidth) %>% summarise(mean=mean(as.numeric(error)))
data.frame(results_df_x1)
results_df_x1 %>% group_by(bandwidth) %>% summarise(bias = mean(error, na.rm=T)^2, var = var(loc_avg,na.rm = T), mse=bias+var) %>%
    ggplot(aes(x=bandwidth))+geom_line(aes(y=bias), color="blue")+geom_line(aes(y=var), color="green")+geom_line(aes(y=mse), color="red")


# x_point = 0.1
results_df_x01 <-map_dfc(seq_len(n), ~{
  loc_avg<-map_dbl( bandwith_values, ~{SimulateLocalAverage(x_point =0.1,bandwidth =.x, sample_size = 300) })
  tibble(simulation = loc_avg)
} )


results_df_x01<-cbind(bandwith_values,results_df_x01)


# E) 



# Problem 2 ---------------------------------------------------------------

####### A)

LocalAveraging2<-function(sample_x, sample_y, x, h){
  sum(sample_y * ifelse( norm(as.matrix(sample_x - x), type = "f")<=(h/2),1,0))/ sum(ifelse( norm(as.matrix(sample_x - x), type="f")<=(h/2),1,0))
}

LocalAveraging3<-function(sample_x, sample_y, x=1, h){
  sum(sample_y * ifelse( map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))<=(h/2),1,0))/ sum(ifelse(map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))<=(h/2),1,0))
}
x=1
# k=5
n <- 1000
k <- 5
h <- seq(1,4,0.5)
sample_x <- matrix(rnorm(n * k, mean = 0, sd=2), nrow = n, ncol = k)
e <- rnorm(n,0,1)
sample_y <- map_dbl(seq_len(n), ~prod(sample_x[.x,])+e[.x])
map_dbl(h, ~LocalAveraging3(sample_x = sample_x, sample_y = sample_y, x=1, h=.x) )
# why
hist(2*(map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))))
# check


# k=8
n <- 1000
k <- 8
h <- seq(1,10,0.5)
sample_x <- matrix(rnorm(n * k, mean = 0, sd=2), nrow = n, ncol = k)
e <- rnorm(n,0,1)
sample_y <- map_dbl(seq_len(n), ~prod(sample_x[.x,])+e[.x])
map_dbl(h, ~LocalAveraging3(sample_x = sample_x, sample_y = sample_y, x=1, .x) )


###### B)








  