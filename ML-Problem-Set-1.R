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
set.seed(2323)
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
n <-1000
# x_point = 1
#Loc_avg<-map_dbl( bandwith_values, ~{SimulateLocalAverage(x_point = 1,bandwidth =.x, sample_size = 300) })
map_df(seq(n), SimulateLocalAverage)
results_df_x1 <-map_df(seq_len(n), SimulateLocalAverage)
results_df_x1 <-data.frame()

for(i in 1:n){
  results_df_x1<- rbind(results_df_x1, SimulateLocalAverage(x=1))
  
}
results_df_x1 %>% group_by(bandwidth) %>% summarise(mean=mean(error))


# Visualize
results_df_x1 %>% group_by(bandwidth) %>% summarise(bias = mean(error, na.rm=T)^2, var = var(loc_avg,na.rm = T), mse=bias+var) %>%
    ggplot(aes(x=bandwidth))+geom_line(aes(y=bias), color="blue", size=1.5)+geom_line(aes(y=var), color="green",size=1.5)+geom_line(aes(y=mse), color="red",size=1.5, alpha=0.8)


# x_point = 0.1
results_df_x01 <-data.frame()

for(i in 1:n){
  results_df_x01<- rbind(results_df_x01, SimulateLocalAverage(x=0.1))
  
}

results_df_x01 %>% group_by(bandwidth) %>% summarise(bias=mean(error)^2)


# Visualize
results_df_x01 %>% group_by(bandwidth) %>% summarise(bias = mean(error, na.rm=T)^2, var = var(loc_avg,na.rm = T), mse=bias+var) %>%
  ggplot(aes(x=bandwidth))+geom_line(aes(y=bias), color="blue", size=1.5)+geom_line(aes(y=var), color="green",size=1.5)+geom_line(aes(y=mse), color="red",size=1.5, alpha=0.8)



# E) 



# Problem 2 ---------------------------------------------------------------

####### A)

# LocalAveraging3<-function(sample_x, sample_y, x, h){
#   sum(sample_y * ifelse( norm(as.matrix(sample_x - x), type = "f")<=(h/2),1,0))/ sum(ifelse( norm(as.matrix(sample_x - x), type="f")<=(h/2),1,0))
# }

LocalAveraging2<-function(sample_x, sample_y, x=1, h=seq(1,4,0.5)){
  
  local_avg_estimate <-sum(sample_y * ifelse( map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))<=(h/2),1,0))/ sum(ifelse(map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))<=(h/2),1,0))
  #ifelse(local_avg_estimate=="NaN",0, local_avg_estimate)
}

x=1
# k=5
n <- 1000
k <- 5
h <- seq(1,4,0.5)
sample_x <- matrix(rnorm(n * k, mean = 0, sd=2), nrow = n, ncol = k)
e <- rnorm(n,0,1)
sample_y <- map_dbl(seq_len(n), ~prod(sample_x[.x,])+e[.x])+1

map_dbl(h, ~LocalAveraging2(sample_x = sample_x, sample_y = sample_y, x=1, h=.x) )

# why
hist(2*(map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))))
# check


# k=8
n <- 1000
k <- 8
h <- seq(1,4,0.5)
sample_x <- matrix(rnorm(n * k, mean = 0, sd=2), nrow = n, ncol = k)
e <- rnorm(n,0,1)
sample_y <- map_dbl(seq_len(n), ~prod(sample_x[.x,])+e[.x])+1
map_dbl(h, ~LocalAveraging2(sample_x = sample_x, sample_y = sample_y, x=1, .x) )

hist(2*(map_dbl(seq(nrow(sample_x)), ~norm(as.matrix(sample_x[.x,]- x), type = "f"))))


###### B)

SimulateModel2<-function(n=1000,k=5,h=seq(1,4,0.5)){
  # Generate sample
  sample_x <- matrix(rnorm(n * k, mean = 0, sd=2), nrow = n, ncol = k)
  e <- rnorm(n,0,1)
  sample_y <- map_dbl(seq_len(n), ~prod(sample_x[.x,])+e[.x])+1
  x_eval <- matrix(1, ncol = dim(sample_x)[2])
  #Estimation
  map_df(h, 
         ~{
           tibble(bandwidth = .x,
                  loc_avg =as.numeric(LocalAveraging2(sample_x = sample_x, sample_y = sample_y, x=1, .x)),
                  error = as.numeric((prod(x_eval)+1) - loc_avg)
           ) }
         
         )
  
}

results_model2<-map_df(seq(1000), SimulateModel2 )
results_model2

# % defined
defined <-results_model2 %>% group_by(loc_avg=="NaN") %>% count()
defined[2,2] / sum(defined[,2]) * 100

# bias, sd, RMSE
summary <-results_model2 %>% group_by(bandwidth) %>% summarise(Bias = mean(error, na.rm=T)^2, SD =sd(loc_avg, na.rm = T), RMSE = sqrt(Bias+var(loc_avg, na.rm = T)) )
summary %>% ggplot(aes(x=bandwidth))+geom_line(aes(y=Bias), color="blue")+geom_line(aes(y=SD), color="green")+geom_line(aes(y=RMSE), color="red")
# as % of true value? what is the true value????


######### C)

map_df(seq(1000), SimulateModel2, k=8 ) %>% group_by(loc_avg=="NaN") %>% count()
