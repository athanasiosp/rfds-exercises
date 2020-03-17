library(tidyverse)


estimate_pi <- function(n){
    
    x = runif(n, 0, 1)
    y = runif(n, 0, 1)
    distance = x**2 + y**2
    
    positions <- ifelse(test = distance<=1, yes = 1, no = 0)
    inside_circle <- length(which(positions == 1))
    
    return(4*inside_circle / n)
}


xp <- vector(mode = "integer", length = 0)
pi_points <- vector(mode = "numeric", length = 0)

# first way of running the simulation with for loop
for (i in seq(100,10000,100)) {
    
    xp <- append(xp, i)
    
    pi_points <- append(pi_points, estimate_pi(i))
    
    data <- tibble(xp, pi_points)
}


# second way of running the simulation with vector operations
xp <- seq(100,10000,100)

pi_points <- sapply(xp, estimate_pi)

# visualizing the output
tibble(xp, pi_points) %>%   

    ggplot(aes(x = xp, y = pi_points)) + 
    geom_point(color = "blue") + 
    geom_line(color = "blue") + 
    geom_hline(aes(yintercept = 3.1415), color = "red")


set.seed(1234)
start_time <- proc.time()
estimate_pi(10000000)
proc.time()-start_time


set.seed(1234)
tictoc::tic("pi")
estimate_pi(10000000)
tictoc::toc()
