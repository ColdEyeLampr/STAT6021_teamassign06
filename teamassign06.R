# Team Assignment 6
#

# 1. A couple of examples of the proc.time function
"
start <- proc.time()   # A slow example
t <- 0
for (i in 1:1000000) {
  t <- t + i
}
proc.time() - start

start <- proc.time()   # A faster example
t <- sum(as.numeric(1:1000000))
proc.time() - start
"
# 2. A gift to your team
# "overlap.area(xt,yt,rl)" is a function that computes the intersection of
# a disk of radius rl centered at (xt,yt) with the 750-by-750 region that
# contains the stand of trees.
# 
overlap.area <- function(xt,yt,rl) {  
  dx <- min(xt, 750-xt)
  dy <- min(yt, 750-yt)
  if (dx >= rl & dy >= rl) {
    area <- pi*rl^2
  } else {
    if (dx < rl & dy >= rl) {
      if (dx >= 0) {
        area <- (pi - acos(dx/rl))*rl^2 + dx*sqrt(rl^2 - dx^2)
      } else {
        ndx <- -dx
        area <- acos(ndx/rl)*rl^2 - ndx*sqrt(rl^2 - ndx^2)
      }
    }
    if (dx >= rl & dy < rl) {
      if (dy >= 0) {
        area <- (pi - acos(dy/rl))*rl^2 + dy*sqrt(rl^2 - dy^2)
      } else {
        ndy <- -dy
        area <- acos(ndy/rl)*rl^2 - ndy*sqrt(rl^2 - ndy^2)
      }
    }
    if (dx < rl & dy < rl & (dx^2 + dy^2) >= rl^2) {
      if (dx >= 0 & dy >= 0) {
        area <- (pi-acos(dx/rl)-acos(dy/rl))*rl^2 + dx*sqrt(rl^2-dx^2)+dy*sqrt(rl^2-dy^2)
      }
      if (dx >= 0 & dy < 0) {
        ndy <- -dy
        area <- acos(ndy/rl)*rl^2 - ndy*sqrt(rl^2 - ndy^2)
      }
      if (dx < 0 & dy >= 0) {
        ndx <- -dx
        area <- acos(ndx/rl)*rl^2 - ndx*sqrt(rl^2 - ndx^2)
      }
      if (dx < 0 & dy < 0) {
        area <- 0
      }
    }
    if (dx < rl & dy < rl & (dx^2 + dy^2) < rl^2) {
      if (dx >= 0 & dy >= 0) {
        theta <- (3/2)*pi - acos(dx/rl) - acos(dy/rl)
        area <- (theta/2)*rl^2 + 0.5*(dx*sqrt(rl^2-dx^2)+dy*sqrt(rl^2-dy^2)) + dx*dy
      }
      if (dx >= 0 & dy < 0) {
        area1 <- acos(dx/rl)*rl^2 - dx*sqrt(rl^2 - dx^2)
        ndy <- -dy
        theta <- (3/2)*pi - acos(dx/rl) - acos(ndy/rl)
        area2 <- (theta/2)*rl^2 + 0.5*(dx*sqrt(rl^2-dx^2)+ndy*sqrt(rl^2-ndy^2)) + dx*ndy
        area <- pi*rl^2 - (area1 + area2)
      }
      if (dx < 0 & dy >= 0) {
        area1 <- acos(dy/rl)*rl^2 - dy*sqrt(rl^2 - dy^2)
        ndx <- -dx
        theta <- (3/2)*pi - acos(ndx/rl) - acos(dy/rl)
        area2 <- (theta/2)*rl^2 + 0.5*(ndx*sqrt(rl^2-ndx^2)+dy*sqrt(rl^2-dy^2)) + ndx*dy
        area <- pi*rl^2 - (area1 + area2)
      }
      if (dx < 0 & dy < 0) {
        ndx <- -dx
        ndy <- -dy
        theta <- (3/2)*pi + asin(ndx/rl) + asin(ndy/rl)
        area <- pi*rl^2 - ((theta/2)*rl^2 + 0.5*(ndx*sqrt(rl^2-ndx^2)+ndy*sqrt(rl^2-ndy^2)) - ndx*ndy)
      }
    }
  }
  return(area)
}


############################################

# Data Initialization
trees <- read.csv("trees.csv", header = TRUE)

r <- 37
max.coordinate <- 750
min.coordinate <- 0
"
# Part 1
start <- proc.time() 

x <- runif(10^5, min.coordinate-r, max.coordinate+r)
y <- runif(10^5, min.coordinate-r, max.coordinate+r)

t_hat <- rep(0,10^5)
for(i in 1:10^5){
  dist <- sqrt((trees$x-x[i])^2+(trees$y-y[i])^2)
  sample <- trees[dist<=r,]
  t_hat[i] <- (max.coordinate+2*r)^2/(pi*r^2) * sum(sample$ba)
}

t = 311.906
100*(mean(t_hat) - t) / t
100*sqrt(var(t_hat)) / t
proc.time() - start
"
############################################

# Part 2
start <- proc.time() 

x <- runif(10^5, min.coordinate, max.coordinate)
y <- runif(10^5, min.coordinate, max.coordinate)

t_hat <- rep(0,10^5)
for(i in 1:10^5){
  dist <- sqrt((trees$x-x[i])^2+(trees$y-y[i])^2)
  sample <- trees[dist<=r,]
  
  if(nrow(sample)==0){
    t_hat[i] <- 0
  } else {
    pi_i <- rep(0,nrow(sample))
    for(j in 1:nrow(sample)){
      
      if((sample$x[j] <= r) | (sample$x[j] >= max.coordinate-r) | (sample$y[j] <= r) | (sample$y[j] >= max.coordinate-r)){
        pi_i[j] <- overlap.area(sample$x[j], sample$y[j], r)/max.coordinate^2
      } else {
        pi_i[j] <- (pi*r^2)/max.coordinate^2
      }
    }
    
    sample$pi_i <- pi_i
    
    t_hat[i] <- sum(sample$ba/sample$pi_i)
  }

}

t = 311.906
100*(mean(t_hat) - t) / t
100*sqrt(var(t_hat)) / t
proc.time() - start




