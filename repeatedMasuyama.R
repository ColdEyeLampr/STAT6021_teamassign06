#Repeating Masuyama

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

#read in data
trees = read.csv("trees.csv")

#define a function to calculate basal areas, recursively
basalAreasR <- function(x, y, r){
  inside = sqrt((trees$x-x)^2 + (trees$y-y)^2) < r
  ba = sum(trees$ba[inside])  #sum up basal areas inside of circle and number of trees
  if(overlap.area(x, y, r)<pi*r^2){  #Check if part of the circle is off the edge
    #Define values for a new circle
    a = pi*r^2 - overlap.area(x, y, r)
    r = sqrt(a/pi)
    coords = runif(2, 0, 750)
    ba = ba + basalAreas(coords[1], coords[2], r) #add in the area from a new smaller circle
  }
  return(ba)
}

#define function to run repeated Masuyama, runs n number of times
repeatedMasuyama <- function(n){
  start <- proc.time()
  #generate n random x and y coordinates
  x = runif(n, 0, 750)
  y = runif(n, -37, 787)
  #calculate areas
  ba = sapply(1:n, function(z) basalAreasR(x[z], y[z], 37))
  est = ((750)^2/(pi*37^2))*ba
  #calculate bias and variance
  bias = 100*(mean(est)-311.906)/311.906
  rmse = 100*sd(est)/311.906
  names(bias) = "percentage bias"
  names(rmse) = "percentage RMSE"
  t = proc.time() - start
  return(c(bias, rmse, t[1]))
}

#define a non recursive function to calculate basal areas
basalArea <- function(x, y){
  inside = sqrt((trees$x-x)^2 + (trees$y-y)^2) < 37
  return(sum(trees$ba[inside]))
}

#define a function for Masuyama
masuyama <- function(n){
  start <- proc.time()
  x = runif(n, -37, 784)
  y = runif(n, -37, 784)
  ba = sapply(1:n, function(z) basalArea(x[z], y[z]))
  est = ((750+2*37)^2/(pi*37^2))*ba
  #calculate bias and variance
  bias = 100*(mean(est)-311.906)/311.906
  rmse = 100*sd(est)/311.906
  t = proc.time() - start
  return(c(bias, rmse, t))
}

repeatedMasuyama(10^5)
