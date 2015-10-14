# Team Assignment 6
#

# 1. A couple of examples of the proc.time function

start <- proc.time()   # A slow example
t <- 0
for (i in 1:1000000) {
  t <- t + i
}
proc.time() - start

start <- proc.time()   # A faster example
t <- sum(as.numeric(1:1000000))
proc.time() - start

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