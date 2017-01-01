aboveN <- function(x, n = 5)
{
  use <- x > n
  x[use]
}

add2 <- function(x,y)
{
  x + y
}

columnMean <- function(y)
{
  nc = ncol(y)
  
  means = numeric(nc)
  
  for (i in 1:nc)
  {
    means[i] = mean(y,i)
  }
  means
}

printTwo <- function(a,b)
{
  print(a)
  print(b)
}

parent <- function(a, b) {
  sum <- a+b
  oldGL <- gl
  gl <- sum
  returned <- function()
  {
    c(a,b, sum, oldGL)
  }
  returned
}

x <- 1
f <- function() x
g <- function() { assign(x,0); f() }
g()

