makeMatrix <- function(x = matrix())
{
	inverse <- NULL
	
	set <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(y) inverse <<- y
	getInverse <- function() inverse
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...)
{
	print("success 1")
	
	i <- x$getInverse()
	
	print("success 2")
	
	if (!is.null(i))
	{
		print("returning cached value!")
		return (i)
	}
	else
	{
		print("solving...")
		res <- solve(x$get())
		
		x$setInverse(res)
		
		return (x$getInverse())
	}
}