## makeCacheMatrix function creates special "matrix" object that can cache its inverse
## cacheSolve function computes the inverse of special "matrix" returned by makeCacheMatrix function. If the inverse has already been calculated and matrix has not changed, then cacheSolve should retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
