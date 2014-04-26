## cacheSolve will use the functions in makeCacheMatrix to 
## either calculate the inverse of a matrix and store the 
## result, or call the stored result.

## This is a list of functions meant to be called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	
	## every time makeCacheMatrix is called
	## it should reset its cache
	## therefore arbitrary temp variable m
	## should be set to null
	m <- NULL
	
	## makeCacheMatrix$set sets the value of
	## the input vector x
	## and also should clear the cache (
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## the 'get' function returns the input
	## vector x.  Isn't it more readable with
	## the { } there?
	get <- function() { x }

	## this function allows us to set a callable
	## inverse directly without calculating it
	setinverse <- function(inverse) m <<- inverse
	
	## finally we use the temp variable m,
	## calling it with this function
	getinverse <- function() { m }
	
	## make the whole thing into a list
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve checks if the inverse matrix is valid, and
## if it is, it returns it from memory
## if it isn't, it calculates it and stores it in memory

cacheSolve <- function(x, ...) {
	## load up the tempvar (to see if it has been
	## reset by the altering of the input matrix)
      m <- x$getinverse()
	
	## check if 'getinverse' actually found anything
	## if it did, return it, no calculations
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## if 'getinverse' didn't find anything, 
	## then we load up the input matrix with x$get()
	data <- x$get()
	## and solve for it with 'solve'
	m <- solve(data, ...)
	## then store it for next time!
	x$setinverse(m)
	## and finally return it
	m

}
