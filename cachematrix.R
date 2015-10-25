## The cachSolve function provides the inverse of a square matrix. It will
## return the inverse of the matrix provided, either the cached version
## if it had already been previously calculated, or will calculate it and
## then return it.


## makeCacheMatrix creates a special "vector", whcih is really a list of
## functions to:
##		1. set the value of the matrix
##		2. get the value of the matrix
##		3. set the value of the inverse of the matrix
##		4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
    set <- function(y) {
    		x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the provided matrix if the inverse
## has not previously been calculated. Otherwise, it returns a cached
## version of the previously calculated inverse of the provided matrix.

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse()
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}
