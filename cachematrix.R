## The pair of functions below is used to cache the inverse of a matrix 
## in order to not to have to recompute it by the costly computation 
## matrix inversion. 

## The function makeCacheMatrix creates a special "matrix" which is really a 
## list containing four functions (to 1. set the value of the matrix, 
## 2. get the value of the matrix, 3. set the value of the inverse matrix, 
## 4. get the value of the inverse matrix). 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the special "matrix" 
## created with the above makeCacheMatrix function. It first checks if the
## inverse has already been calculated. If so, it gets the inverse matrix from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
