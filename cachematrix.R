## Below are two functions that are used:
## 	1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##	2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##	   If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse, containing a function to:
##	1) set the value of the matrix
##	2) get the value of the matrix
##	3) set the value of the solve
##	4) get the value of the solve
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## The cacheSolve function calculates the inverse, via solve, of the special "matrix" object created with the above function. 
## However, it first checks to see if its inverse has already been calculated. 
## If so, it gets the result of the solve function from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the solve in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setsolve(m)
	m        
}
