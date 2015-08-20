## makeCacheMatrix - Caches a matrix
## Input   - a matrix defaults to a [1,1] empty matrix
## Returns - a 4 element list containing references to helper functions:
##           set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	set <- function(y) {
		x       <<- y
		inverse <<- NULL
	}
	get <- function() x	
	setInverse <- function(inverse) inverse <<- inverse
	getInverse <- function() inverse

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve - gets the inverse of a matrix also calcuates and caches
## the inverse in case of cache-miss. Assumes matrix is always invertible
## Input   - list returned by makeCacheMatrix
## Returns - Inverse of matrix
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()

	if(!is.null(inverse)) {
		message("fetching cached data")
		return(inverse)
	}

	matData <- x$get()
	inverse <- solve(matData)
	x$setInverse(inverse)
	inverse
}
