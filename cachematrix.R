## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL

	set <- function(newX) {
		x <<- newX
		inverseMatrix <<- NULL
	}
	get <- function() { x }

	setInverse <- function(newInverseMatrix) { inverseMatrix <<- newInverseMatrix }
	getInverse <- function() { inverseMatrix }

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
	if(!is.null(inverseMatrix)) {
		message("getting cached data")
		return(inverseMatrix)
	}
	dataMatrix <- x$get()
	inverseMatrix <- solve(dataMatrix, ...)
	x$setInverse(inverseMatrix)
	inverseMatrix
}
