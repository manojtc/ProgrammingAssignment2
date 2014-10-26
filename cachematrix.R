## Coursera R Programming : Programming Assignment 2
## Program to demonstrate caching of a calculated result to save recomputation

## makeCacheMatrix creates a special matrix with an 'inverseMatrix' member
## that serves as a pointer to a cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL

	set <- function(newX) {
		x <<- newX
		## Setting inverseMatrix to 'NULL' effectively 'clears the cache'
		inverseMatrix <<- NULL
	}
	get <- function() { x }

	setInverse <- function(newInverseMatrix) { inverseMatrix <<- newInverseMatrix }
	getInverse <- function() { inverseMatrix }

	## List of getters and setters (to be accessed using $ operator)
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks if an inverse is already present in the cache
## if not, it calculates the inverse using 'solve()' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()

    ## 1. Check if a calculated inverse already exists
	if(!is.null(inverseMatrix)) {
		## 2. Return the existing inverse if it's already cached
		message("getting cached data")
		return(inverseMatrix)
	}

	## 3. Otherwise, calculate the inverse and cache it
	dataMatrix <- x$get()
	inverseMatrix <- solve(dataMatrix, ...)
	x$setInverse(inverseMatrix)
	inverseMatrix
}
