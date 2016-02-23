## This file contains two methods


## 1. makeCacheMatrix: This function caches the matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2. cacheSolve: Checks for caching of the solve/inverse of passed matrix. 
##      If cached returns the same, else calculate the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getMatrix()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
