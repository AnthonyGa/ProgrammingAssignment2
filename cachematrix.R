## The two functions below are used to calculate an inversed matrix, ...
## ... to store it in the cache and return the cached value when called again, ...
## ... in order to avoir depeating time consuming calculations.


## makeCacheMatrix takes for argument a particular matrix x (if x specified when calling the function), ...
## ... otherwise x is an empty matrix object, to be specified later (setMatrix):
## This function returns a list of functions useful to calculate and cache the inversed matrix:

makeCacheMatrix <- function(x = matrix()) { 
        ## creates and initialises cache:
        inv <- NULL 
        ## this function assigns matrix object y to x / initialises inv in parent environment: 
        ## can be used to set new value to matrix x:
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## this function returns matrix x:
        getMatrix <- function() x
        ## in this function, cache inv gets the inversed matrix:
        setInverse <- function(inverse) inv <<- inverse
        ## this function returns the inversed matrix from the cache:
        getInverse <- function() inv
        ## creates a list of the four above functions:
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the inversed matrix, from either the cache or via calculation:
## When run for the first time, inversed matrix calculated and stored in cache:
## Afterwards, if matrix object isn't changed using setMatrix, ...
## ... the function returns directly the inversed matrix from the cache:
## note: x now a list, different from x above (was matrix):

cacheSolve <- function(x, ...) {
        ## inv gets the content of the cache: 
        inv <- x$getInverse()
        ## if the cache is not empty, return inversed matrix stored in cache:
        ## also skips the end of the code:
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if the cache is empty, calculate the inverse of the matrix:
        data <- x$getMatrix()
        inv <- solve(data, ...)
        ## stores the inversed matrix in the cache:
        x$setInverse(inv)
        ## returns inversed matrix:
        inv
}