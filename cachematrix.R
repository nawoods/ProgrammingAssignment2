## Gives the ability to cache the inverse of a matrix, saving us
## from having to preform this costly computation multiple times

## Creates a data structure allowing a matrix's inverse to be stored
## alongside it

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInv <- function(z) inv <<- z
    getInv <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInv = setInv, getInv = getInv)
}


## Computes the inverse of a matrix, returning the cached value instead if it
## has already been computed

cacheSolve <- function(x, ...) {
    ## if cached inverse already exists, return it
    s <- x$getInv()
    if(!is.null(s)) {
        message("getting cached data...")
        return(s)
    }
    
    ## otherwise, compute the inverse and store it in the cache
    data <- x$getMatrix()
    s <- solve(data, ...)
    x$setInv(s)
    s
}
