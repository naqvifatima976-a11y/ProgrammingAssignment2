## These functions create a special "matrix" object that can store its inverse.
## The goal is to avoid recalculating the inverse repeatedly, which can be costly.

## makeCacheMatrix: builds a list of helper functions that
## - set and get the matrix
## - set and get the cached inverse (if it exists)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize cache
    
    # store a new matrix and clear any cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # return the current matrix
    get <- function() x
    
    # store the inverse in the cache
    setInverse <- function(inverse) inv <<- inverse
    
    # return the cached inverse
    getInverse <- function() inv
    
    # expose the helper functions as a list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: returns the inverse of the special matrix created by makeCacheMatrix.
## If the inverse is already cached, it retrieves it instead of recomputing.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # if a cached inverse exists, use it
    if(!is.null(inv)) {
        message("using cached inverse")
        return(inv)
    }
    
    # otherwise, calculate the inverse and store it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
