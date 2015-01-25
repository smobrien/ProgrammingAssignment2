## A set of functions to get/set a matrix and its inverse,
## along with a function to compute the inverse and store it in a cache

## makeCacheMatrix: Returns a list of which can be used to get/set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # Set the initial value of inverse to nulla

    # set method, used to set the current matrix
    set <- function(y)
    {
        x <<- y # Set x in parent environment to value y
        inverse <<- NULL # Set inverse in parent environment to NULL
        # Set to NULL since any value already stored for inverse is now incorrect
    }

    # get method, used to get the current matrix
    get <- function() x

    # setInverse method, sets the value of the inverse
    setInverse <- function(i)
    {
        inverse <<- i # Set inverse in parent environment to inverse value provided
    }

    # getInverse method, returns the stored value of the inverse
    getInverse <- function() inverse

    # store functions to list as return value
    list (set = set, get = get,
          setInverse = setInverse, getInverse = getInverse)

}

## Get the inverse from the provided matrix function, if it
## does not exists, the compute it and store for later use
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse() # Get the inverse of the provided matrix function

    if (!is.null(inverse))
    {
        # Matrix inverse has already been computed, return cached value
        message("getting cached data")
        return(inverse)
    }

    # Matrix inverse has not be computed, calculate
    inverse <- solve(x$get())
    x$setInverse(inverse) # Store in cache
    inverse # Returns calculated value
}
