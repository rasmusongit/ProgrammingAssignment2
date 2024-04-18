## Put comments here that give an overall description of what your
## functions do

## This function sets up the matrix for which the inverse can be cached.
## It returns a list of functions specific to this matrix for setting, 
## getting and setting and getting the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_x <<- inverse
    getinverse <- function() inverse_x
    list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## This functions checks if the matrix has a cached inverse
## if it does, we return it.
## if it doesnt, we calculate it, cache it for future use, and then we return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        # If inverse_x is not null, then we can just use it
        if (!is.null(inverse_x)) {
            message("Getting cached inverse matrix")
            return(inverse_x)
        }
        # If we reach this, then we did not have a cached version.
        # We must calculate it ourselves.
        matrix <- x$get()
        inverse_x <- solve(matrix)
        # We remember to cache the result before returning it.
        x$setinverse(inverse_x)
        inverse_x
}
