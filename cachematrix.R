## Caching the inverse of a matrix is more efficient than running repeated calculations ie. looping

## The function creates a matrix that can have its inverse cached with a second function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The matrix created by makeCacheMatrix is evaluated by this function to calculate its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("retrieving cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
}
