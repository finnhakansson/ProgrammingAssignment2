##
## This pair of functions computes the inverse of a matrix and caches the result.
##


## This function creates a list of functions (or methods) for a matrix and its inverse.
makeCacheMatrix <- function(M = matrix()) {
    A_inv <- NULL
    set <- function(M) {
        A <<- M
        A_inv <<- NULL
    }
    get <- function() A
    set_inverse <- function(S) A_inv <<- S
    get_inverse <- function() A_inv
    list(
        set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


## This function operates on a matrix/inverse object that's created
## by makeCacheMatrix().
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    A_inv <- x$get_inverse()
    if (!is.null(A_inv)) {
        message("Getting cached data...")
        return(A_inv)
    }
    A <- x$get()
    A_inv <- solve(A)
    x$set_inverse(A_inv)
    A_inv
}


