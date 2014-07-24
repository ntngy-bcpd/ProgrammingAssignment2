## Calculating the inverse of a matrix and
## caching the inverse for retrieval.

## makeCacheMatrix defines a special matrix, which is a list of functions.
## The inverse of a matrix is calculated by the solve function.
## x - input, a square matrix
## output, a list of functions
##     set    - sets the value of the matrix
##     get    - gets the value of the matrix
##     setinv - sets the inverse of the matrix
##     getinv - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the cached value of a matrix, if available,
## or the calculated one by solve.
## x   - input, a matrix defined by makeCacheMatrix
## inv - output, the inverse of x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
