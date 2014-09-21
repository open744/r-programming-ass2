## this pair of functions cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- x		# stores the matrix data
    inv <- NULL		# caches the inverse

    ## the 4 functions are set, get, setinv, getinv

    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    
    get <- function() m
    
    setinv <- function(i) inv <<- i
    
    getinv <- function() inv

    ## return a list of the 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## check that x is indeed a 'special matrix', abort if not
    if (!'getinv' %in% names(x))
        stop("that's not a special matrix (use 'makeCacheMatrix')")

    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    ## cache was NULL, so spend time computing an inverse
    d <- x$get()
    i <- solve(d, ...)
    x$setinv(i)		# memorise it inside the special matrix
    i
}
