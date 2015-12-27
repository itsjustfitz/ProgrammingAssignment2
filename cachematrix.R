## These two functions create a special object that stores a numeric
## matrix and caches its inverse so that it doesn't need to be
## recalculated each time the inverse is needed.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Computes inverse of object returned by makeCacheMatrix function above.
## If inverse is already in cache, retrieves inverse from cache instead.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    ## Inverse already in cache, so don't recalculate. Retrieve from cache.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## Inverse NOT already in cache, so calculate and put in cache.
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
