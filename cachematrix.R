## enable creation of a matrix like object with the ability to cache the inversion function
## usage:
## - m <- makeCacheMatrix( matrix( 1:4, nrow=2, ncol=2 ) )
## - cacheSolve( m )

## create a matrix that can be passed to cacheSolve, which solves and caches the inversion function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## accepts as a parameter the object created by makeCacheMatrix
## solves the matrix and caches the result; returns this result if called again
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
