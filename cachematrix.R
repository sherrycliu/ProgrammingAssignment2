## The following functions get the inverse of a matrix 
## either thru calling solve() function and cache the inverse 
## or fetch the inverse directly from cache if the inverse has been cached.

## creates a special "matrix", which is really a list containing a function to
## set the values of the matrix
## get the values of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Computing the inverse of the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
