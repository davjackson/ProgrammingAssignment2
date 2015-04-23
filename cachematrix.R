## These functions take a matrix, and apply the solve function,
## to create an inverse of the matrix.  If that inverse is created
## already, then the function returns the cached value instead.

## This function preforms the original solve function to create
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)     
}


## This function determines whether the matrix has already been
## "solved" into the inverse, and returns the prepared solution,
## and if not, then calculates the inverse instead.  This can save
## valuable processor time in large matrices.

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
