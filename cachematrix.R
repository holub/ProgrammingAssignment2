## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Folowing pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The following function calculates the inversed matrix of the special "matix"
## created with the above function. However, it first checks to see if the
## inversed matrix has already been calculated. If so, it gets the inversed
## matrix from the cache and skips the computation. Otherwise, it calculates the
## inversed matrix of the data and sets the value of the inversion in the cache
## via the setsolve function.
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
