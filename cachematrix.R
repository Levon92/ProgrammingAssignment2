# cachematrix.R
# Coursera: R Programming - Programming Assignment 2
# Assignment: Caching the Inverse of a Matrix
#
# This file defines two functions:
#   1) makeCacheMatrix: creates a special "matrix" object that can cache its inverse
#   2) cacheSolve: computes (or retrieves) the cached inverse of the special "matrix"
#
# Notes:
# - The matrix is assumed to be square and invertible.
# - Do not rename functions; Coursera grader expects these signatures.
#
# Example usage:
#   m <- matrix(c(1, 2, 3, 4), 2, 2)
#   cm <- makeCacheMatrix(m)
#   cacheSolve(cm)      # computes inverse
#   cacheSolve(cm)      # returns cached inverse (prints "getting cached data")
#
# -----------------------------------------------------------------------------

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL     # reset cache when matrix changes
    }

    get <- function() x

    setinverse <- function(inverse) inv <<- inverse

    getinverse <- function() inv

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)   # compute inverse
    x$setinverse(inv)         # cache the result
    inv
}
