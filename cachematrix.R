## Programming Assignment 2: Lexical Scoping
##
## Functions for caching the Inverse of a Matrix, which might be a expensive
## computing operation for big matrices.
##
## Example:
## > m  <- matrix(c(1,2,3,4),2,2)
## > mc <- makeCacheMatrix(m)
## > i  <- cacheSolve(mc)
## > i
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then this function retrieves the inverse from the cache.
##
## extra ... arguments are passed directly to solve
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
