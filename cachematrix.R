## This file contains two functions that allow caching the result of 
## a matrix inverse operation as an optimization, since matrix inversion
## can be an expensive operation for large matrices.
##
## Matrices that cache their inverses should be constructed using
## the special "makeCacheMatrix" method. Retrieving (and caching) matrix
## inverse can then be done by calling "cacheSolve" with an argument being
## the special matrix object created using "makeCacheMatrix". Here's an
## an example that illustrates usage:
##
##      > matrixToInvert = cbind(c(1,2),c(3,4));
##      > specialCacheMatrix = makeCacheMatrix(matrixToInvert);
##      > inverse = cacheSolve(specialCacheMatrix);
##      > inverse
##      [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > inverse = cacheSolve(specialCacheMatrix); # expect cached value
##      getting cached inverse
##      > inverse
##      [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
        inv <- NULL
        set <- function(y) {
                x <<- y
                ## Reset the inverse since we are now tracking a new matrix
                ## that could be different than the one that was previously
                ## tracked.
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## First, check if we have a cached inverse
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## No cached inverse found. Will read the matrix first, 
        ## compute the inverse and store it in the cache for potential
        ## subsequent calls.
        matrix <- x$get()
        inv <- solve(matrix)
        matrix <- x$setinverse(inv)
        ## Return the inverse
        inv
}
