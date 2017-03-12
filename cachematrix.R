## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
        mat_inv <- NULL
        set <- function(y) { 
                x <<- y  
                mat_inv <<- NULL 
                }
        get <- function() x
        setInverse <- function(inverse) mat_inv <<- inverse
        getInverse <- function() mat_inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## The cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
        {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getInverse()
        if (!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        mat <- x$get()
        mat_inv <- solve(mat, ...)
        x$setInverse(mat_inv)
        mat_inv
}
