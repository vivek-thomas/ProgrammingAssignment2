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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
