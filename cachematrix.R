## This file includes a pair of functions that cache the inverse of a matrix.
## Example of use:
##
## > test_matrix <- matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3)
## > inverse_matrix <- makeCacheMatrix(test_matrix)
## > solved_matrix <- cacheSolve(inverse_matrix)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setinverse(m)
        m
}
