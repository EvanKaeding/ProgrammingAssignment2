## This set of funcitons helps resolve and cache the inverse of an invertible matrix

## This function creates a set of functions that can be called to store and resolve the inverse
## of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {

        n <- NULL
        
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) n <<- solve
        
        getinverse <- function() n
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function reads the list object returned by `makeCacheMatrix` and returns the cached inverse
## of the matrix, if it exists.

cacheSolve <- function(x, ...) {

        n <- x$getinverse()
        
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        
        data <- x$get()
        
        n <- solve(data, ...)
        
        x$setinverse(n)
        
        n
        }
