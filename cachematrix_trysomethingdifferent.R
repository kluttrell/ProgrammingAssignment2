## These functions check whether the inverse of a matrix has been computed
## and cached.  If so, they return the cached inverse matrix.  If not, they
## compute the inverse matrix, cache it, and return it.

## This function creates a matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## solve x using the solve() function
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        ## set x$setinverse to the solve(i)
        setinverse <- function(solve) i <<- solve
        ## return inverse as x$getinverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will
## retrieve the inverse from the cache. 

cacheSolve <- function(x) {
        ## attempt to retrieve the inverse of x
        i <- x$getinverse()
        ## if the value retreived for the inverse of x is not null,
        ## return message and the inverse of x
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ## leave function
        }
        ## if we made it to this stage, then the value retreived for the 
        ## inverse of x is null so, we need to populate it
        ## data <- x$get()
        ## i <- solve(x$get())
        x$setinverse(solve(x$get()))
        solve(x$get())
}
