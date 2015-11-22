## Calculating the inverse of a matrix may be time-consuming if the matrix is 
## large and/or if it has to be calculated repeatedly (as in a loop). The pair 
## of functions below allow you to cache the inverse of a matrix and retrieve it
## later if the matrix has not changed.

## The first function, makeCacheMatrix, takes a square matrix as its input, 
## stores the matrix in the background as x, and returns a special "matrix," 
## which is actally a list of four functions that can be used on x. The 
## functions are: 1) set() resets the value of x and erases the cached inverse, 
## 2) get() returns the value of x, 3) setinverse() takes the inverse of x as 
## its input and stores it as inv, 4) getcache() returns the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        try(if(!is.matrix(x) | nrow(as.matrix(x)) != ncol(as.matrix(x)))
                message("Warning: Input must be a square matrix to be invertible.")
        )
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, takes as its input the special "matrix" that
## resulted from makeCacheMatrix(x) and retrieves the inverse of x from the 
## cache. If the cache is empty, cacheSolve calculates the inverse, stores it in
## the cache, and returns the result.

cacheSolve <- function(x2, ...) {
        i <- x2$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x2$get()
        i <- solve(data, ...)
        x2$setinverse(i)
        i
}