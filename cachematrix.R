
## These functions aim to cache potentially time-consuming computations
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
# cacheSolve function computes the inverse of the matrix. If the inverse has 
# already been calculated, it retrieves the cache result and skips the computation. 
# Otherwise, it calculates the inverse and sets the value in the cache via
# setinverse function.
# We assume that the matrix is always invertible.
cacheSolve <- function(x, ...) {
 inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
