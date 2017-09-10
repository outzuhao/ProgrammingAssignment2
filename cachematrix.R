## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a list containing four functions:
## 1. set(): Set a matrix
## 2. get(): Get the matrix
## 3. setinv(): Set the inversed matrix
## 4. getinv(): Get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## This function returns the inversed matrix. It first checks if the inversion
## has been solved. If so, it gets the inversed matrix from the cache and skips
## the computation. Otherwise, it solves the inversed matrix and sets it to the 
## cache by setinv() function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
