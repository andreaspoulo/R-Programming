## Programming Assignment 2 Lexical Scoping
## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## There are two functions below. One creates a special "matrix" object and an other
## which computes the inverse of the special "matrix".


##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        inverse <<- NULL
        x <<- y
    }
    get <-  function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setinverse(inv)
    inv
}
