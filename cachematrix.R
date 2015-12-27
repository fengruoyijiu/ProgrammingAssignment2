## The two functions below are used to create a object that stores a 
## matrix and cache its inverse

## The makeCacheMatrix function creates matrix "x", which contains
## a function to 
## (1) set the value of the matrix, 
## (2) get the value of the matrix, 
## (3) set the value of the inverse of matrix,
## (4) get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
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


## The cacheSolve function computes the inverse of the matrix returned by 
## the function makeCacheMatrix. It firstly check whether a cached inverse
## exist. If so, it returns the cached value; if not, the cacheSolve function
## attempts to solve its inverse and set/return it.

cacheSolve <- function(x = matrix(), ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
