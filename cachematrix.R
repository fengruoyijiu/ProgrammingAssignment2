## The two functions below are used to create a object that stores a 
## matrix and cache its inverse

## A example to use these two functions is:
##      > q <- makeCacheMatrix(matrix(1:4, 2, 2))
##      > cacheSolve(q)
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > cacheSolve(q)
##      getting cached data
##             [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5

## The makeCacheMatrix function takes a matrix object as argument and
## creates a list which contains a function to 
## (1) set the value of the matrix 
## (2) get the value of the matrix 
## (3) set the value of the inverse of matrix
## (4) get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ### (1) set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ### (2) get the value of the matrix
    get <- function() x
    
    ### (3) set the value of the inverse of matrix
    setinverse <- function(inverse) i <<- inverse 
    
    ### (4) get the value of the inverse of matrix
    getinverse <- function() i
    
    ### return the list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function also takes a matrix as argument. Its additional arguments are
## acquired from the the solve function by the "dot dot dot argument". The the cacheSolve 
## function computes the inverse of the matrix returned by the function makeCacheMatrix. 
## It firstly check whether a cached inverse exist. If so, it returns the cached value; 
## if not, the cacheSolve function attempts to solve the inverse and set/return it.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() ### retrive data from the cache
    
    if(!is.null(i)) {
    ### if the value is not NULL ...
        message("getting cached data")
        return(i)
    }
    
    ### if the value is NULL, then compute the inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
