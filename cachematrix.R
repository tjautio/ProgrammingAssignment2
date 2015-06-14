## The functions can be used to calculate inverse of a matrix and storing it in
## cache if it's needed e.g. in multiple future computations.

## makeCacheMatrix() first erases all the previous values from cache for one particular
## matrix and then takes a matrix-object and creates a list-object 
## including 4 functions:set, get, setinverse, getinverse. Set() stores
## the matrix, setinverse() stores the inverse, get() and getinverse()
## prints the stored matrices.
## cacheSolve function uses these functions to store or get the cached
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv 
    getinverse <- function() inverse
    
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve() takes the output from makeCacheMatrix function. It first checks 
## if the inverse has been already computed. In that case the function prints a message 
## "using cached inverse" and prints the inverse in the cache.
## If the inverse hasn't been stored the function first computes it
## stores it in the cache and prints the inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)){
        message("using cached inverse")
        return(inverse)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}