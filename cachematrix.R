## makeCacheMatrix() initializes the matrix to be called and used to calculate the 
## inverse matrix, which is initialized as NULL. cacheSolve() takes the initialized matrix from the cache and uses this to calculate the inverse matrix. 

## This function initializes the objects x and inv, setting an empty matrix and 
## inv, which is the inverse matrix once computed and retrieved. When executed,
## makeCacheMatrix will first initialize, then retrieve x from the parent environment
## using set(). Using get(), it will retrieve x from the parent environment after
## it is returned from CacheSolve(). setinverse() and getinverse() similarly set
## and retrieve the inverse functions from the Parent Environment. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  
        set <- function(Neo) {
                x <<- Neo
                inv <<- NULL
        }         
        get <- function() x
        setinverse <- function(invert) inv <<- invert
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}        
        
## cacheSolve() takes the NULL initialized matrix from makeCacheMatrix and uses 
## the matrix x to calculate the inverse matrix using the solve() function. If the 
## input matrix inv is NULL, it will print a message and then return the inverse 
## function to the cache, replacing the NULL inv as the calculated inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting the cached data and a cup of Tea")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}