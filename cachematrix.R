## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix() is a function that takes a matrix as its parameter and returns
## a List of functions for manipulating the matrix and its cached inversed

## It has 4 functions to set, get the value of the matrix, and set get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) i  <<- inverse
        getinverse  <- function() i
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}




## cacheSolve() takes a matrix cache  and
## returns either a cached inverse or a newly calculated one

##It first checks if the inverse had already been calculated. 
##If so,the cachesolve() should retrieve the inverse from the cache and skips the calculation.
## Otherwise the inverse is calculated and set into the cache.


cacheSolve <- function(x, ...) {
        i  <- x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinverse(i)
        i
}
        ## Return a matrix that is the inverse of 'x'

