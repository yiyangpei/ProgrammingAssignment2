#------------------------------------------------------------------------------#
## This file contains two functions that are used to cache the inverse of a matrix. 
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# makeCacheMatrix: 
#------------------------------------------------------------------------------#
# This function creates a special "matrix" object that can cache its inverse.
# It is a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
#------------------------------------------------------------------------------#
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y){
                x<<-y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(INVERSE) inverse<<- INVERSE
        
        getinverse <- function() inverse
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


#------------------------------------------------------------------------------#
## cacheSolve: 
#------------------------------------------------------------------------------#
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#------------------------------------------------------------------------------#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        INVERSE <- x$getinverse()
        if (!is.null(INVERSE)){
                message("getting cached data")
                return(INVERSE)
        }
        data <- x$get()
        INVERSE <- solve(data, ...)
        x$setinverse(INVERSE)
        INVERSE 
}