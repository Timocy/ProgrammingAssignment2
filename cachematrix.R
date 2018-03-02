## Put comments here that give an overall description of what your
## functions do

## Create a list of functions for a given matrix.
## set() : Save a base matrix.
## get() : Load the base matrix. NULL if none saved.
## set_inverse() : Save the given matrix as inverse --> Cached
## get_inverse() : Load the cached inverse matrix.

makeCacheMatrix <- function(m = matrix()) {
        inverse_m <- NULL
        
        set <- function(matrix_object) {
                m <<- matrix_object
                inverse_m <<- NULL
        }
        
        get <- function() m
        
        set_inverse <- function(inversed) inverse_m <<- inversed
        
        get_inverse <- function() inverse_m
        
        list (set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Get a makeCacheMatrix object as argument and return the inverse of the base matrix of it.
## If the given makeCacheMatrix object already has cached inverse of the base matrix, return the cached result.
## Else, create the inverse newly and cache it in makeCacheMatrix object and return it.

cacheSolve <- function(mCM, ...) {
        inverse <- mCM$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        else {
                data <- mCM$get()
                inverse <- solve(data, ...)
                mCM$set_inverse(inverse)
                return(inverse)
        }
}
