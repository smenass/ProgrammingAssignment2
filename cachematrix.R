## smenass
## R Programming Coursera Course 
## Programming Assignment 2

################################################################################
##
## Computing the inverse of a matrix can be computationally intensive, so  
## caching the value can improve performance if the matrix inverse is needed 
## later, and the matrix doesn't change.
##
## This file contains a suite of functions which allow a user to calculate the 
## inverse of a matrix and cache its value for later use. These functions draw 
## from and modify examples provided by course professors in the original 
## README.md file.
##
## To use these functions {variable names are suggestions and need not be used}:
## 1. Define your matrix, X
## 2. Run myMatrix <- makeCacheMatrix(X) 
## 3. Run myInverse <- cacheSolve(myMatrix) when you need the inverse of X
##
################################################################################
##
## makeCacheMatrix - this function creates a special object from a matrix to be
## inverted. Use of this function and resulting object allows the inverse of 
## matrix to be cached. 
##
## Inputs : x -- matrix to be inverted 
##              Note: these functions do not provide data validation to ensure
##              matrix is invertible.
## Outputs : special matrix 'object' (i.e., list) containing functions for use 
##           in computing and caching inverse with cacheSolve.
##           Functions:
##              1) set : stores the value of the matrix (in cache)
##              2) get : retrieves the value of the matrix 
##              3) setInv : stores the value of the inverse in cache (to be 
##                          called after calculation)
##              4) getInv : retrieves the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {

        # Define functions to store and retrieve the matrix and its inverse
        
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInv <- function(inverse) inv <<- inverse
        
        getInv <- function() inv
        
        # Create the 'special matrix object', i.e., a list of functions
        
        list(set = set, 
             get = get, 
             setInv = setInv,
             getInv = getInv)

}

################################################################################
##
## cacheSolve(x) - this function computes and caches the inverse of a matrix, 
##                 first checking the cache to see if the inverse was already 
##                 computed, and confirming that the matrix has not changed.
##
## Inputs : x -- special matrix object created by makeCacheMatrix 
##              Note: these functions do not provide data validation to ensure
##              matrix is invertible.
## Outputs : the inverse of matrix from which the special matrix x was made


cacheSolve <- function(x) {
        
        # Check the cache for precalculated inverse and return if found
        
        inv <- x$getInv()
        if(!is.null(inv)) {
                return(inv) # Inverse was already calculated, we are done!
        }
        
        # Otherwise, compute, store and return the inverse
        
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
        
}
