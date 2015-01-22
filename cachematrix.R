## smenass
## R Programming Coursera Course 
## Programming Assignment 2

################################################################################

## This file contains a suite of functions which allow a user to calculate the 
## inverse of a matrix and cache its value for later use. 
## Computing the inverse of a matrix can be computationally intensive, so  
## caching the value can improve performance if the matrix inverse is needed 
## later, and the matrix doesn't change.

## To use these functions {variable names are suggestions and need not be used}:
## 1. Create the matrix, X,  you wish to invert in R and assign it to a variable
## 2. Run myMatrix <- makeCacheMatrix(X) 
## 3. Run myInverse <- cacheSolve(myMatrix) anytime you need the inverse of 
##    matrix X in your program

################################################################################

## makeCacheMatrix
## Inputs : x = matrix to be inverted
## Outputs : special matrix 'object' (i.e., list) containing functions for use 
##           in computing inverse with cacheSolve:
##           Functions:
##              1) 


makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInv <- function(inverse) inv <<- inverse
        
        getInv <- function() inv
        
        # Create the 'special matrix object' which contains a list of functions
        list(set = set, 
             get = get, 
             setInv = setInv,
             getInv = getInv)

}


################################################################################

## cacheSolve(x)
## Inputs : x -- special matrix object of type created by makeCacheMatrix 
##                (i.e., you must first run makeCacheMatrix on the matrix you 
##                wish to invert)
## Outputs : the inverse of matrix from which the special matrix x was made


cacheSolve <- function(x) {
        
        # Check the cache for precalculated inverse and return if found
        
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) # Inverse was already calculated, we are done!
        }
        
        # Otherwise, compute, store and return the inverse
        
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
        
}
