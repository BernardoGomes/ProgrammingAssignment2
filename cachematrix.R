## This is the R Programming Course on Coursera
## These functions bellow are supposed to correctly create a matrix and store its inverse value 
## in its cache, in order to only calculate it when necessary. 


makeCacheMatrix <- function (pMatrix = matrix())
{
    ##creates the variable to store the inverse of the matrix created
    inverse_matrix <- NULL
    
    ##function to set the internal value, in case it changes
    set <- function (new_Matrix)
    {
        pMatrix <<- new_Matrix
        ##if the matrix changes, the inverse matrix should be assigned to NULL
        inverse_matrix <<- NULL
    }
    
    ##internal function to return the matrix
    get <- function() pMatrix
    
    ##function called to set the inverse matrix once it is calculated
    setInverse <- function(solve) inverse_matrix <<- solve
    
    ##function called to return the inverse matrix, calculated or not
    getInverse <- function() inverse_matrix
 
    ##list with the internal functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## Write a short comment describing this function

cacheSolve <- function (pMatrix, ...)
{
    ##creates and sets the internal variable to hold the inverse of the matrix
    local_inverse <- pMatrix$getInverse()
    ##test to see if the inverse has already been calculated
    if(!is.null(local_inverse))
    {
        ##if it is not null, return the calculated inverse
        message("inverse matrix has already been calculated")
        return(local_inverse)
    }
    else
    {
        ##if it is null, then calculate the inverse of the matrix and set it to 
        ##its internal cache
        
        local_matrix <- pMatrix$get()                 ##gets the matrix value locally
        local_inverse <- solve(local_matrix)          ##creates and stores locally its inverse
        pMatrix$setInverse(local_inverse)             ##sets the matrix inverse on its internal variable
        return(local_inverse)                         ##returns the inverse
        
    }
}
