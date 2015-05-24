## Creates matrix cache and declares functions
##_________________________________________________________

makeCacheMatrix <- function(x = matrix()) 
{
    ## Create inverse and set to null for cache check
    inverse <- NULL
    
    ## Sets the matrix "x" to argument "y", which then sets
    ## inverse NULL as it hasn't been calculated yet
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    ## Gets 'special' matrix "x" 
    get <- function() x
    
    ## Sets the matrix "inverse" to the "arg" provided
    setInverse <- function(arg) 
    {
        inverse <<- arg
    }
    
    ## Gets inverse
    getInverse <- function() inverse
    
    ## Lists the data
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
    
}


## Checks whether or not there is cached data:
## If yes: return cached data; Else calculate it
##_________________________________________________________

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    ## Checks for null value, if not null then value is cached
    if(!is.null(inverse))
    {
        message("Getting Cached Data")
        return(inverse) 
    }
    
    ## Gets data from "x"
    matrixToInverse <- x$get()
    
    ## Calculating inverse with solve function
    inverse <- solve(matrixToInverse)
    
    ## Calling "X" function "setInverse" to store inverse
    x$setInverse(inverse)
    
    ## Print/Return inverse
    inverse
    
}