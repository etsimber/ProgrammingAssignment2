## The functions created here work together to allow the user to cache the 
## inverse of the matrix for later reuse.  The first function creates a special 
## matrix which can store the matrix itself as well as its calculated inverse.
## It basically returns a list of getters and setters for the matrix and the inverse
## the second function uses the result of the first to either lookup the inverse in 
## the cache or calculate it and store it in the cache if it is not available


## This function creates a special "matrix", which is really a list containing 
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## preset the inverse to NULL
    inverse <- NULL
    ## setter for the original matrix.  This should reset the inverse to NULL
    set <- function(y){
        x<<-y
        ## reset the inverse to NULL to make sure we dont't use it if the matrix
        ## changes
        inverse<<-NULL
    }
    ## getter for the original matrix
    get <- function() x
    ## setter for the inverse
    setinverse <- function(calculatedInverse){
        inverse<<-calculatedInverse
    }
    ## getter for the inverse
    getinverse <- function() inverse
    list(set=set, get=get, 
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function calculates the inverse of the special "matrix" created by
## makeCacheMatrix.  It first checks to see if the inverse has already been 
## calculated.  If so it gets the inverse from the cache.  Otherwise it calculates
## the inverse and sets its value via the setinverse function

cacheSolve <- function(x, ...) {
        ## see if the inverse exits
        inverse <- x$getinverse()
        if (!is.null(inverse)){
            ## inverse exists.  Write a message to that effect and return it
            message("using cached inverse")
            return(inverse)
        }
        data <- x$get()
        ## actually calculate the inverse
        inverse <- solve(data)
        ## cache the newly calcuated inverse
        x$setinverse(inverse)
        inverse
}
