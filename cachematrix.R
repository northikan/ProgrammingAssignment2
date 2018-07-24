## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##MakeCacheMatrix creates a List of  functions that set and get a matrix and its inverse in function's calling environment.

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        #function to Set matrix into the environment
        setM <- function(y) {
            m <<- y
            im <<- NULL
        }
        #function to get the matrix from the environment
        getM <- function() m
        #function to set the Inverse of the matrix into the environemnt
        setI <- function(inverseM) im <<- inverseM
        #function to get the Inverse of the matrix from the environment
        getI <- function() im
        #List of functions to manage matrix and its inverse in the environemnt
        list(set = setM, get = getM,
             setinverse = setI,
             getinverse = getI)
}


## Write a short comment describing this function
## cacheSolve verifies if the Inverse of  Matrix is cached in the calling environemnt.  if the inverse does not yet exist, it calculates the Inverse of the Matrix and sets it. In either case it returns the Inverse Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is managed by the MakeCacheMatrix list x
    # try to get the inverse of the matrix from environment
    im <- x$getinverse()
    # Does inverse exists. if so, return the inverse
    if(!is.null(im)) {
        message("getting cached Inverse Matrix..")
        return(im)
    }
    # If Inverse does not exist, get the Matrix, calculate inverse and 
    # set inverse into environment
    # return inverse
    matrix <- x$get()
    im <- solve(matrix, ...)
    x$setinverse(im)
    im
}
