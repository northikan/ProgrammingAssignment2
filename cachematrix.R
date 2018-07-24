## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##MakeCacheMatrix creates a List of  functions that set and get a matrix and its inverse in function's calling environment.

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        setM <- function(y) {
            m <<- y
            im <<- NULL
        }
        getM <- function() m
        setI <- function(inverseM) im <<- inverseM
        getI <- function() im
        list(set = setM, get = getM,
             setinverse = setI,
             getinverse = getI)
}


## Write a short comment describing this function
## cacheSolve verifies if the Inverse of  Matrix is cached in the calling environemnt.  if the inverse does not yet exist, it calculates the Inverse of the Matrix and sets it. In either case it returns the Inverse Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is managed by the MakeCacheMatrix list x
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached Inverse Matrix..")
        return(im)
    }
    matrix <- x$get()
    im <- solve(matrix, ...)
    x$setinverse(im)
    im
}




cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}