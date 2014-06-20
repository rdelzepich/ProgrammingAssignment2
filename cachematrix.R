## Put comments here that give an overall description of what your
## functions do

## Coursera Course rprog-004
## Assignment: Caching the Inverse of a Matrix
## Ralph Delzepich

## basically, this is an adaption of the vector example
## it creates a special matrix, that stores its own inverse in a list
makeCacheMatrix <- function(x = matrix()) {
    ## set the inverse inv to null
    inv <- NULL
    ## create the setter and getter functions
    set <- function(y) {
        x <<- y
        inv <<- null
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    ## set up the list (that makes the special matrix so special ;)
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## this will return the inverse of the passed "special matrix" matrix
cacheSolve <- function(x, ...) {
    ## get the current content of the inverse matrix
    inv <- x$getinv()
    ## if there is a cached entry (inv is not null), return this and leave out
    ## the time consuming calculation
    if(!is.null(inv)) {
        message ("getting cached data")
        ## Return a cached matrix that is the inverse of 'x'
        return(inv)
    }
    ## there was no cached inverse, so do the calculation here
    data <- x$get()
    inv <- solve(data,...)
    ## store the inverse calculation result in the cache
    x$setinv(inv)
    ## Return a freshly calculated (and then stored) matrix that is the inverse of 'x'
    inv
}
