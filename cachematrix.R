# Assignment2: Write a pair of functions that cache the inverse of a matrix.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, we assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # cache matrix thah will be made
    # set the value of the matrix
    set <- function(y) {
        x <<- y          # assigning a value to object with <<-
        m <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # set the value of the inverse matrix
    setsolve <- function(solve) m <<- solve 
    # get the vaue of the inverse matrix
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve) 
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
# changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    # see if inverse is already calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()        # get data form special input matrix x
    m <- solve(data, ...)  # use function solve to calculate inverse matrix 
    x$setsolve(m)          # set inverse with setsolve 
    m  # returs matrix m that is inverse of x
}
