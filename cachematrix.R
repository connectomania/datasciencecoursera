# The following creates two functions: makeCacheMatrix and cacheSolve. The purpose is to
# perform, store, and retrieve potentially time-consuming calculations.

# The function makeCacheMatrix creates a list containing following functions:
# 1. set the value of a matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The function cacheSolve checks to see if the inverse of given a square matrix has already 
# been calculated. If not, the function calculates and sets the value of the inverse using
# the setinverse function defined in makeCacheMatrix. If the inverse is already stored, 
# then cacheSolve retrieves the inverse and presents the value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    matx <- x$get()
    inv <- solve(matx)
    x$setinverse(inv)
    inv
}
