## 1. function: A matrix is calculated and in a list the get-, set-, getinverse-
## and getinverse-functions ar stored. By calling these functions
## (via x$function()) the matrix (and its inverse) can be shown,
## committed or set.
## 2. function: if the inverse has not been calculated or stored, it calculates
## and stores it

# creates a list with content:
# set-function (to set / change a matrix),
# the get-function to get (and for example show) the matrix (computed by the input x)
# setinverse-function (sets (calculated) inverse of the matrix)
# getinverse-function (gets the inverse oft the matrix)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# checks if the inverse of the matrix already exists in getinverse. if not:
# takes the matrix calculated by makeCacheMatrix via get-function
# and calculates the inverse of the matrix and stores is in setinverse

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!(is.null(inv))) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    return(inv)
}
