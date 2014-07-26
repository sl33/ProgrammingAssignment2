## 
#  1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#  2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#
## 

makeCacheMatrix <- function(x = matrix()) {
    ## init the inverse variable
    inv <- NULL

    ## function to set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## function to get matrix
    get <- function() x

    ## function to set inverse of matrix
    setinv <- function(inverse) inv <<- inverse
    
    ## function to get inverse of matrix
    getinv <- function() inv

    ## Lists of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {

    ## assign the inverse of matrix x to inv variable
    inv <- x$getinv()

    ## if inv is not null then return cached inverse matrix data
    if (!is.null(inv)) {
        message("getting cached inverse matrix data")
        return(inv)
    }

    ## get matrix object and apply inverse solve function
    data <- x$get()
    inv <- solve(data, ...)

    ## set inverse matrix and return
    x$setinv(inv)
    inv
}
