## This programme creates a special "matrix" object and computes its inverse.
## To save computation time, the cache functionality is avoid repeat computation.


## This function creates the special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function.
## If the inverse is already calculated and the matrix has not changed, 
## then the cachSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
