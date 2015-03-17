## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y  
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solv) i <<- solv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(M, ...) {
        solv <- M$getinv()
        if(!is.null(solv)) {
                message("getting cached data")
                return(solv)
        }
        data <- M$get()
        solv <- solve(data, ...)
        M$setinv(solv)
        solv
}
