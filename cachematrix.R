## -----------------------------------------------------------------------------------
## The functions in this file allow the creation of R objects ("cache matrices") that
## encapsulate a numeric matrix and its inverse.  The matrix itself and the inverse
## are cached in memory so that the inverse only needs to be computed once.
## -----------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------
## makeCacheMatrix() takes a single argument, a matrix, and returns a list of named
##      functions defined in the body of the function.  These functions are used to
##      redefine and retieve the matrix (set and get) and to specify and retrieve
##      the cached inverse (setinv and getinv)
## -----------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## The function set(y) assigns the new value y to the label x
        ## and also resets i to NULL to require the inverse to be
        ## recalculated when cacheSolve() is called.
        set <- function(y) {
                x <<- y  
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solv) i <<- solv
        getinv <- function() i
        ## The list below is returned by makeCacheMatrix.  It contains a series of
        ## elements named "set", "get", etc, whose values are pointers to the
        ## functions set(), get(), etc
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## -----------------------------------------------------------------------------------
## cacheSolve plays the role for cache matrices that solve() plays for regular
##      matrices; namely, it returns the matrix inverse. If a value has already
##      been cached, it retrieves and returns this value; otherwise, it uses solve()
##      to compute the inverse and caches this before returning it.  Note that any
##      additional arguments denoted by ... are passed to solve() if this is called.
## -----------------------------------------------------------------------------------
cacheSolve <- function(M, ...) {
        ## Gets the inverse, if it has been cached; otherwise gets NULL
        solv <- M$getinv()
        ## If solv is not NULL, it is the cached value and is returned
        if(!is.null(solv)) {
                message("getting cached data")
                return(solv)
        }
        ## Otherwise, the matrix is retrieved and its inverse is calculated
        data <- M$get()
        solv <- solve(data, ...)
        M$setinv(solv)
        solv
}
