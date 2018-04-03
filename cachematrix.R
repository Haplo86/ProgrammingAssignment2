## This FUN chache the results of a Solve function called over an invertible
## matrix so, if the matix does not change, the operation is not computed again
## and the result is recovered from the cache

## This firs FUN create a special matrix, set and get the values of the matix 
## and of the solve FUN

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list ( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This FUN Solve the invertible matrix and if a solution for the matrix already
## exists the FUN get it from the makeCacheMatrix FUN instead of computing the
## results again

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if (!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}