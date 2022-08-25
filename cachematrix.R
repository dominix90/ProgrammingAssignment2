## The following functions work in combination to obtain 
## a cached version of the solve() function, that computes
## the inverse of a matrix.

## This function aims to create a non-atomic version
## of the input argument (e.g., x). In this way, we 
## can cache, together with the matrix, the inverse
## of the same x, and access more efficiently to the
## data.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## The following function takes the input x and computes
## the inverse of x. First, it checks if the inverse has
## already been computed. If not, the results of the first
## get will be NULL, and a new computation of the inverse, 
## by means of the solve() function, will be performed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
