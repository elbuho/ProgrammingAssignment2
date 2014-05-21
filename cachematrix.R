## this functions caching the inverse of a matrix
## rather than compute it repeatedly

## this function creates a special Matrix objet that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## the function computes the inverse of de matrix, if the inverse
## has already been calculated retrieve de inverse fromm de cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
