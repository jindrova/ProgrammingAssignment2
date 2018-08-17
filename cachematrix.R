## Two functions below enable caching of the inverse matrix instead of calculating it repeatedly.

## This function creates a special "matrix" object that can cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function uses the makeCacheMatrix function. It returns the inverse matrix either from the cache in case it was already calculated 
## or it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
