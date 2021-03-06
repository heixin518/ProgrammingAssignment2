## https://github.com/heixin518/ProgrammingAssignment2
## SHA-1 hash：f28db1023337d390ca2f765937cdba210278395e

## Q1: makeCacheMatrix Function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Q2: cacheSolve Function - Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
