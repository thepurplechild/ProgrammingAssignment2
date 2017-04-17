## The following functions create an R object that can store
## a matrix and cache its inverse

## The makeCacheMatrix function below creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function()I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cachesolve function below creates an inverse of the matrix stored by
## the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data") 
                return(I)
        }
        data <- x$get()
        I <- solve(data,...)
        x$setinverse(I)
        I
}
