## Create a Matrix that can be cached, then compute the inverse
## If it's already calculated, cachesolve will get the inverse of the cache

## Create the matrix

makeCacheMatrix <- function(x = matrix()) {
     m  <- NULL
     set  <- function(y){
          x <<- y
          m <<- NULL 
     }
     get  <- function() x
     setinverse  <- function(inverse) m  <<- inverse
     getinverse  <- function() m
     list(set= set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}

## Solve the inverse, if it's been calculated will return cached data

cacheSolve <- function(x, ...) {
     m  <- x$getinverse()
     if (!is.null(m)){
          message("getting cached data")
          return(m)
     }
     matrix <- x$get()
     m  <- solve(matrix, ...)
     x$setinverse(m)
     m
}
