## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
      message("Getting cached data")
      return(inv)        
    }
    message("Calculating matrix inverse")
    matrix_x  <- x$get()
    inv  <- solve(matrix_x)
    x$setinv(inv)
    inv
}