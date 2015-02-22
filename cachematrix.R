## makeCacheMatrix: Creates a list of nested functions performed on a matrix 'x' 
## These nested functions "set" (or cache) and "get" the matrix and its inverse

## cacheSolve: Return the inverse of matrix 'x' from cache if possible

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL
    
    # superassign the passed variable 'y' to the local variable 'x'
    # superassign the local variable 'inv' to NULL   
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    
    # superassign the passed variable 'inv_passed' to the local variable 'inv'
    setinv <- function(inv_passed) inv <<- inv_passed
    
    # Get the cached value of matrix inverse 'inv'
    getinv <- function() inv
    
    # Store nested functions in a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the matrix 'x' inverse from cache or by calculation
## Where the matrix 'x' inverse value is calculated, it is then cached

cacheSolve <- function(x, ...) {
  
    # Get the cached value of matrix x, and if not a NULL value, return it
    inv <- x$getinv()
    if(!is.null(inv)){
      message("Getting cached data")
      return(inv)        
    }
    
    # If the cached value of matrix 'x' is null, calculate it
    message("Calculating matrix inverse")
    matrix_x  <- x$get()
    inv  <- solve(matrix_x)
    
    # Set the inverse of the matrix 'x' in the cache list
    x$setinv(inv)
    
    # Display the newly calculated inverse of the matrix 'x' 
    inv
}