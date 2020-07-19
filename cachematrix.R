## Two fuctions to create a matrix and get its inverse.

## This function creates and stores the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv_m <- NULL
  set_m <- function(y){
    
    x <<- y
    inv_m <<- NULL
    
  }
  get_m <- function(){
    
    x
    
  }
  set_inverse <- function(inv){
    
    inv_m <<- inv
    
  }
  get_inverse <- function(){
    
    inv_m
    
  }
  
  list(set_m = set_m, get_m = get_m, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


## The function, returns the inverse of the matrix inserted
## Before to get the inverse it is evaluated if determinant is not null
## If determinant is null, it is not posible to get the inverse.

cacheSolve <- function(x, ...) {
  
  inv_m <- x$get_inverse()
  if (!is.null(inv_m)){
    
    message("Getting cached data")
    return(inv_m)
    
  }
  
  z <- x$get_m()
  if(det(z) == 0) {
    
    message("This is not an invertible matrix")
    
  }
  else {
    
    inv_m <- solve(z)
    x$set_inverse(inv_m)
    inv_m
    
  }

}