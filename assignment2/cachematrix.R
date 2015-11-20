## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## set the matrix
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix
  get <- function(){
    x
  }
  
  ## set inverse matrix
  setinv <- function(i){
    inv <<- i
  }
  
  ## get inverse matrix
  getinv <- function(){
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
## compute the inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  ## return the inverse if its already set
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## get matrix from data
  data <- x$get()
  ## use matrix multiplication to get the inverse
  inv <- solve(data, ...)
  
  x$setinv(inv)
  inv
  
}
