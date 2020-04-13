## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #local inverse varivale inv set to null initially
  inv <- NULL
  
  #1. set function
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #2. get function
  get <- function(){
    x
  }
  #3. set inverse function
  setinv <- function(inverse_){
    inv <<- inverse_
  }
  
  #4. get inverse function
  getinv <- function(){
    inv
  }
  
  #retuning a list with above functions
  list(set = set, get = get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function



## This function recieves a list of functions

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message('cached inverse of matrix')
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}