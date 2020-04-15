# Used to create Cached Matrix for inverse calculation

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

# > source('cachematrix.R')
# > m <- makeCacheMatrix(matrix(rnorm(9),3,3))
# > cacheSolve(m)
# [,1]       [,2]        [,3]
# [1,]  1.3198238 -0.2822013 -3.13708960
# [2,] -0.7451885  0.6756384  1.39353302
# [3,] -0.2880197 -0.3727569 -0.08436647


