## Put comments here that give an overall description of what your
## functions do

## This function is a factory that creates an object that contains a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  
  #get/set value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  #get/set inverse value of matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  #return the object as a collection (list) of getters and setters
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Create a random uniform square n * n matrix

randommatrix <- function(n) {
    m <- matrix(runif(n*n), ncol=n)
    
    m
}
