##this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##This function calculates the inverse of the matrix from the makeCacheMatrix
##function, If the inverse is already calculated and the matrix doesnt change, 
##this function just retrieves the inverse from the cache

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
}
