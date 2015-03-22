## Matrix inversion is usually a costly computation 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse 
## and a list containing a function to
## set value of the matrix, get value of the matrix, 
## set value of inverse of the matrix, get value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    i <<- inverse
  }	
  getInverse <- function(){ 
    i
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function 
# if the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve function should retrieve the inverse from the cache
# The following function returns the inverse of the matrix. It first checks if
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Retrieving Cached Data.")
    return(i)
  }
  dt <- x$get()
  i <- solve(dt)
  x$setInverse(i)
  i
}