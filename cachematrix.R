## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## Specifically creates variable in the 'get' function and creates list of functions (set/get)
## from which the next function can call.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. 

cacheSolve <- function(x) {
        ## Returns a matrix that is the inverse of 'x'
 
        ## If the inverse has already been calculated (and the matrix has not 
        ## changed), then retrieve the inverse from the cache.
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Otherwise, calculate the inverse.
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
