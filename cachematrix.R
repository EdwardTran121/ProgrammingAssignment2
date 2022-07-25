## These functions cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Make NULL variable for nothing to cache
  set <- function(y) { #Set a new matrix
    x <<- y
    m <<- NULL
  }
  
  #Return matrix
  get <- function() x
  
  #Set inverse matrix
  setinverse <- function(inverse) m <<- inverse
  
  #Get inverse matrix
  getinverse <- function() m
  
  #Return a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function make the solve function
cacheSolve <- function(x, ...) {
  #Get cached matrix
  m <- x$getinverse()
  
  #Check whether matrix exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  #Return the inverse
  m       
}
