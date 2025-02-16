makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special matrix object that can cache its inverse.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  #This function computes the inverse of the matrix returned by makeCacheMatrix. 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m      ## Return a matrix that is the inverse of 'x'
}
