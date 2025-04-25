## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Argument of setter is assigned to x in the global environment
  # Cache is set as empty
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Getter return x as it is
  get <- function() x
  
  # Pass solve(x) into m in the global environment
  setinverse <- function(solve) m <<- solve
  
  # Get the inverse from the cache(m)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Get the cache(m), if not empty -> return the result
  m <- x$getinverse()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  # Else getter get x, pass it into solve 
  # and pass the whole thing into setinverse
  # which put the result in m
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}