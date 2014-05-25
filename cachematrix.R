## The two functions below create a special object that stores a matrix
## and cache's its inverse.

## makeCacheMatrix creates a list of functions to 
## 1. set and get the value of the matrix
## 2. set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve returns a matrix that is the inverse of 'x'.
## If the inverse has already been calculated, it gets the inverse from the cache.
## Otherwise, it calculates the inverse of 'x' and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
