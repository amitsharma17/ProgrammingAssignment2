## This function creates a special "matrix" object that can cache its inverse
## The function itself contains four function as given in the example makeVector
## get: to show the matrix, set: to change the matrix, getinverse: to get inverse, if in cache
## setinverse: to set inverse in cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
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

## CacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}