## This functions compute the inverse of the matrix and cache the value 
## so that when we need it again, it can be looked up in the cache 
## rather than recomputed.


## makeVector creates a special "matrix", which is really a list 
## containing a function to set and get the value of the matrix
## and get and set the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Return the inverse of the special "matrix" created for function makeCacheMatrix. 
## First time calculate and set the value of the inverse in the cache. 
## Next, get the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m
}
