## Michael Assink
## R Programming Assignment 2 (Coursera)
## 1/8/2018

## These functions implement this odd list structure which 
## embeds functions to enable caching of the results of solve
## in the exact manner as the functions makeVector and cacheMean 
## which were provided by the instructors

makeCacheMatrix <- function(x = matrix()) {
  ## creates this weird fake matrix/function object (returns list)
  ## that is later passed to cacheSolve
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## takes a CacheMatrix object created by makeCacheMatrix()
## returns the inverse using makeCacheMatrix$getsolve()
## implements a cache mechanism which can be inadvertantly overridden
## by passing a recently instantiated makeCacheMatrix objects

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

