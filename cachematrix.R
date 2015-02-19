## Programming Assignment 2: makeCacheMatrix and cacheSolve

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix, computes its inverse,
## and stores its computed inverse to cache 

makeCacheMatrix <- function(x = matrix()) {
  ## initializes m, matrix, to null
  m <- NULL
  ## sets value of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## gets value of matrix
  get <- function() x
  ## sets value of inverse
  setinverse <- function(solve) m <<- solve
  ## gets value of inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve computes inverse of special matrix created
## with makeCacheMatrix but first checks to see 
## if inverse has already been computed, if so returns inverse
## and skips computation. If not, computes inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if m contains inverse, m is returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## gets value of matrix x and stores to object data
  data <- x$get()
  ## solves for inverse of data and stores to object m
  m <- solve(data, ...)
  ## sets inverse of object m
  x$setinverse(m)
  ## returns object m, (computed inverse)
  m
}
