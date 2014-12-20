## The makeCacheMatrix and cacheSolve are being created to
## solve the inverse of a matrix (assuming the inverse
## exists) and caching it rather than computing it
## repeatedly.

## The makeCacheMatrix calculates the inverse of the
## matrix and caches it.

makeCacheMatrix <- function(x = matrix()) {
## Creating a variable m inside the makeCacheMatrix
## environtment
    
  m <- NULL 
  set <- function(y) {
      
## The value of y is superassigned to x, the input
## variable.
  
  x <<- y
    
## m gets reinitialized to NULL, if x is different.
    
  m <<- NULL 
    
  }

## To obtain the values of the matrix, the get function
## is called upon and used by the cacheSolve function.

  get <- function() x

## The inverse of the matrix is assigned to the
## the variable m, which is in the makeCacheInverse
## environment.

  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m

## Allows the use of the functions listed below outside
## the environments of makeCacheMatrix.

  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of a
## a matrix if it has not been determined. 

  cacheSolve <- function(x, ...) {
  
## The getInverse functions gets called upon and assigned 
## to m.

  m <- x$getInverse()

## Checking if inverse has already been calculated and
## assignmed to m.

  if(!is.null(m)) {

## If the if condition returns TRUE, the inverse is returned,
## the value m. If FALSE, the inverse is computed.    

      message("Getting the Cached Matrix")
      return(m)
  }

## The get function is called upon from makeCacheSolve to 
## populate variable date with the data to solve for the
## inverse of the matrix.

  data <- x$get()

## The solve function is used to calculate the inverse and
## assigning the value of the inverse to m.

  m <- solve(data, ...)
    
## The new inverse is cached and assigned to m in
## makeCacheSolve function.

  x$setInverse(m)

## Printing m, the inverse

m
    
}
