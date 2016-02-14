## makeCacheMatrix and cacheSolve create a structure that store
## a given matrix and its inverse, and also provide the methods
## to calculate the inverse of a matrix when not available.

# Creates a list of functions with accessors to the matrix x and its inverse.
# The inverse can be null.
# @returns nothing.
# @exception  matrix x can't be null and must be a reversible matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# Calculates, retrieves and stores the inverse of the matrix x.
# @returns the inverse of the matrix whether by calculating it or by retrieving it from cache.
# @exception  matrix x can't be null and must be a reversible matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
