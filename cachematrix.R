#Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(numeric(0), 0,0)) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inversed) inverse <<- matrix_inversed
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Computes the inverse of the special matrix returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed), 
#then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  result  <- x$getinverse()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setinverse(result)
  result
}