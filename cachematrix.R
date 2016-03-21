## makeCacheMatrix expects as input an invertible matrix,
## returns a list of functions:
## 1. $set : sets the matrix
## 2. $get : gets the matrix
## 3. $setinverse : returns the inverse of the matrix
## 4. $getinverse : the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) x <<- y; i <<- NULL
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve expects a makeCacheMatrix object and
## returns the inverse of the matrix provided.
##
## cacheSolve first determines if the inverse of the
## makeCacheMatrix object has already been computer,
## and returns the computer value if it is found.
##
## Otherwise, cacheSolve will compute the inverse for
## the provided matrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix)
  x$setinverse(i)
  i
}
