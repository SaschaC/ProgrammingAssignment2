
## This function creates a special matrix object with functions to 1. set the value 2.get the value
## 3.set the value of the inverse 4. get the value of the inverse.

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

# This function calculates the inverse for the matrix object created infunction above.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
# via the setinverse function.

cacheSolve <- function(x, ...) {
       
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve (data, ...)
  x$setinverse(i)
  i
}

# test the functions:

matu = matrix(1:4,2,2)
x = makeCacheMatrix(matu); x
cacheSolve(x)
