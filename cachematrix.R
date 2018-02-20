## The following functions create a special 'matrix' that caches it's inverse

## The following function, makeCacheMatrix creates a list containing a function to
##set the value of the matrix and reset the inverse matrix to NULL
##get the value of the matrix
##set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function retreives the inverse of the matrix, if it has been cached
## If the inverse of the matrix hasn't been cached then the function solves for the inverse and caches the result

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
