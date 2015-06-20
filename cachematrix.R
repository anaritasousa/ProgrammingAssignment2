## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv<- function(inverse) inv_x <<-inverse
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  ##if the inverse is already cached then it returnes the inverse right away
  if (!is.null(inv_x)) { 
    message("getting cached inverse matrix")
    return(inv_x)
  } else { ##if the inverse is not cached then use solve to calculate
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
