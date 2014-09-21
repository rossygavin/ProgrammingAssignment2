## This first function, makeCacheMatrix creates a list of functions that:
## set the value of a matrix
## get the value of a matrix
## set the value of an inverse of a matrix
## get the value of an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)

}


## This second function, cacheSolve returns the inverse of a matrix.
## It first checks if the inverse has already been computed and if it has it returns the inverse.
## If it hasn't been calculated, it computes the inverse and sets the value in the cache using the setinver function.

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinver(inver)
  inver
}