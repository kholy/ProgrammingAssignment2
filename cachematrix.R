## Create the CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #the setter function for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #the getter function for the matrix
  get <- function() x
  #the setter function for the matrix inverse
  setinv <- function(s) inv <<- s
  #the getter function for the matrix inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## return the cached matrix inverse or calculate it if it wasn't calculated before

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    #inverse is calculated and stored already
    message("getting cached data")
    return(inv)
  }
  #the inverse wasn't calculated before
  #get the matrix object
  data <- x$get()
  #calculate and set the inverse
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
