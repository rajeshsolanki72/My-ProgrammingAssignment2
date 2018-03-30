## the fuction makeChaeMatrix will create a special Matrix, which is a list conaining 
## functions set the value of the Matrix, getthe value of matrix , set the value of invese,
## get the invese matrix

makeCacheMatrix <- function(x = matrix()) {

  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_mat <<- solve
  getinverse <- function() inv_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinvese = getinverse)
}



## Following function will calculate the invese of matric created in above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  function(x, ...) {
    inv_mat <- x$getinvese()
    if(!is.null(inv_mat)) {
      message("getting cached data")
      return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$setinverse(inv_mat)
    inv_mat
  
}
}

