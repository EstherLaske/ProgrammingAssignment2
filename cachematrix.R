## The functions will allow to cache the value of the inverse 
## of a matrix and check if it has alreday been calculated to call
## it when needed

## makeCacheMatrix creates an object containing the inverse  
## of matrix as a cache,and functions to call and set this value
## in a cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse  = getInverse)
  
}


## cacheSolve will check if the value of inverse of the matrix 
## is already calculated in the object created with 
## makeCacheMatrix and if not, it will calculate it to 
## cache it and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, )
  x$setInverse(inv)
  inv
}