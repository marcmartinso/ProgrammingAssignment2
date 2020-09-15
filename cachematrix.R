###makeCacheMatrix function

# Using the Lexical Scoping the result of makeCacheMatrix function is an object makeCacheMatrix 
# which will be passed to the following Cachesolve function

makeCacheMatrix <- function (x = matrix()) {
  inverse <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x #retrieve the value of x
  setInverse <- function(inv.mat) inverse <<- inv.mat #set retrieve the value of matrix
  getInverse <- function() inverse # retrieve the value of matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) # return list of makeCacheMatrix objects
  
}

###Cachesolve function

# cacheSolve uses the object created with makeCacheMatrix, uses its x$getInverse() possible 
# with the Lexical Scoping and tests if it is null, If it is not null, return a message with 
# its value, and if its is null it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) { # test if the value of is null or not and return a inverseessage
    message("getting cached data")  
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
