###makeCacheMatrix function

makeCacheMatrix <- function (x = as.matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #retrieve the value of x
  setmatrix <- function(inv.mat) m <<- inv.mat #set retrieve the value of matrix
  getmatrix <- function() # retrieve the value of matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix) # return list of makeCacheMatrix objects
  
}
# the result is an object makeCacheMatrix which will be passed to the following

# cacheSolve function

###Cachesolve function

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) { # test if the value of is null or not and return a message
    message("getting cached data")  
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

