## The function caches the inverse of a matrix

## The "makeCacheMatrix" contains a list of four functions, which 
## 1.set matrix value 2. get matrix value 3.set inverse of the matrix 
## 4.get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(inv) m <<- inv
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## In the "cacehSolve" function, we first search whether the inverse 
## has been computed. If so, the result is retrived and output. Otherwise, 
## it goes to the "get.inverse" function. 

cacheSolve <- function(x, ...) {
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$set.inverse(m)
  m
}
