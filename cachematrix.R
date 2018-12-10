##makeCacheMatrix receives matrix as input, sets and gets the value of the matrix, sets and gets the inverse Matrix	
# caches the matrix in the main environment for use later. 

makecacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL   #sets inverse matrix to Null
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve determines the inverse of the special "matrix" returned by the makeCacheMatrix function and returns it
## If it already hasn't been cacalculated.  If it has, it just returns it. 

cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()
  if (!is.null(invMatrix)) {       #if inverse matrix is not null (or not created)
    message("getting cached data")   #tells you that it exists and is retrieving
    return(invMatrix)               #returns the already inverted matrix and stops the function there
  }
  data <- x$get()
  invMatrix <- solve(data, ...) #solve function inverts matrix
  x$setinverse(invMatrix)     #sets the inverted matrix
  invMatrix                   #returns the newly calculated inverted matrix
}
