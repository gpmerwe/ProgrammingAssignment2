## The first function creates a list that contains 
## the matrix itself and its inverse
## The second function looks up the list to retrieve
## the inverse of the matrix

## The makeCacheMatrix function consists of 4 functions,
## setmatrix sets the contained matrix to that of your matrix
## getmatrix just retrieves the contained matrix
## setinverse calculates the inverse of the contained matrix
## getinverse retrieves the stored inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      setmatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      getmatrix <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(setmatrix = setmatrix, getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve retrieves the inverse of you matrix from the
## list created by the makeCacheMatrix function
## If the inverse is stored, then it just retrieves the inverse
## else it calculates the inverse, stores and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("Getting cached data")
            return(m)
      }
      data <- x$getmatrix()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}
