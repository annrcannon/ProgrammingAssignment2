## These two functions together take a square matrix and compute
## the inverse of that matrix. The first time the inverse is computed
## the result is stored in the cache.  If the inverse has already been 
## stored in the cache, it is taken from there rather than recomputed.

## This function takes a matrix as its input.  It gives, as its output
## a list of four functions (set, get, setinverse, and getinverse). It also 
## stores the matrix that is passed into it.

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL
      set <- function(y) {
            x <<- y
          im <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) im <<- inverse
      getinverse <- function() im
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function takes as its argument, an object that is the output of
## the makeCacheMatrix function.  It first checks to see if the inverse
## matrix has already been stored in the cache. If so, it tells the user that 
## it is retrieving the inverse from cached data and gives the inverse matrix.
## If there is nothing in the cache, it computes the inverse of the matrix in 
## object passed into cacheSolve.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
               message("getting cached data")
               return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
  
        ## Return a matrix that is the inverse of 'x'
}
