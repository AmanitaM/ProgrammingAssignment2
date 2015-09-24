## Cache the Inverse of a Matrix
## call with makeCacheMatrix(num) 
## Create a special "matrix" object that can cache its inverse
## Pre-create a matrix with A <- matrix(1:4,2,2) or
## B <- matrix(c(num,num),nrow,ncol)
## Call function : a1 <- makeCacheMatrix(A)
## Then call cachesolve(a1)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Compute the inverse of the "matrix" object created above. If the
## object hasn't changed and the inverse is already calculated, 
## retrieve the calculated value from cache.
## Call with cachesolve()
cachesolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      matrix <- x$get()
      print(m)
      ## m <- solve(x) %% x   ## inverse of x
      m <- solve(matrix,...)
      x$setmatrix(m)
      m
      ## Tested for save in cache ...
      }
