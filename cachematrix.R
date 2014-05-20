##
## The two functions below allow for computation and caching of matrix inverses.  The assumption is that
## the matrix inverse exists.  The following comment lines shows how these two functions were actually used 
## in a R interpreter session to compute the inverse of a matrix
## 
## > xm<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
## > xm$getMatrix()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > xm$getMatrixInverse()
## NULL
## > xm$setMatrix(matrix(1:4,2,2))
## > xm$getMatrixInverse()
## NULL
## > xm$getMatrix()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > xi<-cacheSolve(xm)
## > xi
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

##
## This function creates a "matrix" which is really a list of functions.Each of these functions
## allow for the caching of the inverse of the matrix  
## The return value is a list of these functions 
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setMatrixInverse<- function(inverse) m <<- inverse
  getMatrixInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

##
## This function caches the inverse of a matrix.  The specific matrix to be cached
## is passed as a parameter and must be "made" using the makeCacheMatrix function above
## It returns the cached inverse of the matrix passed as an argument (argument x)
## 

cacheSolve<- function(x, ...) {
  ##
  ## Return a matrix that is the inverse of 'x'
  ##
  m <- x$getMatrixInverse()
  ##
  ## Determine if matrix inverse is already computed and cached and return it
  ##
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##
  ## Matrix inverse has not been cached nor computed so must be computed first
  ## 
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  ##
  ## Return matrix inverse
  ##
  m
}
