##Functions to create/read etc. a matrix with a cached inverse

##The function which generates the "object" containing the matrix and the functions needed for caching the inverse
makeCacheMatrix <- function(x = matrix()) {
  ##I start by defining the functions I want to be included in the returned list
  inverse <- NULL
  setMatrix <- function(newMatrix){x <<- newMatrix 
                                   inverse <<- NULL}
  getMatrix <- function(){x}
  setInverse <- function(x){inverse <<- solve(x)}
  getInverse <- function(){inverse}
  ##I then return the list containing the 4 functions
  return(list(set=setMatrix, get=getMatrix, setInverse=setInverse, getInverse=getInverse))
}



##The function used to read the cached inverse matrix
cacheSolve <- function(x, ...) {
## I start by finding out what the getInverse function returns
  inverse <- x$getInverse()
  if(is.null(inverse)){
## If the inverse is not defined, calculate and cache the inverse
    matrix <- x$get()
    inverse <- x$setInverse(matrix)
  }
#Finally, return the inverse
  return(inverse)
}
