## Creating the cache matrix function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()){
  inv_matrix <- NULL
  get <- function() mat
  setinv <- function(inv) inv_matrix <<- inv_matrix
  getinv <- function() inv_matrix
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed, the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...){
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)){
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinv(inv_matrix)
  inv_matrix
}