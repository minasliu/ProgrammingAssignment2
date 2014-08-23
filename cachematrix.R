# Coursera-R Class
# R Programming Assignment 2
# by Mina Liu
# 8/23/14
#
# makeCacheMatrix creates special matrix object that can cache its inverse
# The function takes as an argument x, a matrix object
# There are 4 functions associated with the cached matrix object:
#     1) set(x): sets cached matrix as 'x', resets the cached inverse to NULL
#  	So anytime the cached matrix is updated, the inverse is reset to 
#		NULL
#	2) get(): returns the cached matrix
#	3) setInv(invMatrix): sets the inverse matrix of the cached matrix to 
#		invMatrix
#		Note: setInv does not calculate the inverse.  It is passed in.
#	4) getInv(): returns the cached inverse matrix of the cached matrix
#		Note: if the inverse has not been set, returns NULL


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get<-function() x
  setInv <- function(invMatrix) inv <<- invMatrix
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


# The function cacheSolve computes the inverse of 'x', a cached matrix 
# created using makeCacheMatrix.  
# If the inverse has already been calculated and the matrix has not changed, 
#     then cacheSolve retrieves the inverse from the cached matrix object.
# If the inverse has not previously been set using setInv, then 
#  the inverse is calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  cachedMatrix <- x$get()
  inv <- solve(cachedMatrix)
  x$setInv(inv)
  inv
}
