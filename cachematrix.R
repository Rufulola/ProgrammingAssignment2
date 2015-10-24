## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions cache the inverse of a matrix.


## ‘makeCacheMatrix’ creates a special matrix object that can cache its 
## inverse. It returns a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	invmatrix <- NULL
	set <- function(y) {
		x <<- y #operator ‘<<-‘ assigns a value to an object in an environment different from the current one
		invmatrix <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) invmatrix <<- inverse
	getinv <- function() invmatrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## ‘cacheSolve’ computes the inverse of the special matrix returned by
## the function ‘makeCacheMatrix’. First, it checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
	invmatrix <- x$getinv()
	if(!is.null(invmatrix)) {
		message("getting cached data")
		return(invmatrix)
	}
	datamatrix <- x$get()
	invmatrix <- solve(datamatrix, ...) #’datamatrix’ assumes a square invertible matrix
	x$setinv(invmatrix)
	return(invmatrix)
}

# Example:
# m <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), 3, 3)
# m
#      [,1] [,2] [,3]
# [1,]    1    0    5
# [2,]    2    1    6
# [3,]    3    4    0
# temp <- makeCacheMatrix(m)
# cacheSolve(temp)
#      [,1] [,2] [,3]
# [1,]  -24   20   -5
# [2,]   18  -15    4
# [3,]    5   -4    1