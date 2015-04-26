## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
		inv <- NULL ## set the initial inverse to NULL
		preMatrix <- NULL ## cache the previous matrix which the inverse already been calculated
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		getpre <- function() preMatrix
		setinverse <- function(inverse) {
				inv <<- inverse
				preMatrix <<- x
		}
		getinverse <- function() inv
		list(set = set, get = get, getpre = getpre,
				setinverse = setinverse,
				getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inv) && x$get() == x$getpre()) {
                message("getting cached inverse")
                return(inv)
        }
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv # return the inverse
}
