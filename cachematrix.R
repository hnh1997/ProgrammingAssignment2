## A pair of functions "makeCacheMatrix" and "cacheSolve" are utilized
## to create a matrix then cache the inverse of said matrix. 

## makeCacheMatrix is the function created to create the matrix.
## I used the example given as a template and changed every 
## "m" to "inv" to indicate we are solving for an inverse
## for readability. I also set every "mean" to "Inverse" for readability.


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse)inv <<- inverse
	getinv <- function()inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is the function created to compute the inverse of the matrix 
## created above (x) and sets the value of the invrse in the cache via the 
## "setInverse" function. If the inverse of the matrix has already been
## calculated, this function "get"s the inverse from the cache and skips 
## the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
