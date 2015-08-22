## the following function create a matrix object in memory and then return
## the inverse of the square matrix, from the cache if present in cache, or 
## computes the inverse and then saves in cache

## Creates a matrix object, with a list of functions operable on the object.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	x <<- y
	m <<- NULL
	}
	get <- function() x
	setmat <- function(mat) m <<- mat
	getmat <- function() m
	list(set = set, get = get, setmat= setmat, getmat=getmat)
}


## Returns the inverse if saved in cache or else computes the inverse and 
## saves in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mat <- x$getmat()
	if (!is.null(mat)) {
	message("getting cached inverse")
	return(mat)
	}
	data <- x$get()
	mat <- solve(data)
	x$setmat(mat)
	mat
}
