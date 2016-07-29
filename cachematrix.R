## The makeCacheMatrix and cacheSolve functions are used to create an speacial object which holds 
## a matrix and its inverse.

## Creates a special objects whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setsolve <- function(solve) inv <<- solve
		getsolve <- function() inv
		list(set = set, get = get,
			 setsolve = setsolve,
			 getsolve = getsolve)
}


## Returns the cached inverse of the matrix or computed inverse if it is null.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x.getsolve()
		if(!is.null(m)){
				message("getting cached data")
				return(inv)
		}
		data <- x.get()
		inv <- solve(data, ...)
		x$setsolve(inv)
		inv
}