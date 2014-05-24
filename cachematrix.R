##This is a function that takes the arguments of a matrix values and its rows and creates a list of functions for that matrix
## The list of functions it returns are the following
##set is used to set the values of the matrix
##get is used to get the values of the matrix
##setsol is used to set the inverse of the matrix
##getsol is used to get the inverse of the matrix

## The function takes the arguments x,r,c and defines a matrix of x values, r rows, and c columns and returns a list of fuctions
## This functions can be used to obtain or modify the arguments of the matrix.

makeCacheMatrix <- function(x,r,c) {
	m <- matrix(x,r,c)
	sol <- NULL
	set <- function(y1,y2,y3) {
		m <<- matrix(y1,y2,y3)
		sol <<- NULL
	}
	get <- function() m
	setsol <- function(solve) sol <<- solve
	getsol <- function() sol
	list(set = set, get = get, setsol = setsol, getsol = getsol)
}


##This function searchs if the solution for the inverse matrix has already been definied in the cache. 
##If the inverse solution has been already defined it returns the value.
##If the inverse solution has NOT been defined it computes the solution, saves it in the cache, and returns the value.

cacheSolve <- function(m, ...) {
	sol <- m$getsol()
	if(!is.null(sol)) {
		message("getting cached data")
		return(sol)
	}
	data <- m$get()
	sol <- solve(data, ...)
	m$setsol(sol)
	sol
}
        ## Return a matrix that is the inverse of 'x'

