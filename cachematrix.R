## An implementation of a matrix that caches its inverse to avoid unnecessary computation.

## construct a matrix 
makeCacheMatrix <- function(x=numeric()) {
	# pseudo-object's private members go here
	mat <- x
	inv <- NULL
	
	set <- function (new.mat) {
		mat <<- new.mat
		inv <<- NULL
	}
	
	get <- function () { mat }
	
	set.inv <- function (new.inv) { inv <<- new.inv }
	
	get.inv <- function () { inv }
	
	list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## compute the inverse of the matrix x, optional arguments get passed along to solve

cacheSolve <- function(x,...) {
	mat <- x$get()
	inv <- solve(mat,...)
	x$set.inv(inv)
	return(inv);
}
