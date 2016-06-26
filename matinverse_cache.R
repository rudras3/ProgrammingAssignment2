
makeMatrix <- function(x = matrix()) {
   ## By Sunil Rudravajhala for Assignment 3 2016/06/25
	## function creates list containing functions for set,get value of matrix and set,get 
	##  matrix inverse
	m <- NULL
	set <- function(y) {
		 x <<- y
		 m <<- NULL
	}
	get <-function() x
	setmatrixinv <- function(inverse) m <<- inverse
	getmatrixinv <- function() m
	list(set=set, get=get, setmatrixinv=setmatrixinv, getmatrixinv=getmatrixinv)
}
cachematrixinv <- function(x, ...) {
	## this function calculates the inverse of a matrix and checks if its cached first
	m = x$getmatrixinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrixinv(m)
	m
}
testcache = function(m){
	## to test functions above
	tempm=makeMatrix(m)
	start = Sys.time()
	cachematrixinv(tempm)
	end=Sys.time()
	diff = end - start
	print(diff)

	start = Sys.time()
	cachematrixinv(tempm)
	end=Sys.time()
	diff = end - start
	print(diff)	
}
