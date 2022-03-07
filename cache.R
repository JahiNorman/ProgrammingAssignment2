	makecachematrix <- function(x=matrix()){
		inv <- NULL		#intializing inverse as null 
		set <- function(y){

	x  <<- y {
		inv  <<- NULL
} 

	get <- function()x		#function to get matrix
		setinverse <- function(inverse)inv
    	      	getinverse <- function() {
						inver <-ginv(x)
						inver%*%x		#function to obtain inverse of the matrix
						}
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
	cacheSolve <- function(x, ...) {		#gets cache data
		inv <- x$getinverse()
		if(!is.null(inv)) {			#checking whether inverse is null
					message("getting cached data")
					return(inv)	#returns the inverse value 
}
	data <- x$get()
		inv <- solve(data, ...)		#calculates inverse value 
		x$setinverse(inv)
		inv	## returns a matrix that is the inverse of "x"
}
