## makeCacheMatrix  creates a special matrix object which has three methods
## this special matrix caches the inverse calclated and returns when ever required without computing again
## get			- to get the base underlying matrix class object
## setInverse	- to set the inverse of the matrix - the output of solve
## getInverse	- to get the inverse of the matrix - 

makeCacheMatrix <- function(x=matrix()) 
{
	##initialise the inverse to null when the makeCacheMatrix is called
	inverseMatrix <- NULL
	
	##get function - returns the underlying base matrix
	get <- function() 
	{
		x
	}

	##inverseMatrix is set to inverseX paramter to cache
	setInverse<- function(inverseX) 
	{
		inverseMatrix <<- inverseX

	}
	
	getInverse <- function() 
	{
		inverseMatrix
	}

	##return a list of functions as an R object
	list(get = get, setInverse = setInverse, getInverse = getInverse )
}


## cacheSolve - takes the special matrix object created by makeCacheMatrix 
## returns the inverse of the matrix by calling setInverse 
## if the inverse is not calculated - the inserve is calcualted and set using the setInverse
cacheSolve <- function(x) 
{
	xInverse <- x$getInverse()

	if(!is.null(xInverse))
	{
		message("Cached data found. Getting result... Done.")
		return(xInverse)
	}
	else 
	{
		message("No cached data found. Calculating inverse matrix...")
		
		xInverse <- solve(x$get()) ## finds inverse matrix
		
		x$setInverse(xInverse) ## assigns resulting inverse matrix to object x
		
		return(xInverse)
	}
}
