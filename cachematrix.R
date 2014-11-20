## Put comments here that give an overall description of what your
## functions do
##Overall These both functions are used to calculate inverse of a matrix
##and also cache the inverse of the matrix so that a heavy calculation like 
##calculating the inverse need not be carried out again and again


## Write a short comment describing this function
##makeCacheMatrix is a function that consists of 4 member functions
##a)set -> To set/initialize the matrix.
##b)get -> To get the value of the matrix.
##c)setinverse -> This function is used to assign the inverse of the matrix.
##d)getinverse -> This function is used to fetch the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
{
	x_inv <- NULL
	set <- function(y) 
	{
		x <<- y
		x_inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) x_inv <<- inv
	getinverse <- function() x_inv
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Write a short comment describing this function
##CacheSolve is a function that is used to calculate the inverse of the matrix
##and also fetch from cache (the inverse) if the matrix is already loaded into memory and its inverse is already calculated once

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	m_inv <- x$getinverse()
	if(!is.null(m_inv)) 
	{
		message("Getting Inverse Of the Matrix from Cached Data")
		return(m_inv)
	}
	data <- x$get()
	m_inv <- solve(data, ...)
	x$setinverse(m_inv)
	m_inv
		
}
