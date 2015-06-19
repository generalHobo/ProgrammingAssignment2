## R Programming, programming assignment 2


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	##set() :: only needed to change the matrix stored in the main function 
	##inputs :: new matrix
	##returns :: nothing
      set <- function(y) {
      	x <<- y
		m <<- NULL
      }

	##get() : returns the matrix stored in the main function
	##inputs :: none 
	##returns :: cached matrix
      get <- function() x

	##setinverse() : sets the cache for the inverse matrix
	##inputs :: normal matrix
	##returns :: nothing
      setinverse <- function(cache) m <<- cache

	##getinverse() : returns the inverse matrix stored in the main function
	##inputs :: none
	##returns :: cached inverse matrix
      getinverse <- function() m

	#store a list of the functions that can be called
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

       ## Return a matrix that is the inverse of 'x'
       m <- x$getinverse()

       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }

	#create the inverse of the matrix
	data <- x$get()
      m <- solve(data, ...)
      
	#persist the matrix in the cache
	x$setinverse(m)

      m
}
