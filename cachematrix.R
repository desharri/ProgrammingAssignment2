## save inverse matrix in parent envoronment 
## for more efficient processing of looping on the results

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		s <- NULL			# s might be better named as matrixInverse
		set <- function (y) {  
			x <<- y
			s <<- NULL   ## resets inverse to Null if matrix changes
		}
		get <- function() x
		setinverse <- function(solve) s <<- solve
		getinverse <- function() s
		list(set = set, get = get, setinverse = setinverse,
			getinverse = getinverse)
			## remember to use () when calling list elements a$get()
}


## Whis function computes the inverse makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

