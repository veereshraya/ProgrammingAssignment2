## set - Assign matrix with data passed to function. This will be matrix in context for future computations.
##       Keep inverse matrix as nulll
## get - Returns matrix in context for this function
## setInverse - Calculates inverse of matrix in context for this function.
## getInverse - Returns inverse matrix that is already calculated.
## list - Return value of main function. 
##        Returns a named list of functions of makeCacheMatrix without actually executing them.
##        It helps us in executing subfunctions later using names of list.

## This is a function to store matrix as an entity. It provides getter and setter method for matrix itself.
## It also provides getter and setter methods for inverse of the same matrix.

makeCacheMatrix <- function(x = matrix()) {
    ##Default value of inverse matrix is set to null
	##Data of matrix is not known yet		
	invx <- NULL
	##Assign matrix data with values passed Keep inverse as nulll
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
		
    ##get and return current matrix
    get <- function() x
		
	##set value for matrix inverse of current matrix
    setinverse <- function() {
	    invx <<- solve(x)
	}
		
	##get inverse matrix and return it.
    getinverse <- function() invx
		
	##list of named functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function accepts makeCacheMatrix as parameter and calculates inverse of matrix by calling setInverse of makeCacheMatrix

cacheSolve <- function(x, ...) {        
	## get a matrix that is the inverse of 'x' from cache
	invx <- x$getinverse()
		
	##Check if inverse matrix is already available. If yes, Return it. 	   
    if(!is.null(invx)) {
        message("getting cached data")              
    }else{
		##Since inevrse matrix is not available set it		      
		x$setinverse()
              
		##get new inverse matrix
		invx <- x$getinverse()
	}	
		
    ## Return newly calculated inverse
	return(invx)
}
