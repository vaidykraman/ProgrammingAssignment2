## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function accepts the matrix as input and return list of functions 
## that can be invoked to set and get the cached values of matrix and 
## its inverse. 

## The matrix also stores a special variable to track if latestInverse
## is computed by turning off and on a FLAG.

makeCacheMatrix <- function(x = matrix()) {
	
	inverseMat <- NULL
	latestInverse <<- FALSE
	
	setMatrix <- function (inpMatrix) {
		x <<- inpMatrix
		inverseMat <<- NULL
		latestInverse <<- FALSE      ## Reset the flag to show inverse is outdated
	}
	getMatrix <- function () x
	
	setInverse <- function (inpInverse) {
		inverseMat <<- inpInverse
		latestInverse <<- TRUE       ## Enable the flag to show inverse is current
	}
	
	## Function to return the boolean to show if the Inverse is computed
	## for the matrix cached. This function will serve to reduce the 
	## computing required to compute the inverse
	
	isLatestInverse <- function () latestInverse
	
	getInverse <- function () inverseMat
	
	list(getMatrix = getMatrix, setMatrix=setMatrix,getInverse = getInverse, 
	      setInverse = setInverse,isLatestInverse = isLatestInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a Inverse matrix from the Cache
		
		## The function attempt to check the following condition to 
		## determine if the inverse needs to be computed.
		
		## Condition #1 :: Check if the matrix supplied is valid
		## without Null Value
		
		## Condition #2 :: Check if the matrix supplied has changed
		## since the inverse was computed last time. If it was not 
		## computed, determine the inverse and update the cache with 
		## inverse
		
		
		matrixValue <- x$getMatrix()

		if(is.null(matrixValue)) {         	  ### Condition 1 - Return the NULL
			return (matrixValue)
		}
		else if (!x$isLatestInverse()) {      ### Condition 2 - Compute the inverse & Cache
		
			inverseValue <- solve(matrixValue)
			x$setInverse(inverseValue)
			return (inverseValue)

		}
		else {                                          
			return (x$getInverse())
		}
}
##########################
## Examples
##########################
## Condition #1
##
##     mcm <- makeCacheMatrix(matrix())
##     > cacheSolve(mcm)
##          [,1]
##     [1,]   NA
##     > mcm$getMatrix()
##          [,1]
##     [1,]   NA
##
##########################
## Condition #2
##     
##     > sampMat <- matrix(c(1:4),ncol=2,nrow=2)
##     > mcm$setMatrix(sampMat)
##     > cacheSolve(mcm)
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##     > 
##     > mcm$setMatrix(matrix(c(3:6),ncol=2,nrow=2))
##     > cacheSolve(mcm)
##           [,1] [,2]
##     [1,]   -3  2.5
##     [2,]    2 -1.5
##     > 
##     > cacheSolve(mcm)              ### Value obtained from Cache
##          [,1] [,2]
##     [1,]   -3  2.5
##     [2,]    2 -1.5
##     > 
##
##########################
