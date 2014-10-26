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
	inputMatrix <<- x
	latestInverse <<- FALSE
	
	setMatrix <- function (inpMatrix) {
		inputMatrix <<- inpMatrix
		inverseMat <<- NULL
		latestInverse <<- FALSE      ## Reset the flag to show inverse is outdated
	}
	getMatrix <- function () inputMatrix
	
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
        ## Return a matrix that is the inverse of 'x'
		
		## The function attempt to check the following condition to 
		## determine if the inverse needs to be computed.
		
		## Condition #1 :: Check if the matrix supplied is valid
		## without Null Value
		
		## Condition #2 :: Check if the matrix supplied has changed
		## since the inverse was computed last time. If it was not 
		## computed, determine the inverse and update the cache with 
		## inverse
		
		## Condition #3 :: Return the inverse value
		

		cacheValues <- makeCacheMatrix(x)
		
		matrixValue <- cacheValues$getMatrix()

		inverseValue <- cacheValues$getInverse()
		
		if(is.null(matrixValue)) {         				### Condition 1
			return (matrixValue)
		}
		else if (!cacheValues$isLatestInverse()) {      ### Condition 2
		
			cacheValues$setMatrix(x)
			inverseValue <- solve(x)
			cacheValues$setInverse(inverseValue)
			return (inverseValue)

		}
		else {                                          ### Condition 3
			return (inverseValue)
		}
		
		
}
