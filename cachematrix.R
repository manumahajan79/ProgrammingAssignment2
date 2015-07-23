## The folliwing functions allow user to pass an invertible matrix as an argument into a funtion, calculate its inverse 
## and stores the inverse in cache. Everytime user passes a matrix the function checks if the inverse already exists in cache or not.
## If the inverse is found in cache the recalculation is not done and results are displayed from the Cache itself.

## This function gets and sets variables for matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
 	i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse and returns. If it exists in cache, the inverse is not calculated again 

cacheSolve <- function(x=matrix(), ...) {

	i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i


        ## Return a matrix that is the inverse of 'x'
}
