## R programming assignment 2
## functions makeCacheMatrix() and cacheSolve() permit
## the inverse of a matrix to be calculated and teh value stored
## Once value is stored, this can be retrieved without re-calculation
##
## Example:
## mdat <- matrix(c(1,2,3, 0,4,5, 1, 0, 6), nrow = 3, ncol = 3)
## x <- makeCacheMatrix(mdat)
## cacheSolve(x)


## Create list of set, get, setinverse and getinverse functions
## for calculation of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		  s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## checks to see if inverse of matrix has been calculated and stored.  
## Returns stored value if present, 
## else calculates matrix inverse using 'solve'

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
