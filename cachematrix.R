## These functions will calculate the inverse of a square matrix. If the inverse of the
## matrix has already been calculated it will not be calculated again but instead return
## a cached value

## This function receieves the matrix, sets the matrix, gets the matrix, sets the inverse
##  of the matrix and gets the inverse of the matrix.  A list containing these functions
## is returned to the user for use by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

#initialze  m
        m <- NULL
#set the matrix       
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
#get the matrix
        get <- function() x
#set the inverse of the matrix
        setinverse <- function(solve) m <<- solve
#get the inverse of the matrix
        getinverse <- function() m
#return the set, get, setinverse, and getinverse functions to the user
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)

}


## This function will calculate and return the inverse of the matrix.  If the matrix has
## not changed, it will return the cached value

cacheSolve <- function(x, ...) {
# get the inverse of the matrix and save it to m
        m <- x$getinverse()
# check to see if the inverse has already been solved. If so, return the solved inverse
# and indicate with message
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#If it has not been solved, work through the following functions which were returned by
# makeCacheMatrix function
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
