## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #Initialize the matrix
        inv <- NULL

        #Set Matrix data
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        #Retrieve actual matrix
        get <- function() x

        #Set inverse matrix
        setinverse <- function(inverse) inv <<- inverse

        #Retrive inverse matrix
        getinverse <- function() inv

        #Retern the special matrix object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Get the inverse matrix from the special matrix object
        inv <- x$getinverse()

        #If the inverse in not NULL, it's stored in the cache, so return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # Retrieve actual matrix
        matrix_to_invert <- x$get()

        #Solve for the inverse
        inv <- solve(matrix_to_invert, ...)

        #Set the inverse matrix in the special matrix object, going into cache
        x$setinverse(inv)
        inv
}
