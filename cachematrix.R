## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get  the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {   ## creates a matrix from function parameter
                x <<- y
                m <<- NULL
        }
        get <- function() x                            ## When this function is called, it returns the matrix
        setinverse <- function(inverse) m <<- solve(x) ## solve calculates the inverse of a matrix only when this function is called
        getinverse <- function() m                     ## getinverse returns the inverse matrix
        list(set = set, get = get,                     ## list of functions to return
             setinverse = setinverse,
             getinverse = getinverse)
}


##cacheSolve function calculates the inverse of the special "matrix" created with the above function
## It is completely necessary to have create the matrix using the function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                     ## It tries to get the inverse matrix
        if(!is.null(m)) {                       ## In case the inverse has already calculate, this object is returned
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         ## In case the inverse hasn't been calculate previously, 
        m <- solve(data, ...)                   ## it is calculated and returned
        x$setinverse(m)
        m
}

