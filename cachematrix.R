# The following functions invert a given matrix and cache the result.
# If the respective matrix has been inverted before, the same set of functions 
# retrieves the result from the cache instead of computing it again. 

# makeCacheMatrix defines the environment in which the non-inverted Matrix is 
# placed. This environment contains:
#   1. x: variable for non-inverted matrix
#   2. m: variable for inverted matrix
#   3. set: function that prefers non-inverted matrix (x)
#   4. get: function that returns non-inverted matrix (x)
#   5. setinverse: function that prefers inverted matrix (m)
#   6. getinverse: function, that returns inverted matrix (m)

makeCacheMatrix <- function(x) {
        
        # First, we create an empty variable 
        m <- NULL
        
        # Then we create the first function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # the second function
        get <- function() {
                x
        }
        
        # the third function
        setinverse <- function(solve) {
                m <<- solve
        }
        
        # the fourth function
        getinverse <- function () m 
        
        # Finally, we list all elements that are defined in this environment
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

# To test the function, we use a test matrix
test <- matrix(c(2, 5, 1, 3), 2, 2)

# The test run of the function returns an inverted matrix, 
# which will now be stored in the cache
makeCacheMatrix_test <- makeCacheMatrix(test)

# We now create a second function called cacheSolve that uses makeCacheMatrix to 
# either retrieve the data from the cache or compute the inverse of the matrix 
cacheSolve <- function(x, ...){
        
        # First, the function extracts the elements it finds for m by using the 
        # getinverse function
        use_m <- x$getinverse()
        
        # If the inverse is in the cache, the function returns the value, 
        # otherwise it returns NULL 
        if(!is.null(use_m)){
                message("Matrix has been inverted before. Getting cached data.")
                return (use_m)
        }
        
        # If the returned value is NULL, the non-inverted matrix is retrieved
        use_x <- x$get()
        
        # Then the matrix is inverted via the solve function
        make_m <- solve(use_x, ...)
        
        x$setinverse(make_m)
        
        message("Matrix has not been inverted before. Inverting matrix and saving to cache.")
        
        # Finally, the function returns the inverted matrix
        make_m
}
# First test run: the matrix has not yet been inverted, so it will now be inverted
# and stored in a variable
cacheSolve(x=makeCacheMatrix_test)

# Second test run: the matrix has been inverted before, so the inverted matrix will
# be retrieved from the cache
cacheSolve(x=makeCacheMatrix_test)
