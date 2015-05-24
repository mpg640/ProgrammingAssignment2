## These functions enable caching for matrix inversion in order to spare
## computation costs

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        #Base off similar functions in order to develop caching
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        x <- list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #Return vector type list of functions
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed) then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #if the inverse was already cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }                                 #exit program without executing sub. code
        data <- x$get()                   #otherwise, put the data in 'data'
        m <- solve(data, ...)             #compute the inverse of the data
        x$setinverse(m)                   # call function to cache the inverse
        m                                 # return the inverse
}

cachematrix <- function(x,...){
        makeCacheMatrix(x)
        cacheSolve(x)
}
