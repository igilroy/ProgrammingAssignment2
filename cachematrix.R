makeCacheMatrix<-function(x=numeric()) {
        
        ## This function creates a special "matrix" object that can cache its inverse.

        ## initialise the inverse matrix so that all the values are NA
        im<-matrix(data=NA,nrow=dim(x)[1],ncol=dim(x)[2])

        ## function to get input matrix
        get <- function() x
        
        ## function to create inverse matrix
        setInverse <- function(solve) im <<- solve
        
        ## function to retrieve inverse matrix
        getInverse <- function() im
        
        ## create special "vector" to be returned
        list(get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
        ## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse 
        ## from the cache.

        ## load current cached value of inverse matrix
        im <- x$getInverse()
        
        ## is it empty (whicn means we haven't cached teh inverse yet)?
        if(!anyNA(im,recursive = TRUE)) {
                ## let user know we are using cached value
                message("getting cached data")
                return(im)
        }
        ## otherwise extract teh matrix we are going to invert and apply teh "solve" function
        data <- x$get()
        im <- solve(data, ...)
        
        ## cache the inverse matrix
        x$setInverse(im)

        im
}

