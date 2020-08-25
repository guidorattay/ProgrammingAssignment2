#Caching the Inverse of a Matrix

#my example inversible matrix
#A<-matrix(c(2,5,1,3),nrow=2,ncol=2)

#inverse
#A_1<-solve(A)

makeCacheMatrix<- function(x = matrix()) {
    #This function creates a special "matrix" object that can cache its inverse.
    m <- NULL
    
    set <- function(y) {
        x <<- y #variable in cache for the matrix
        m <<- NULL #variable in cache for the inverse matrix
    }
    
    get <- function() x
    
    setsv <- function(solve) m <<- solve
    
    getsv <- function() m
    
    list(set = set, get = get, setsv = setsv, getsv = getsv)
}

#C<-makeCacheMatrix(A)
#C is the Special Matrix from A

cacheSolve <- function(x, ...) {
    #This function computes the inverse of the special "matrix" returned 
    #by makeCacheMatrix.
    
    #If the inverse has already been calculated (and the matrix has not changed),
    #then the cachesolve should retrieve the inverse from the cache.
    m <- x$getsv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setsv(m)
    m
}  

#cacheSolve(C)
