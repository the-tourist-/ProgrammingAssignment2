## Put comments here that give an overall description of what your
## functions do

## The following will create a nxn matrix and seed it with random 
## normal values with a Mean of 0 and SD of 1.  It is to assist testing

randomMatrix <- function(n) {
    matrix(rnorm(n^2), n, n)
}

## The following will create a list of functions that allow for a 
## vector to be set or get, and also the inverse of that matrix to
## be set or get.  It also contains an internal memory that will
## cache the inverse matrix so that it doesn't need to be recalculated
## if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
                                           
    mem <- NULL
    set <- function(y) {
        x <<- y
        mem <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseX) mem <<- inverseX
    getInverseMatrix <- function() mem
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function will first check in the memory of the CacheMatrix to see if
## the inverse has already been returned and return that if so.  If not it
## will calculate the inverse and store it in the CacheMatrix

cacheSolve <- function(x, ...) {
    mem <- x$getInverseMatrix()
    if(!is.null(mem)) {
        message("getting cached data")
        return(mem)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}

# This is a quick test that uses a timer on a large matrix to see if the cache is being used

timeCacheSolveTest  <- function() {
    m <- randomMatrix(1000)
    cm <- makeCacheMatrix(m)
    
    t1 = system.time (mi1<-cacheSolve(cm))
    t2 = system.time (mi2<-cacheSolve(cm))
    
    list(isInverse=round(sum(as.vector(m %*% mi2)), 5)==1000,timer1=t1,timer2=t2)
}
