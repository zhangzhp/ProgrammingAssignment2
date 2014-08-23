## Matrix inversion can be computationally costly. The following two 
## functions take advantage of the scoping rules of the R language 
## to carry out matrix inversion. If the contents of a matrix are not
## changing, they first look up in the cache rather than recomputed. 


## The function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <-NULL
        set<- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinvM <- function(solve) invM <<- solve
        getinvM <- function() invM
        list(set = set, get = get,
             setinvM = setinvM,
             getinvM = getinvM
             )

}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache 
## via the setinvM function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinvM()
        if(!is.null(invM)){
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM<-solve(data,...)
        x$setinvM(invM)
        invM
}


