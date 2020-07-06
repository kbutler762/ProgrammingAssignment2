## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#require(matlib)

myMatrix <- makeCacheMatrix(matrix(c(2, -4, 150, 80, 94, 2.3, 7, 21, 283), nrow = 3, byrow = TRUE))

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<-y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<-inverse
        getinverse <- function() i
        list(set=set, get=get, 
             setinverse = setinverse, 
             getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cahced inverse matrix data")
                return(i)
        }
        data <- x$get()
        i <-  solve(data,...)
        x$setinverse(i)
        i
        
        ## Return a matrix that is the inverse of 'x'
}
