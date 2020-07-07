## myMatrix is a variable that stores the 3x3 square matrix of values listed
## makeCacheMatrix initializes the variables that will be used to calculate the
## inverse of myMatrix.
##cacheSolve takes the inputs from the parent environment and finds the inverse of myMatrix



## myMatrix is a variables that stores the matrix input that will be used later 


myMatrix <- makeCacheMatrix(matrix(c(2, -4, 150, 80, 94, 2.3, 7, 21, 283), nrow = 3, byrow = TRUE))

##makeCacheMatrix is the variable that stores a function that initializes object x and i. 
##makeCacheMatrix also defines the functions set, get, setinverse, and getinverse

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


## cacheSolve is the variable assigned to the function that populates the matrix and
## executes the inverse function for the matrix

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached inverse matrix data")
                return(i)
        }
        data <- x$get()
        i <-  solve(data,...)
        x$setinverse(i)
        return(i)
        
        ## i is a matrix that is the inverse of 'x' aka myMatrix
}
