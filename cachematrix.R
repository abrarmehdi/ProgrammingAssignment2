## Matrix inversion is usually a costly computation.
## Two functions here calculates the Matrix inversion and stores it in cache so that it doesn't have to calculate the inversion again for the same matrix

## makeCacheMatrix creates a new type of data structure.
## Using this data structure we can store the matrix inversion value along with the original matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initializing the matrixInverse variable to NULL
        matrixInverse <- NULL
        
        ## setMatrix function
        setMatrix <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        
        ## getMatrix function - returns the matrix
        getMatrix <- function() x
        
        ## setInverse function - sets the matrix inversion value
        setInverse <- function(mi) matrixInverse <- mi
        
        ## getInverse function - returns the matrix inversion value
        getInverse <- function() matrixInverse
        
        ## list to store all the values and functions
        list(x = x, getInverse = getInverse, setInverse = setInverse, getMatrix = getMatrix, setMatrix = setMatrix)
}


## cacheSolve checks the new data structure for matrix inversion
## If it finds the matrix inversion then it will directly return the value without calculating
## If the value of matrix inversion is unavailable then it will calculate the matrix inversion value and stores it in the data structure

cacheSolve <- function(x, ...) {
        
        ## Retrieving the value of inverse
        matrixInverse <- x$getInverse()
        
        ## Checking if the value is NULL
        if(!is.null(matrixInverse)) {
                
                ## If the value is not null then displaying the message
                message("Inverse Already Calculated. Retrieving from Cache...")
                
                ## Returning the value of already calculated matrix inverse
                return(matrixInverse)
        }
        
        ## Retrieving the matrix
        data <- x$getMatrix()
        
        ## Since all the matrices in this example are invertible we are not checking for square invertiblness of a matrix
        ## Using the solve function we are calculating the matrix inversion
        matrixInverse <- solve(data)
        
        ## Upon calculation of matrix inversion, storing the value using setInverse function
        x$setInverse(matrixInverse)
        
        ## Returning the value of matrix inversion
        matrixInverse
}
