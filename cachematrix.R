g## Data Scientist Toolbox series, R Programming course, Week3, Assignment 2
##  Assignment = write the following R functions:
##      1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##      2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##         If the inverse has already been calculated (and the matrix has not changed), then the caschesolve should
##         retrieve the inverse from the cache.     
## 
##         To help understand the calculations for an inverst matrix go to http://www.mathsisfun.com/algebra/matrix-inverse.html


##  MakeCasheMatrix, creates a matrix where the rows and columns are undefined.  When the function is used, the 
##  end user can enter the desired dimentions of the matrix.

makeCacheMatrix <- function(x = matrix()) {
##      Use the same 4 steps outlined in the example assignement
##      1) Set the matrix
##      2) Get the matrix 
##      3) Set the value of inverse
##      4) Get the value of the inverse

    
##  SET the matrix        
    inv <- NULL             ##example shows name assigned to NULL value
    set <- function(y){
        x <<- y
        inv <<- NULL
    }    
##  GET the matrix
        get <- function() x
        
        


    
##  SET the value of the inverse
    setinv <- function(inverse) inv <<- inverse
    
##  GET the value of the inverse in the form of a list
    getinv <- function() inv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

    
## Creates a function that looks for existing solutions.  So, if a matrix inverse has already been solved, R
## will find that matrix in the local directory and show that value.  R then only performs the calculation of
## inverse, if it hasn't been solved (and stored aka cashed) previously.    

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Follow same steps as in example
    
        ## Has this been calculated and stored in cache?
        inv <- x$getinv()
        
        ## If it has been calculated, than return the calculated matrix
        if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
        }
        
        ## If it hasn't been calculated, then get matrix, calculate, and store it in cache. 
        m <- x$get()
        inv <-solve(m)  ##NOTE: originally had inverse instead of solve, referenced this code https://github.com/anrim/coursera-r-programming-assignment-2/blob/master/cachematrix.R to disover my error.
        x$setinv(inv)
        return(inv)
}

##TEST, use these to confirm cachematrix and casheSolve work as expected
## TM <- matrix(5:8, nrow = 2, ncol = 2)
## MM <- makeCacheMatrix(TM)
## cacheSolve(MM)
## returns a 2x2 matrix

## casheSolve(MM)
## returns "getting cached matrix" message
## returns 2x2 matrix as aboves
