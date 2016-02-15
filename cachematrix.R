## makeCacheMatrix this function creates a special type of Matrix
## that can temporarely store the inverse 

## This function is storing the original function and the inverse
## of the function if possible to calculate (if det is different than 0)


makeCacheMatrix <- function(matrixOriginal = matrix()) {
     # I set where I will store the matrix inverse
       matrixI <- NULL
    
      # Funtion to set the value of the matrix to invert
        setMatrix <- function( y ) {
              matrixOriginal <<- y
              matrixI    <<- NULL
           }
      
        # Here this function will return the Original matrix
          getMatrix <- function () matrixOriginal
        
         # I create a function that sets the inverse of the matrix
            setInv <- function (inverse)  matrixI <<- inverse
          
        # Function to get the inverse of the matrix
             getInverse <- function ()  matrixI
        
                # Build the list to return
               list(set = setMatrix, get = getMatrix, setinverse = setInverse, getinverse = getInverse)
            }


## Using the special "vector" created abouve, caluclate the inverse
## of the given matrix. However, it first checks to see if we have
## already done this calculation, and if we have return the cached result.
## If there is no cached vaule, it inverts the matrix, stores the
## value and returns the inverse

  
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
         
    ## Here I am checking if the inverse is already on the cache
    ## matrix and it's getting it from there
    
          inversematrix <- x$getinverse()
          if ( !is.null(inversematrix) ) {
                message("Using cached value of inverse")
                return(inversematrix)
          }
    ## If it's not there then it will calculate by using the
    ##function solve
      else{
        matrix <- x$get()
        inversematrix <- solve(matrix)
        x$setinverse(inversematrix)
        
        inversematrix
        
      } 
              }
