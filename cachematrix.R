## makeCacheMatrix this function creates a special type of Matrix
## that can temporarely store the inverse 

## This function is storing the original function and the inverse
## of the function if possible to calculate (if det is different than 0)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  getx<-as.matrix(x)
  ## I'm setting x as a matrix
  det(getx)  
    if (det(x)==0){
      print("No Inverse")
      break()
      ## I'm breaking the function if the det is 0
          }
  else {
    inv<-solve(getx)
    list(getx, inv)
  }    
}

    ## I calculate the inverse if det is different than 0
    ## I list the original matrix and the inverse
  
## This function will return the inverse of x if not returned
## already by the previous function
  

cacheSolve <- function(A) {
  x<-A
  inv<-NULL
  makeCacheMatrix(A)
          ## Return a matrix that is the inverse of A if not done
          ## if not already on makeCacheMatrix function
    if(is.null(inv)) {
      if(det(getx)==0){
        print("Does not have inverse")
        break
      }
      else{
        inv<-solve(getx)
      }
        return(inv)
    }
}

## Returns the inverse of the matrix
