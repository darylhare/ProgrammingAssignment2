## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      #store inverse of matrix in i.
      #set as NULL for a new matrix 
      i<-NULL
      
      # function to set the stored matrix to y.  Reset i to NULL since 
      #inverse not yet computed
      set <- function(y) {
         x <<- y
         i <<- NULL
      }
      
      #function to return matrix
      get <- function() x
      
      #function to set inverse i to calculated value inv
      setinv <- function(inv) i <<- inv
      
      #function to return inverse
      getinv <- function() i
      
      #list of 4 functions created by makeCacheMatrix
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      #lookup inverse
      i<-x$getinv()
      
      #if it has been previously calculated, return cached value
      if(!is.null(i)) {
         message("getting cached inverse")
         return(i)
      }
   
      #Inverse not yet calculated. Calculate it with solve and store.
      data <- x$get()
      i<-solve(data)
      x$setinv(i)
      #return newly calculated inverse
      i
   }
