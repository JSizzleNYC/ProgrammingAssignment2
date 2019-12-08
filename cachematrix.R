## These functions allow you to create a matrix and cache the inverse of the matrix to cut down 
## on calculation time.

## This function creates a special "Matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {                       ## define x with default mode of "matrix"
      inverseM <- NULL                                            ## initialize inverseM as NULL
      set <- function(y) {                                        ## define the set function to assign matrix.      
            x <<- y                                               ## assign value of matrix to parent environment matrix
            inverseM <<-  NULL                                    ## reset the inverseM variable to emptry matrix.
      }
      
      get <-function() x                                          ## define the get fucntion - returns value of the matrix argument
      setInverse <- function(inverse) inverseM <<- inverse       ## assigns value of inverseM in parent environment
      getInverse <- function() inverseM                           ## get the value of inverseM when called
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  #creates a list of the functions in order to refer via $ operator
}


## This function computes the inverse of a "special" matrix returned by makeCacheMatrix above. If the inverse has already been created, then it will return cached value. 
## if the inverse has not been calculated, then it will calculate the inverse. 

cacheSolve <- function(x, ...) {
      inverseM <- x$getInverse()                ## Gets the inverse of the "special" matrix input
      if(!is.null(inverseM)) {               ## checks whether the matrix is completely empty and if it is not, it will return the value provided by $getInverse.
            message("getting cached data")      ## prints a message saying "getting cached data"
            return(inverseM)                    ## reutrns Inverse Matrix
      }
      
      data <- x$get()                           ## it reaches this code if the InverseM is empty, in which case it first gets the input matrix.
      inverseM <- solve(data)                     ## applies the solve method in order to get the inverse.
      x$setInverse(inverseM)                    ## sets the inverse
      return(inverseM)                          ## returns the inverse. 
}
