##The function makeCacheMatrix takes a matrix as an input
##set the value of matrix, gets the value of the matrix.
##sets the value of invert and gets the value of the invert


##<<- is a special operator used to assign value to a variable in an environment
##which is different from the current/parent environment.

#creating the function



makeCacheMatrix <- function(x = matrix()){ ##the argument is set to matrix mode by default
  
  
  invMatrix <- NULL      ##variable which will hold the inversion of the i/p matrix.
  
  set <- function(y){    ## defines a set function which will
    ## set a new value to our matrix in the parent environment.
    x <<- y
    
    invMatrix <- NULL   ## if new matrix found, reset the invert matrix to NULL.
    
  }
  get <- function() x     ## get function - returns value of the matrix argument.  
  
  
  
  setInverse <- function(inverse) invMatrix <<- inverse  ##assigns value to invMatrix in parent environment.
  getInverse <- function() invMatrix       ## gets the value of our inverted matrix.
  
  list(set = set, get= get, setInverse = setInverse, getInverse = getInverse) ##need this while referring
                                                                              ##to the func using $ sign 
  
}






##The second part is about creating a function which should be able to read
##the inverse of the matrix from the cache.



##cacheSolve takes o/p of makeCacheMatrix function as its i/p
##checks if value is present in invertmatrix....
##if invert matrix is empty, it gets the original matrix data and sets the 
##inverse matrix usingthe solve function.
##else it will pring "Getting Cached Inverse Matrix."



cacheSolve <- function(x, ...){     ##gets the inverse matrix from the original 
  
  invMatrix <- x$getInverse()
  
  if(!is.null(invMatrix)){  ##If inverse matrix is not empty
    
    message("Getting Cahced Inverse Matrix")
    
    return(invMatrix)     ##returns the inverse Matrix.
    
  }
  
  ##if the inverse matrix is empty
  
  matrix_data <- x$get()      ##gets the original matrix data
  
  invMatrix <- solve(matrix_data, ...)  ##solve function gives inverse of the matrix
  
  x$setInverse(invMatrix)               ##sets the inverse matrix
  
  return(invMatrix)                     ##returns the inverse matrix
  
}

