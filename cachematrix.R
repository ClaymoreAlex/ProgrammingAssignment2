
makeCacheMatrix <- function (x, ...) {
  
    ## This function creates a special matrix object and cache its inverse
    m <- NULL
  
    ## this changes vector stored in main function 
    set <- function(y) {
        ## substitutes the vector x with input y the main function 
        x <<- y        
        ## resets the matrix m to null
        m <<- NULL
    }
  
      ## this returns x stored in the main function to get data for cacheSolve
      get <- function() x  
  
      ## store the value of m into the main function makeCacheMatrix(matrix) 
      ## and return it to getmatrix
      setmatrix <- function(matrix) m <<- matrix
      getmatrix <- function() m
  
      ## stores the four functions for makeCacheMatrix
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## calculates the inverse vector created in makeCacheMatrix 
cacheSolve <- function (x, ...) {
  
    ## checks to see if inverse has been calculated and returns proir inverse
    m <- x$getmatrix()
  
    ## else, calculates inverse matrix and
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
  
    ## gets the vector stored in makematrix
    data <- x$get()
  
    ## solve generates an inverse matrix
    m <- solve(data, ...)
  
    ## puts inverse in cache using setmatrix
    x$setmatrix(m)
    m
}

