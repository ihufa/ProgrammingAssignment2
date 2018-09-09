
## makeCacheMatrix stores a matrix and the inverted matrix, as a list of getter and setter functions for both the original and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              ## Create empty matrix for inverse, inside funtion environment (makeCacheMatrix())
  set <- function(y) {                     ## Setter function, sets the matrix x to a value and nulls the 
    x <<- y                                ## inverse in case it was cached. This in parent environment (makeCacheMatrix())                    
    inv <<- NULL
  }
  get <- function() x                      ## Getter function prints x, no x in get() environment so R looks in parent environment and finds x in makeCacheMatrix()
  setinverse <- function(inverse) inv <<- inverse  ## Setter function sets the inverse in parent frame, aka. makeCacheMatrix(),
                                           ## the value is obtained from cacheSolve()
  getinverse <- function() inv             ## Prints the inverse matrix if defined, if not 'NULL'
  list(set = set, get = get,               ## Lists the setters and getters in a named format that cacheSolve can subset easily
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve uses list from makeCacheMatrix() to find inverse matrix. If none is cached it computes and stores the new inverse in MakeCacheMatrix()

cacheSolve <- function(x, ...) {           
  
    inv <- x$getinverse()                  ## Retrieves inverse to 'inv'
    if(!is.null(inv)) {                    ## Checks if anything is cached in 'inv', if it is, it returns the message and the inverse matrix
      message("getting cached data")       
      return(inv)
    }
    data <- x$get()                       ## If it isn't, the matrix is retrieved into 'data'
    inv <- solve(data, ...)               ## The inverse matrix is computed with solve() as 'inv'
    x$setinverse(inv)                     ## The setter function inside makeCacheMatrix() is called with argument 'inv' to store the inverse
    inv                                   ## Returns a matrix that is the inverse of 'x'
  }        

