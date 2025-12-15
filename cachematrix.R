
## makeCacheMatrix function creates a special "matrix" object that can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) { 
   # Make it null to empty inverse matrix
    m <- NULL
    
    # Make a function which set the value of the matrix
    set <- function(y) {
        # assign the values to objects x and m in parent environment
        x <<- y 
		# Make an empty inverse matrix
        m <<- NULL
    }
    
    # Make a function to get value of matrix
    get <- function() x # lexical scoping
    
    # set value of inverse matrix
    setinverse <- function(inverse) m <<- inverse
    
    # get value of inverse matrix
    getinverse <- function() m
    
    # named elements in list allows use of $ to access functions
    list(set = set, # gives name set to set()
         get = get, # gives name get to get()
         setinverse = setinverse, # gives name setinverse to setinverse()
         getinverse = getinverse # gives name getinverse to getinverse()
         )
}


## cacheSolve() computes the inverse matrix returned by function makeCacheMatrix()
## If the inverse is already computed, cacheSolve will retrieve it and return it
## else, it'll compute the inverse again and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # returns to parent env if cached matrix is valid
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    # executes if false
    data <- x$get()
    m <- inverse(data, ....)
    x$setinverse(data)
    m
}
