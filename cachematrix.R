## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   # assign NULL into inv variable
    inv <- NULL
   #set() assign the x matrix to the x variable in the environment
    set <- function(y) {
         #look up variables in the next environment (<<- vs. <-) 
         #x <- y  #Let's break it - why does it break when there is a single less than symbol? 
         #This is due to Lexical scoping. The "set" function environment is different than 
         #the makeCacheMatrix()
        x <<- y
        inv <<- NULL
    }
    #get() Give access to the matrix which is stored in the makeCacheMatrix environment
    get <- function() x
    #setInverse() assign the inverse matrix to the inv variable
    setinverse <- function(inverse) inv <<- inverse
    #getInverse() give access to the inverted matrix
    getinverse <- function() inv
    #The list is an object that contains the behaviors defined
    #in the functions above
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Assume that the matrix can be inverted
    inv <- x$getinverse()
    #Checkif inverse is already cached or not 
    if(!is.null(inv)) {
        message("getting cached data.")
        #cached matrix return without processing
        return(inv)
    }
    #Now, inverse of matrix is not cached
    data <- x$get()
    #Call a function to inverse the matrix
    inv <- solve(data)
    #Cache the data i.e. inverse matrix
    x$setinverse(inv)
    #Return the inverse matrix
    inv
}
