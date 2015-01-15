# TEST EXAMPLE:
# x <- c(7,0,-3,2,3,4,1,-1,-2)
# dim(x) <- c(3,3)
# mat <- makeCacheMatrix(x) 
# cacheSolve(mat)
# cacheSolve(mat)

#-------------------------------------------------
#-------------------------------------------------
# function makeCacheMatrix. makes a 'special' matrix
# that can be called by cacheSolve to get inverse, either
# by direct computation or retrieving from cache.
# it essentially returns a list of the functions defined within
# the function.
# x : input matrix

makeCacheMatrix <- function(x = matrix()){
    
    # set inverse to NULL initially
    # print and store some environment info
    inv <- NULL
    
    # function setfunction. this never actually gets called
    # but maintain to be analogous with vector example 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # function getfunction
    # returns value of x  
    get <- function(){ x }
    
    # function setinverse. stores a copy of inverse matrix in cache
    # inverse : input argurment, is the inverse matrix
    # inv     : copy of inverse matrix stored in cache
    # <<- operator causes to search parent environment for existing definition
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    # function getinverse. returns copy of inverse matrix 
    getinverse <- function(){ inv } 
    
    
    # the function makeCacheMatrix returns a list containing the
    # functions defined.  
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
#-------------------------------------------------
#-------------------------------------------------

#-------------------------------------------------
#-------------------------------------------------
# function Cachesolve. If inverse of matrix is stored in cache
# returns cached data, if not it calculates and stores solution
# in cache.
# Essentially selects the relevent functions generated from
# makeCacheMatrix
# NOTE: can scrap ... in cacheSolve() and solve(), wont change result of 
# of function in current form

cacheSolve <- function(x, ...) {
    
    # call getinverse function by retrieving the
    # getinverse element of the list returned by makeCacheMatrix.
    # getinverse will return a NULL value if not stored in cache
    # If set in cache, cacheSolve returns cached value rather than calculate
    
    inv <- x$getinverse()
    if( !is.null(inv) ) {
        message("getting cached data")
        return(inv)
    }
    
    # if inv is NULL (ie not cached), retrieve the value of matrix using get() 
    # calling the get element of the list returned by makeCacheMatrix
    
    data <- x$get()
    
    # calculate inverse matrix using solve()
    
    inv <- solve(data, ...) 
    
    # call the setinverse element of x, providing inverse matrix
    # as the argument. The inverse matrix is now set in the cache
    
    x$setinverse(inv)
    
    #finally, return inverse of matrix if providing the answer
    #by calculation
    
    inv
}
#-------------------------------------------------
#-------------------------------------------------
