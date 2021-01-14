#These functions take a matrix, store it's components in the 
#parent function and then solves the inverse of the matrix 

#MakeCacheMatrix stores the matrix that is passed through it. 
#First it declares that m(the matrix) is declared as NULL 
#Following this, the set function is called and x is assigned the argument 
#in the parent function
#m is set to NULL once again
#get takes the x from the parent enviornment
#set inverse takes the stored m in the parent enviornment and finds the inverse
#getinverse takes the m and displays it. 
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}
#cacheSolve takes the getinverse function from the makeCacheMatrix function first
#it stores the inverse of x by passing through the getinverse argument
#following this, m is checked. If it is not null, the argument returns an already 
#stored inverse matrix
#if it is null, the matrix solution is obtained by using x$get() to get x and store it in data
#solve is then used to store the inverse of data in m 
#x$setinverse(m) sets m as the cached data in the parent function
#m just displays returns the m value
cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
