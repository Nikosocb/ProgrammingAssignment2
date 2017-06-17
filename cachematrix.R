
## Essentialy what is needed is to create an object which holds attributes of an other object
## so as to avoid computing them every time we need them, and waste time
## Here's the version where the "storing" object is a list, and the object is a matrix
## We create a null matrix in order to generalize the function of the...well, function
## and later on we fill it with the attributes of the object, here, a matrix.


## The function was created by modifying slightly the example presented in the course

makeCacheMatrix <- function(x = matrix()) {
    
    #We first create a null matrix, or a matrix placeholder. 
    inv_m <- NULL
    
    #then, we set up the set fuction which initiates the matrix
    set <- function(x=matrix)
    {
        x <<- y       ## here we correlate two variables from different environments
        inv_m <- NULL ## while keeping our abstract matrix
    }
    
    # now let's make a function that gets the input, nxn matrix
    get <- function() 
    {
        x
    }
    
    # the inverse is set here, via, again, the association of different environment variables
    setinverse <- function(inverse) 
    {
        inv_m <<- inverse
    }
    # and retrieved here
    getinverse <- function()
    {
        inv_m
    }
    
    #and finally, we put all this stuff into a list, as an input to the function that actually does the inverse job.
    #this is the result of the function, a list includining functions
    
    list(
     set = set,
     get = get,
     setinverse = setinverse, 
     getinverse = getinverse
        )
    
}


## Here we write a function that actually calculates the inverse of the input matrix and caches it. 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    #Get here the current matrix,
    inv_m <- x$getinverse()
    
    #and with an if statement, see if the getinverse() function has already been applied
    #i.e., if the matrix that the fuction returns is null
    
    if (!is.null(inv_m))
    {
        message("Here's your inverted matrix")
        inv_m
    }
    
    else ## if it is not ready, create it! 
    {
        # first, get it
        m <- x$get()
        #invert it
        inv_m <- solve(m,...) 
        #cache it
        x$setinverse(inv_m)
        #and retrieve it!
        inv_m
    }
}


## TEST AND OUTPUT

#create matrix and view it
#> x<-matrix(c(8,15,3,29),2,2)
#> x
#[,1] [,2]
#[1,]    8    3
#[2,]   15   29
#
# make the list
#> m_o <- makeCacheMatrix(x)
#
#invert it
#> cacheSolve(m_o)
#
#the result after the invertion
#[,1]        [,2]
#[1,]  0.1550802 -0.01604278
#[2,] -0.0802139  0.04278075
#
# and we call cacheSolve() again to ensure that the inverse is cached
#
#> cacheSolve(m_o)
#
#Here's your inverted matrix
#           [,1]        [,2]
#[1,]  0.1550802 -0.01604278
#[2,] -0.0802139  0.04278075
#
#it is!, that's what the message tells us