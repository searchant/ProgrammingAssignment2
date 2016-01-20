
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
# test function measure time difference directly compute / read from cache

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
       
	# set inverse of the matrix
	  setinverse <- function(inverse) {
			 m <<- solve }
	  
	  #Note you may write own inverse algorthim without using R embeded solve function
	# get the valuse of inverse matarix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve check whether inverse matrix been caculated, if not it caculate the inverser
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  m <- x$getinverse()
	# check whether inverse been caculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	# caculate inverse
        m <- solve(data, ...)
	# set inverse in cache
        x$setinverse(m)
        m

}

 # test function to measure time to compute inverse matrix
timeMeasure <- function(mdata){
	
	temp = makeCacheMatrix(mdata)

	start.time = Sys.time()
      cacheSolve(temp)
	duration = Sys.time() - start.time
	print(duration)

	# 2nd time run only look inverse in cache
	start.time = Sys.time()
      cacheSolve(temp)
	duration = Sys.time() - start.time
	print(duration)

}


#test command in R console:
#  source("cachematrix.R")
#  set.seed(100)
#  mdata = matrix(rnorm(1000000), nrow=1000, ncol = 1000)
#  timeMeasure(mdata)


#console printout
#	Time difference of 1.0517 secs
#	getting cached data
#	Time difference of 0.0009999275 secs

