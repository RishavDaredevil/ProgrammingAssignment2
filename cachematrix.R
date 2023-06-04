## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a object of 
## type -> makeCacheMatrix(i will henceforth refer to this as mcm) 
## what this essentially does is object that is created gets the methods/functions
## of the mcm with the arguments it was initialized with at the time of creation.
## This means that the mcm object within itself is not just a data storage variable
## but also can have other functions within itself that can do anything 
## with the objects present inside(not just limited to them) 
## the mcm objects environment. Here the mcm object essentially has 4 functions
## that help it interact with the 2 data storage variables present within it,
## changing it or calling as instructed. 
## ****I am still struggling with the use case of this of this assignment so if you 
## know about it do tell in the comments while you are grading. Thanks

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setter <- function(y){
        x <<- y
        inv <<- NULL
    }
    getter <- function()x
    setinv <- function(inverse){
        inv <<- inverse
    }
    getinv <- function()inv
    list(setter=setter,getter=getter,setinv=setinv,getinv=getinv)
}


## cacheSolve takes the mcm object that was 
## created and first tries to get the inverse out of it. If there is a inverse present
## within the mcm object(meaning it has already been calculated and hence stored within it)
## it will call that and return its value to the user. If the mcm object's inverse
## has nit been cached then it will calculate it and then store it inside the mcm object
## using setinv function and then return the calculated inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        print('Getting the cached inverse')
        return(inv)
    }
    inv <- solve(x$getter())
    x$setinv(inv)
    inv
}
