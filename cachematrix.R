#the makeCacheMatrix function implements a list with a set of functions to
#get/set either a matrix "x" or its inverse "i_x". When the function set a matrix,
#it is saved in cache.

makeCacheMatrix<-function(x=matrix()){
    #this function implements a set of functions contained in a list to
    #writes/reads a matrix "x" and its inverse "i_x". Both matrices are stored in cache.
    
    i_x<-NULL
    #if the inverse of "x" hasn't be computed yet, then "i_x" is NULL
    
    set<-function(y=matrix()){
        #this function receives a matrix "y" and stores it in cache. Also, since this is a new
        #matrix, its inverse "i_x" is reset to NULL
        
        x<<-y
        i_x<<-NULL
    }
    
    get<-function() x
    #returns the matrix "x" which was previously stored
    
    setinverse<-function(inverse=matrix()) i_x<<-inverse
    #receives the variable "inverse" which represents the result of inverting matrix "x"
    #and stores it in cache ("i_x")
    
    getinverse<-function() i_x
    #returns the last inverse matrix "i_x" stored in cache
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    #return a list containing all previously implemented functions
    
}




#this function computes the inverse of the matrix saved by makeCacheMatrix function
#If the inverse was already calculated and the matrix didn't changed,
#then cacheSolve returns the inverse from the cache

cacheSolve<-function(x=matrix(),...){
    
    inverse<-x$getinverse()
    #get the previously calculated inverse matrix "i_x" and assign it to the variable "inverse"
    
    if(!is.null(inverse)){
        #if the inverse matrix was previously calculated and the value of the original matrix "x"
        #hasn't changed, then the function returns the inverse matrix stored in cache.
        
        message("getting cached data for inverse matrix")
        return(inverse)
    }
    
    #if there's no inverse matrix stored in cache, then the function calculate the new inverse
    #matrix and stores it in cache.
    
    data<-x$get()   #gets the matrix from cache
    inverse<-solve(data)  #calculates the inverse of matrix "data" and stores it in variable "inverse"
    x$setinverse(inverse) #saves the recently calculated inverse matrix in cache
    inverse               #returns the inverse matrix
}

