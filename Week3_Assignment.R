#the overall pourpose is to save the computing power. Only if
#the inverse of the matrix has not existed already,
#then computing the matrix


#for a given matrix, creat a list of 4 elements
makeCacheMatrix<-function(x=matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setInv<-function(si) inv<<-si
    getInv<-function() inv
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}

#computing inverse if the inverse of a matrix doesn't exist
cacheSolve<-function(x,...) {
    inv<-x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matr<-x$get()
    inv<-solve(matr,...)
    x$setInv(inv)
    inv
}

#testing
q<-matrix(1:4,2,2)
qq<-makeCacheMatrix(q)
qq$get()
qq$getInv()
qqq<-cacheSolve(qq)

