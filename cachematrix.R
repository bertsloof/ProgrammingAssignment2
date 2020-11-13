makeCacheMatrix <- function(x= matrix()){
  m<-NULL
  v<-matrix(x,nrow=sqrt(length(x)),ncol=sqrt(length(x)))
  set <- function(w){
    v<<-w
    m<<-NULL
  }
  get <- function() v
  setsolve <- function(solve) m<<-solve
  getsolve <- function() m
  print(list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve))
}

cacheSolve <- function(v,...){
   m <-v$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-v$get()
  m<-solve(data,...)
  v$setsolve(m)
  m
}
