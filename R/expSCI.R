#' Find simultaneous confidence for the successive pair-wise differences of exponential locations
#'
#' More detailed description
#'
#' @param data a real matrix
#' @param alpha real number between 0 and 1 called significance level
#'
#' @return numeric vector
#'
#' @examples
#' k=3;N=c(20,30,20);S=c(1,1,1);alpha=0.05
#' g=NULL
#' for(i in 1:k)
#' {
#'  g[[i]]=rexp(N[i],1/S[i])
#' }
#' data=g
#' expSCI(data, 0.05)
#' @export

expSCI<-function(data,alpha)
{
  f1<-function(data)
  {
    N=unlist(rbind(lapply(data,length)))
    M=unlist(rbind(lapply(data,mean)))
    #S=unlist(rbind(lapply(data,var)))
    X=unlist(rbind(lapply(data,min)))
    k=NROW(data)
    S=NULL
    for(i in 1:k)
    {
      S[i]=((N[i]-1)/N[i])*(M[i]-X[i])
    }
    V=NULL
    for(i in 1:k-1)
    {
      V[i]=((N[i]-1)/N[i]^3)*S[i]^2+((N[i+1]-1)/N[i+1]^3)*S[i+1]^2
    }
    T=NULL
    for(i in 1:k-1)
    {
      T[i]=(X[i+1]-X[i])/sqrt(V[i])
    }
    value=max(T)
    #value
    return(value)
  }
  f2<-function(s,N,k)
  {
    g=NULL
    for(i in 1:k)
    {
      g[[i]]=rexp(N[i],1/s[i])
    }
    data=g
    T1=f1(data)
    return(T1)
  }
  f3<-function(s,N,k,alpha)
  {
    x<-replicate(5000,f2(s,N,k))
    y<-sort(x,decreasing=FALSE)
    m=(1-alpha)*5000
    c<-y[m]
    return(c)
  }
  #f4<-function(s,N,k,alpha)
  #{
  #  z=replicate(10,f3(s,N,k,alpha))
  #  cri=mean(z)
  #  return(cri)
  #}
  data1<-lapply(data, function(col)col[!is.na(col)])
  N=unlist(rbind(lapply(data1,length)))
  M=unlist(rbind(lapply(data1,mean)))
  X=unlist(rbind(lapply(data1,min)))
  k=NROW(data1)
  S=NULL
  for(i in 1:k)
  {
    S[i]=((N[i]-1)/N[i])*(M[i]-X[i])
  }
  d<-f3(S,N,k,alpha)
  V=NULL
  for(i in 1:k-1)
  {
    V[i]=((N[i]-1)/N[i]^3)*S[i]^2+((N[i+1]-1)/N[i+1]^3)*S[i+1]^2
  }
  for(i in 1:k-1)
  {
    print("lower confidence limit for the groups")
    t1=sqrt(V[i])
    t2=(X[i+1]-X[i])
    t4=i+1;t5=i
    #t6=(t4,t5)
    t3=t2-d*t1
    l=i+1;u=i
    print(l);print(u)
    print(t3)
  }
}
