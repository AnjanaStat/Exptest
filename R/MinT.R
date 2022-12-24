#' Find test statistic value from a given data and corresponding critical value
#'
#' More detailed description
#'
#' @param data a real matrix
#' @param alpha real number between 0 and 1 called significance level
#'
#' @return numeric vector
#'
#' @examples
#' k=3;N=c(20,30,20);S=c(1,1,1)
#' g=NULL
#' for(i in 1:k)
#' {
#'  g[[i]]=rexp(N[i],1/S[i])
#' }
#' data=g
#' MinT(data, 0.05)
#' @export


MinT<-function(data,alpha)
{
  f1<-function(data)
  {
    N=unlist(rbind(lapply(data,length)))
    M=unlist(rbind(lapply(data,mean)))
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
    value=min(T)
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
  #  z=replicate(1,f3(s,N,k,alpha))
  #  cri=mean(z)
  #  return(cri)
  #}
  data1<-lapply(data, function(col)col[!is.na(col)])
  N=unlist(rbind(lapply(data1,length)))
  M=unlist(rbind(lapply(data1,mean)))
  X=unlist(rbind(lapply(data1,min)))
  k=NROW(data1)
  s=NULL
  for(i in 1:k)
  {
    s[i]=((N[i]-1)/N[i])*(M[i]-X[i])
  }
  set.seed(49)
  statistic_value<-f1(data1)
  crit_value<-f3(s,N,k,alpha)
  result<-c(statistic_value, crit_value)
  print("test statistic value and critical value")
  print(result)
  r1=result[1];r2=result[2]
  if(r1>r2)
  {
    print("Null hypothesis is rejected")
  }
  else
  {
    print("Null hypothesis is not rejected")
  }
}

