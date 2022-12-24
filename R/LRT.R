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
#'  g[[i]]=rexp(N[i],1/sqrt(S[i]))
#' }
#' data=g
#' LRT(data, 0.05)
#' @export
LRT<-function(data,alpha)
{
  f1<-function(t)
  {
    a<-t
    b<-length(t)
    y=NULL
    for(i in 1:b)
    {
      y[i]=min(a[i:b])
    }
    return(y)
  }
  f2=function(data)
  {
    g=data
    xbar0=unlist(rbind(lapply(data,min)))
    N=unlist(rbind(lapply(data,length)))
    k=length(N)
    xbar1=f1(xbar0)
    X0=min(xbar1)
    S1=0
    p=xbar1
    for(i in 1:k)
    {
      a1=g[[i]]
      b=length(a1)
      s1=0
      for(j in 1:b)
      {
        s1=s1+(a1[j]-p[i])
      }
      S1[i]=s1/b
    }
    S0=0
    for(i in 1:k)
    {
      a1=g[[i]]
      b=length(a1)
      s0=0
      for(j in 1:b)
      {
        s0=s0+(a1[j]-X0)
      }
      S0[i]=s0/b
    }
    l=1
    for(i in 1:k)
    {
      l=l*(S1[i]/S0[i])^(N[i])
    }
    return(l)
  }
  f3<-function(S,N,k)
  {
    g=NULL
    for(i in 1:k)
    {
      g[[i]]=rexp(N[i],rate=1/S[i])
    }
    data=g
    l=f2(data)
    return(l)
  }
  f4<-function(S,N,k,alpha)
  {
    x<-replicate(5000,f3(S,N,k))
    y<-sort(x,decreasing=FALSE)
    m=alpha*5000
    c<-y[m]
    return(c)
  }
  #f5<-function(S,N,k,alpha)
  #{
  #  z=replicate(10,f4(S,N,k,alpha))
  #  cri=mean(z)
  #  return(cri)
  #}
  data1<-lapply(data, function(col)col[!is.na(col)])
  M=unlist(rbind(lapply(data1,mean)))
  xbar0=unlist(rbind(lapply(data1,min)))
  N=unlist(rbind(lapply(data1,length)))
  k=NROW(data1)
  V=NULL
  for(i in 1:k)
  {
    V[i]=(N[i]/(N[i]-1))*(M[i]-xbar0[i])
  }
  set.seed(39)
  statistic_value<-f2(data1)
  c<-f4(V,N,k,alpha)
  result<-c(statistic_value,c)
  print("test statistic value and critical value")
  print(result)
  r1=result[1];r2=result[2]
  if(r1<r2)
  {
    print("Null hypothesis is rejected")
  }
  else
  {
    print("Null hypothesis is not rejected")
  }
}
