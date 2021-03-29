ll <- function(x,para,data){
  if(x==para[1]){
    ll=data
    n=1:2
  }else{
    if(x==para[2]){
      ll=1:40
      n=1
    }else{n=NULL
    ll=NULL}
  }
  return(list(n,ll))
}