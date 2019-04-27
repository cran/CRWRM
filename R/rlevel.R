rlevel=function(fit,var,oldrf,newrf,cl=NA){
  if(is.null(fit$call)){
    type=as.character(fit$glmnet.fit$call)
  }else{
    type=as.character(fit$call)
  }
  
  w=paste(var,newrf,sep="")
  
  if(type[1]=="glm"|type[1]=="lm"){
    cv=vcov(fit)
    fit_sum=summary(fit)
    c=fit_sum$coefficients
    c=as.data.frame(c)
    
    for(i in 1:length(oldrf)){
      x=c[substr(rownames(c),1,nchar(var[i]))==var[i],]
      y=c[substr(rownames(c),1,nchar(var[i]))!=var[i],]
      
      if(nrow(x)==1){
        y[1,1]=y[1,1]+x[1,1]
        x[1,1]=-x[1,1]
        x[1,3]=-x[1,3]
        rownames(x)=paste(var[i],oldrf[i],sep="")
      }else{
        n=which(substr(rownames(x),nchar(var[i])+1,nchar(rownames(x)))==newrf[i])
        rownames(x)[n]=paste(var[i],oldrf[i],sep="")
        y[1,1]=y[1,1]+x[n,1]
        x[n,1]=-x[n,1]
        x[n,3]=-x[n,3]
        for(j in 1:nrow(x)){
          if(j==n){
            x[j,1]=x[j,1]
          }else{
            x[j,1]=x[j,1]+x[n,1]
            x[j,2]=sqrt((x[j,2])^2+(x[n,2])^2-2*cv[which(rownames(cv)==rownames(x[j,])),which(colnames(cv)==paste(var[i],newrf[i],sep=""))])
            x[j,3]=x[j,1]/x[j,2]
            if(type[1]=="glm"){
              x[j,4]=2*pnorm(-abs(x[j,3]))
            }else{
              x[j,4]=2*pt(abs(x[j,3]),fit$df.residual,lower.tail = FALSE)
            }
          }
        }
      }
      
      if(i==1){
        e=which(colnames(cv)==paste(var[i],newrf[i],sep=""))
        vr=cv[1,1]+cv[which(rownames(cv)==paste(var[i],newrf[i],sep="")),which(colnames(cv)==paste(var[i],newrf[i],sep=""))]+
          2*cv[which(rownames(cv)==w[i]),1]
        
      }else{
        
        vr=vr+cv[which(rownames(cv)==paste(var[i],newrf[i],sep="")),which(colnames(cv)==paste(var[i],newrf[i],sep=""))]+
          2*sum(cv[which(rownames(cv)==w[i]),c(1,e)])
        e=c(e,which(colnames(cv)==paste(var[i],newrf[i],sep="")))
      }
      
      c=rbind(y,x)
    }
    c[1,2]=sqrt(vr)
    c[1,3]=c[1,1]/c[1,2]
    if(type[1]=="glm"){
      c[1,4]=2*pnorm(-abs(c[1,3]))
    }else{
      c[1,4]=2*pt(abs(c[1,3]),fit$df.residual,lower.tail = FALSE)
    }
    return(c)
  }
  if(type[1]=="glmnet"){
    if(is.na(cl)){
      cl=1
    }
    if(is.null(fit$call)){
      c=coef(fit$glmnet.fit)[,cl]
    }else{
      c=coef(fit)[,cl]
    }
    c=as.data.frame(c)
    c$p=1
    for(i in 1:length(oldrf)){
      x=c[substr(rownames(c),1,nchar(var[i]))==var[i],]
      y=c[substr(rownames(c),1,nchar(var[i]))!=var[i],]
      if(nrow(x)==1){
        y[1,1]=y[1,1]+x[1,1]
        x[1,1]=-x[1,1]
        rownames(x)=paste(var[i],oldrf[i],sep="")
      }else{
        n=which(substr(rownames(x),nchar(var[i])+1,nchar(rownames(x)))==newrf[i])
        rownames(x)[n]=paste(var[i],oldrf[i],sep="")
        y[1,1]=y[1,1]+x[n,1]
        x[n,1]=-x[n,1]
        for(j in 1:nrow(x)){
          if(j==n){
            x[j,1]=x[j,1]
          }else{
            x[j,1]=x[j,1]+x[n,1]
          }
        }
      }
      
      c=rbind(y,x)
    }
    name=rownames(c)
    c=c[,1]
    names(c)=name
    return(c)
  }
  
}


#rlevel(fit=a,var=var,oldrf=oldrf,newrf=newrf)
#var=c("c_edu","hispanic","race")
#oldrf=c("<HS","Hispanic","White")
#newrf=c("HS","Not Hispanic","Black")

