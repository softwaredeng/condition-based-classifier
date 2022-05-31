EqualFreq <- function(x,n){
    nx <- length(x)
    nrepl <- floor(nx/n)
    nplus <- sample(1:n,nx - nrepl*n)
    nrep <- rep(nrepl,n)
    nrep[nplus] <- nrepl+1
    x[order(x)] <- rep(seq.int(n),nrep)
    x
}

indx_dup=function(rules,data)
{
 
 temp_trans1 <-  as(data, "transactions")
 lhs_list=lhs(rules)
 
 lhs_trans=as(lhs_list, "transactions")
 new_data=is.subset(lhs_trans,temp_trans1) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new_data[new_data]=1
 new_data=t(new_data)
 new_data=data.frame(new_data)
 dupindx0=duplicated(t(new_data))

 distinx=which(dupindx0==FALSE)
 rules=rules[distinx]
 return (rules)

 }
distinctRule=function(rules,data)
{
 trans1 <-  as(data, "transactions")
 lhs_list=lhs(rules)
 lhs_trans=as(lhs_list, "transactions")
 new1=is.subset(lhs_trans,trans1) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new1[new1]=1
 dupindx=duplicated((new1))
 dupindx=which(dupindx==TRUE)
 if(length(dupindx)>0)rules=rules[-dupindx]
 return(rules)
}

rules_to_data=function(rules,data)
{
 trans1 <-  as(data, "transactions")
 lhs_list=lhs(rules)
 lhs_trans=as(lhs_list, "transactions")
 new1=is.subset(lhs_trans,trans1) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new1[new1]=1

 #if the response y also match
 rhs_list=rhs(rules)
 rhs_trans=as(rhs_list, "transactions")
 new2=is.subset(rhs_trans,trans1) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new2[new2]=1

 #the following code will mask the up one i.e. don't consider Y's value
 new2[,]=1 # Can not consider Y's value, since if consider Y for the train, don't know how to set furture data
 #-------

 new3=new1*new2
 new1=new3


 new_data=new1

 new_data=data.frame(new_data)
 new_data=t(new_data)

 new_data=data.frame(new_data)

 new_data=cbind(new_data,data[,"Y"])
 new_data=data.frame(new_data)
 colnames(new_data)<-c(paste("X", 1:(dim(new_data)[2]-1), sep = ""),"Y")
 new_data[,"Y"]<-as.factor(new_data[,"Y"])
 rownames(new_data)=NULL

 return(new_data);
}
rules_to_testdata=function(rules,datatest)
{
 trans1 <-  as(datatest, "transactions")
 lhs_list=lhs(rules)
 lhs_trans=as(lhs_list, "transactions")
 new1=is.subset(lhs_trans,trans1) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new1[new1]=1

 new_data=new1

 new_data=data.frame(new_data)
 new_data=t(new_data)

 
 new_data=data.frame(new_data)

 colnames(new_data)<-c(paste("X", 1:(dim(new_data)[2]), sep = ""))
 rownames(new_data)=NULL

 return(new_data);
}

variable_fact=function(data)
{
 #factorize the variable
 data=data.frame(data)
 for(i3 in 1:ncol(data))
 {
 temp=i3
 data[,temp]=as.factor(data[,temp])
 }
return(data)
}

#from the data, see if the data confirm the rules
data_discretize=function(data)
{
data <- Discretize(Y ~., data = data)
}
