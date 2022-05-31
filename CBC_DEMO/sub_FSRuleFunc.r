
FSRuleF=function(dataTrain,dataTest,y,FSRule)
{
 trans1 <-  as(dataTrain[,1:(ncol(dataTrain)-1)], "transactions")
 lhs_list=lhs(FSRule);
 lhs_trans=as(lhs_list, "transactions")
 new1=is.subset(lhs_trans,trans1 ) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new1[new1]=1; new1=t(new1)
 r=rhs(FSRule); class = labels(r); class = class$elements; 
 class = gsub("\\{Y=","",class); class = gsub("\\}","",class)
 
 ypred = NULL #get the default class by the instances that not covered
 for(dI in 1:nrow(new1))
{
 ix = which(new1[dI,]==1)
 if(length(ix)==0)ypred = c(ypred,as.character(unlist(dataTrain[dI,ncol(dataTrain)])))
}
 clsDefault = names(which.max(table(ypred)))


 trans1 <-  as(dataTest[,1:ncol(dataTest)], "transactions")
 new1=is.subset(lhs_trans,trans1 ) #if the rule's left hand subset of later transaction, if yes, then it satsify that rule 
 new1[new1]=1; new1=t(new1)
 class = labels(r); class = class$elements; 
 class = gsub("\\{Y=","",class); class = gsub("\\}","",class) 
 
 ypred = NULL
 for(dI in 1:nrow(new1))
{
 ix = which(new1[dI,]==1)
 if(length(ix)==0){ypred = c(ypred,clsDefault);}
 if(length(ix)>0) {ypred = c(ypred,class[ix[1]]);}
}

 err=100*(1-sum(ypred==y)/length(y))

 return(err)
}

