
C45Tree=function(data,datatest,y)
{
 y=unlist(y)
 nLeaf=NULL;nRule=NULL;err =NULL;
 T<- J48(Y ~ ., data = data,na.action=na.pass,control =Weka_control(B = TRUE)) 
 #T<- J48(Y ~ ., data = data,na.action=na.pass,control =Weka_control(M = 1)) 
 ypred=predict(T, datatest); 
 err = rbind(err,  100*(1-sum(ypred==y)/length(y)))
 result=list(tree=T,ypred=ypred,err=err)
}


get_CBA_size=function(tree)
{
#extract the number of leaves for the tree
temp1=tree[1]
temp2=unclass(temp1)
temp3=temp2$classifier

#if the object can't be displayed, using this to capture the output
temp4=capture.output(print(temp1)); temp4= temp4[2]
temp_indx1=regexpr("Number of Classification Rules",temp4)
temp_indx2=regexpr("Mining Time in sec.",temp4)
temp4 = substr(temp4,temp_indx1,temp_indx2)

temp_indx1=regexpr("Rules",temp4)+7
temp_indx2=regexpr("\\\\n\\\\n",temp4)-1
temp4 = substr(temp4,temp_indx1,temp_indx2)

var=as.numeric(temp4)
}
get_CMAR_size=function(tree)
{
#extract the number of leaves for the tree
temp1=tree[1]
temp2=unclass(temp1)
temp3=temp2$classifier

#if the object can't be displayed, using this to capture the output
temp4=capture.output(print(temp1)); temp4= temp4[2]
temp_indx1=regexpr("Number of Classification Rules",temp4)
temp_indx2=regexpr("Mining Time in sec.",temp4)
temp4 = substr(temp4,temp_indx1,temp_indx2)

temp_indx1=regexpr("Rules",temp4)+7
temp_indx2=regexpr("\\\\n",temp4)-1
temp4 = substr(temp4,temp_indx1,temp_indx2)

var=as.numeric(temp4)
}