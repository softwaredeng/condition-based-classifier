dataProcess=function(data)
{
colN = c("survived","pclass","sex","age","sibsp","parch","fare","embarked")
data = data[,colN]

data[,"age"] = as.factor(data[,"age"])
data[,"pclass"] = as.factor(data[,"pclass"])
data[,"sibsp"] = as.factor(data[,"sibsp"])
data[,"fare"] = as.factor(data[,"fare"])
data[,"parch"] = as.factor(data[,"parch"])
data[,"survived"] = as.factor(data[,"survived"])

levels(data[,"age"])[levels(data[,"age"])=="\\(-inf-22.5]\\"] <- "small"
levels(data[,"age"])[levels(data[,"age"])=="\\(22.5-34.25]\\"] <- "median"
levels(data[,"age"])[levels(data[,"age"])=="\\(34.25-inf)\\"] <- "large"
levels(data[,"fare"])[levels(data[,"fare"])=="\\(-inf-8.65835]\\"] <- "small"
levels(data[,"fare"])[levels(data[,"fare"])=="\\(26.125-inf)\\"] <- "large"
levels(data[,"fare"])[levels(data[,"fare"])=="\\(8.65835-26.125]\\"] <- "median"

L = levels(data[,"parch"]); levels(data[,"parch"])=c(L,">=2")
data[which(as.numeric(as.character(data[,"parch"]))>=2),"parch"]=">=2";data[,"parch"]=factor(data[,"parch"])

L = levels(data[,"sibsp"]); levels(data[,"sibsp"])=c(L,">=2")
data[which(as.numeric(as.character(data[,"sibsp"]))>=2),"sibsp"]=">=2";data[,"sibsp"]=factor(data[,"sibsp"])

colnames(data) = c("Y","pclass","sex","age","sibsp","parch","fare","embarked")

return(data)
}

cbtBuild=function(train,rules)
{

new_dataTrain=rules_to_data(rules,train)
colnames(new_dataTrain)<-c(paste("condition_", 1:(dim(new_dataTrain)[2]-1), sep = ""),"Y")

for(i in 1:(ncol(new_dataTrain)-1))
{
new_dataTrain[which(new_dataTrain[,i]==1),i]=-1
new_dataTrain[,i]= as.factor(new_dataTrain[,i])
}

for(i in 1:(ncol(new_dataTrain)-1))
{
levels(new_dataTrain[,i])[levels(new_dataTrain[,i])=="-1"] <- "__yes"
levels(new_dataTrain[,i])[levels(new_dataTrain[,i])=="0"] <- "__no"
}

T<- J48(Y ~ ., data = new_dataTrain,na.action=na.pass) 

pred=predict(T,new_dataTrain)

return(list(cbt=T,cbtRules = rules,newD = new_dataTrain))
}


cbtPred=function(test,rules,T)
{
new_dataTest=rules_to_data(rules,test)
colnames(new_dataTest)<-c(paste("condition_", 1:(dim(new_dataTest)[2]-1), sep = ""),"Y")
#colnames(new_dataTest)<-c(c( 1:(dim(new_dataTest)[2]-1),"Y"))

for(i in 1:(ncol(new_dataTest)-1))
{
new_dataTest[which(new_dataTest[,i]==1),i]=-1
new_dataTest[,i]= as.factor(new_dataTest[,i])
}

for(i in 1:(ncol(new_dataTest)-1))
{
levels(new_dataTest[,i])[levels(new_dataTest[,i])=="-1"] <- "__yes"
levels(new_dataTest[,i])[levels(new_dataTest[,i])=="0"] <- "__no"
}

pred=predict(T,new_dataTest)
pred=as.character(pred)
return(pred)
}

cbt2cbc=function(T,rules)
{
temp1=T[1]
temp4=capture.output(print(temp1)); temp4= temp4[2]
temp4 = gsub("\\n","",temp4,fixed=TRUE)
temp4 = gsub("|","",temp4,fixed=TRUE)
temp4 = gsub(" ","",temp4,fixed=TRUE)
temp4=as.character(temp4[1])

ix=gregexpr("condition_",temp4)
ix=ix[[1]];
str = NULL
for(i in 1:(length(ix)-1))
{
 thisI = ix[i]
 nextI = ix[i+1]
 thisStr = substr(temp4,thisI,(nextI-1))
 str = c(str,thisStr)
}
tmp= ix[length(ix)]
tmp1=gregexpr("NumberofLeaves",temp4)
thisStr = substr(temp4,tmp,tmp1[[1]]-1)
str = c(str,thisStr)

exisR = NULL
CBC = NULL
for(i in 1:length(str))
{
 tmp1=gregexpr("condition_",str[i])
 tmp3=gregexpr("yes",str[i]);tmp3=which(tmp3>0)
 tmp2=gregexpr("=",str[i])
 this = substr(str[i],tmp1[[1]]+10,tmp2[[1]]-1)
 ix = match(this,exisR) 
 if(is.na(ix) & length(tmp3)>0)
 {  
  exisR = c(exisR,this)
 }
 if(!is.na(ix))
 { 
  exisR = exisR[-ix]
 }
 tmp1=gregexpr(":",str[i],fixed=TRUE);tmp2=which(tmp1>0)
 if(length(tmp2)>0)
 {
  thisS = str[i]; thisS = substr(str[i],tmp1[[1]],nchar(str[i]))
  #if(length(exisR)>1)stop("1")
  CBC = c(CBC,paste(paste(exisR,collapse=","),thisS,sep=""))
 } 
}



for(i in 1:length(CBC))
{
 tmp1=gregexpr(":",CBC[i],fixed=TRUE);tmp1=tmp1[[1]]
 outcome = substr(CBC[i],tmp1,nchar(CBC[i]))
 if(tmp1==1)
 {
  thisRule = paste("else",outcome,sep=" ")
  CBC[i]=thisRule
  next
 }

 tmp1 = substr(CBC[i],1,(tmp1-1))
 
 tmp1 = strsplit(tmp1, split=",")
 tmp1 = as.numeric(tmp1[[1]])
 thisRule = NULL
 for(ii in 1:length(tmp1))
 {
  tmp2 = tmp1[ii]
  
  tmp5 = capture.output(print(inspect(lhs(rules[as.numeric(tmp2)]))))
  tmp5 = paste(tmp5,collapse="")
  tmp6=gregexpr("{",tmp5,fixed=TRUE);
  tmp7=gregexpr("}",tmp5,fixed=TRUE);
  tmpC = substr(tmp5,tmp6[[1]]+1,tmp7[[1]]-1)
  tmpC = strsplit(tmpC, split=",")
  tmpC = gsub(" ","",tmpC[[1]],fixed=TRUE)  
  thisRule = c(thisRule,tmpC)
 }
 thisRule = unique(thisRule)
 thisRule = paste(thisRule,collapse=",")
 thisRule = paste(thisRule,outcome,sep=" ")
 CBC[i]=thisRule
}



return(CBC) 


}

