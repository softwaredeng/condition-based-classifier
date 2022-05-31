rm(list=ls(all=TRUE))
library("arules")
library("RWeka")
library("randomForest")
library("RRF")
source(paste(getwd(), "/","sub_classifier.r",sep=""))
source(paste(getwd(), "/","sub_datatransform.r",sep=""))
source(paste(getwd(), "/","sub_FSRuleFunc.r",sep=""))
source("sub_CBT.r")

dataname = "Discretized_trainTitanic.csv" #tic-tac.data "german.data"  "austra.data"  Discretized_CV0_train_labor  german wine
data =read.table(paste(getwd(),"/",dataname ,sep=""),header=TRUE,
na.strings = c("?"),sep = ",")
data =dataProcess(data)

write.table(data, file=paste(getwd(),"/","titanic_discretized" ,sep="")
,na = "?",col.names=TRUE,sep=",")

cbcV=NULL; treeV=NULL

train = data



T1<- J48(Y ~ ., data = train,na.action=na.pass,control =Weka_control(M = 1)) 
pred=predict(T1,train)
pred=as.character(pred)
trueY = as.character(unlist(train[,"Y"]))
ix0=which(trueY=="0");ix1=which(trueY=="1")
Treeer0 = sum(pred[ix0]==trueY[ix0])/length(pred[ix0]);
Treeer1 = sum(pred[ix1]==trueY[ix1])/length(pred[ix1]);
TreeEr =1- c(Treeer0,Treeer1)

sup=0.05;conf=0.5
maxlen = 10
trans1 <-  as(train, "transactions")
lable=unique(train[,"Y"])
rhs_def=character(length =length(lable))
for (jj in 1:length(lable))rhs_def[jj]=paste("Y=",as.character(lable[jj]),sep="") 
lhs_empty=NULL
rules <- apriori(trans1,parameter = list(supp = sup, conf = conf, maxlen = maxlen),
appearance = list(none=lhs_empty,rhs =rhs_def,default="lhs")) 


r = cbtBuild(train,rules)
cbt = r$cbt; cbtRules = r$cbtRules; newD = r$newD

CBT_0 = cbt
CBC_0 = cbt2cbc(r$cbt,r$cbtRules)



pred=cbtPred(train,cbtRules,r$cbt)
trueY = as.character(unlist(train[,"Y"]))
ix0=which(trueY=="0");ix1=which(trueY=="1")
er0 = sum(pred[ix0]==trueY[ix0])/length(pred[ix0]);er1 = sum(pred[ix1]==trueY[ix1])/length(pred[ix1]);
CBC_Er = 1 - c(er0,er1)



#process the rules
B = gsub(":0","$ & $\\Rightarrow  Y = 0 \\ $",CBC_0,fixed=TRUE)
B = gsub(":1","$ & $\\Rightarrow  Y = 1 \\ $",B,fixed=TRUE)
B = gsub(","," \\wedge ",B,fixed=TRUE)
B = gsub("else"," Else ",B,fixed=TRUE)
B =paste(B,"\\\\")
B =paste("$",B)
#for(i in 1:length(B)){
#tmp1=gregexpr("(",B[i],fixed=TRUE)
#CBC_RRF_B1[i] = substr(CBC_RRF_B1[i],1,tmp1[[1]]-1)
#}
write.table(B, file=paste(getwd(),"/","titanic_RulesCBC" ,sep="")
,na = "?",quote=FALSE,col.names=FALSE,row.names=FALSE,sep=",")



# --- RRF Rules
rf <- RRF(newD[,-ncol(newD)],as.factor(newD[,"Y"]),flagReg = 0) # build an ordinary RF 
impRF=rf$importance 
impRF=impRF[,"MeanDecreaseGini"] # get the importance score 
imp=impRF/(max(impRF)) #normalize the importance scores into [0,1]
gamma = 0.1   #A larger gamma often leads to fewer features. But, the quality of the features selected is quite stable for GRRF, i.e., different gammas can have similar accuracy performance (the accuracy of an ordinary RF using the feature subsets). See the paper for details. 
coefReg=(1-gamma) + gamma*imp   # each variable has a coefficient, which depends on the importance score from the ordinary RF and the parameter: gamma
grrf <- RRF(newD[,-ncol(newD)],as.factor(newD[,"Y"]), flagReg=1, coefReg=coefReg)
imp=grrf$importance
imp=imp[,"MeanDecreaseGini"]
subsetGRRF = which(imp>0) #
inspect(cbtRules[subsetGRRF])

fsIx = c(subsetGRRF)
rules1 = rules[fsIx]

r = cbtBuild(train,rules1)
pred=cbtPred(train,rules1,r$cbt)

CBT_RRF = r$cbt
CBC_RRF = cbt2cbc(r$cbt,rules1)

trueY = as.character(unlist(train[,"Y"]))
ix0=which(trueY=="0");ix1=which(trueY=="1")
er0 = sum(pred[ix0]==trueY[ix0])/length(pred[ix0]);
er1 = sum(pred[ix1]==trueY[ix1])/length(pred[ix1]);
CBC_RRF_Er = c(er0,er1)


# --- RRF handle unbalanced 
minL = min(summary(newD[,"Y"]));
samplesizes = rep(minL,2); samplesizes = samplesizes/2
rf <- RRF(newD[,-ncol(newD)],as.factor(newD[,"Y"]),flagReg = 0,samplesizes=samplesizes) # build an ordinary RF 
impRF=rf$importance 
impRF=impRF[,"MeanDecreaseGini"] # get the importance score 
imp=impRF/(max(impRF)) #normalize the importance scores into [0,1]
gamma = 0.1   #A larger gamma often leads to fewer features. But, the quality of the features selected is quite stable for GRRF, i.e., different gammas can have similar accuracy performance (the accuracy of an ordinary RF using the feature subsets). See the paper for details. 
coefReg=(1-gamma) + gamma*imp   # each variable has a coefficient, which depends on the importance score from the ordinary RF and the parameter: gamma
grrf <- RRF(newD[,-ncol(newD)],as.factor(newD[,"Y"]), flagReg=1, coefReg=coefReg,samplesizes=samplesizes)
imp=grrf$importance
imp=imp[,"MeanDecreaseGini"]
subsetGRRF = which(imp>0) #
inspect(cbtRules[subsetGRRF])

fsIx = c(subsetGRRF)
rules1 = rules[fsIx]

r = cbtBuild(train,rules1)
pred=cbtPred(train,rules1,r$cbt)

CBT_RRF_B = r$cbt
CBC_RRF_B = cbt2cbc(r$cbt,rules1)

#process the rules
CBC_RRF_B1 = gsub(":0","$Rightarrow  Y = 0$",CBC_RRF_B,fixed=TRUE)
CBC_RRF_B1 = gsub(":1","$Rightarrow  Y = 1$",CBC_RRF_B1,fixed=TRUE)
CBC_RRF_B1 = gsub(","," \\wedge ",CBC_RRF_B1)
for(i in 1:length(CBC_RRF_B1)){
tmp1=gregexpr("(",CBC_RRF_B1[i],fixed=TRUE)
CBC_RRF_B1[i] = substr(CBC_RRF_B1[i],1,tmp1[[1]]-1)
}


trueY = as.character(unlist(train[,"Y"]))
ix0=which(trueY=="0");ix1=which(trueY=="1")
er0 = sum(pred[ix0]==trueY[ix0])/length(pred[ix0]);
er1 = sum(pred[ix1]==trueY[ix1])/length(pred[ix1]);
CBC_RRF_B_Er = c(er0,er1)


# --- CFS rules
nombi=make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection") 
datbin <- nombi(Y ~., data=newD, control =Weka_control(
E=" weka.attributeSelection.CfsSubsetEval",
S="weka.attributeSelection.BestFirst -D 2 -N 5"
))
coln=colnames(datbin)
CFSRuleIx = coln[1:(length(coln)-1)];
CFSRuleIx = gsub("condition_","",CFSRuleIx)
CFSRule = as.numeric(CFSRuleIx);
CFSRule= rules[CFSRule]

r = cbtBuild(train,CFSRule)

CBT_CFS = r$cbt
CBC_CFS = cbt2cbc(r$cbt,rules1)

pred=cbtPred(train,CFSRule,r$cbt)
trueY = as.character(unlist(train[,"Y"]))
ix0=which(trueY=="0");ix1=which(trueY=="1")
er0 = sum(pred[ix0]==trueY[ix0])/length(pred[ix0]);
er1 = sum(pred[ix1]==trueY[ix1])/length(pred[ix1]);
CBC_CFS_Er = c(er0,er1)


 









