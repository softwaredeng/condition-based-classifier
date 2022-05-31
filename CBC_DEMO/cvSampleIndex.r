rm(list=ls(all=TRUE))
set.seed(1)

nRep=100

data <- read.table ( "Discretized_trainTitanic.csv", header=TRUE );

Y = data[,1]
Yunique = unique(Y)

ratio = 2/3

length = length(Y)
print(length)

N = nrow(data)

trainIxAll = NULL; testIxAll = NULL;
for(repI in 1:nRep)
{
trainIxRep = NULL; testIxRep = NULL
for(yI in 1:length(Yunique))
{
 thisY = Yunique[yI]
 ix = which(Y==thisY)
 L = ceiling(length(ix)*(2/3))
 thisIx = sample(length(ix),L,replace=FALSE)
 
 ixThisTrain = ix[thisIx]
 ixThisTest  = ix[-thisIx]

 trainIxRep = c(trainIxRep,ixThisTrain)
 testIxRep = c(testIxRep,ixThisTest) 
}
trainIxAll = rbind(trainIxAll,trainIxRep)
testIxAll  = rbind(testIxAll,testIxRep)

}

write.table(trainIxAll, file="sampleTrainIndex.txt",quote=FALSE,row.names=FALSE,col.names=FALSE,sep="\t")#
write.table(testIxAll, file="sampleTestIndex.txt",quote=FALSE,row.names=FALSE,col.names=FALSE,sep="\t")#





