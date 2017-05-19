setwd("~/desktop/Data")

####Load in data####

mydata<-read.csv("Cleandata.csv",header=T)
mydata<-mydata[,c("Country",
                  "WorldBankGDP2011",
                  "VliertIngroupFav",
                  "MurraySchallerDisease", 
                  "StateHistPutSite",
                  "q_rule_lawNP",
                  "EuropeanDescentCompl",
                  "PWAgTran",
                  "Latitude")]
mydata<-mydata[complete.cases(mydata),]
mydata$WorldBankGDP2011<-log(mydata$WorldBankGDP2011)

####Ancestry adjustment####
#State history
adjdata<-mydata[,c(1,5)]
adjdata<-adjdata[complete.cases(adjdata),]
adjdata<-data.frame(adjdata[,-1],row.names=adjdata[,1])
ancestrymatrix<-read.csv("PWMigrationMatrix.csv",header=T,sep=",",stringsAsFactors=FALSE)
ancestrymatrix<-data.frame(ancestrymatrix[,-1],row.names=ancestrymatrix[,1])

#Resize ancestry matrix to sampled countries
resizedmatrix<-ancestrymatrix[rownames(ancestrymatrix) %in% rownames(adjdata),]
resizedmatrix<-resizedmatrix[,rownames(ancestrymatrix) %in% rownames(adjdata)]

#For every column and every row, make each cell the corresponding cell from the ancestry matrix multiplied by the
#row from our data
t<-length(rownames(resizedmatrix))
j<-length(resizedmatrix)
df<-data.frame(matrix(NA,ncol=t))
result<-data.frame(matrix(NA,nrow=length(rownames(resizedmatrix)),ncol=2))
names(result)<-c("Country","AdjustedScore")
result$Country<-rownames(resizedmatrix)
for(h in 1:t){
  for(i in 1:j){
    df[h,i]<-resizedmatrix[h,i]*adjdata[i,]
  }
  result[h,2]<-sum(df[h,])
}
mydata$adjuststatehist<-result[,2]

#Timing of agriculture
adjdata<-mydata[,c(1,8)]
adjdata<-adjdata[complete.cases(adjdata),]
adjdata<-data.frame(adjdata[,-1],row.names=adjdata[,1])
ancestrymatrix<-read.csv("PWMigrationMatrix.csv",header=T,sep=",",stringsAsFactors=FALSE)
ancestrymatrix<-data.frame(ancestrymatrix[,-1],row.names=ancestrymatrix[,1])

#Resize ancestry matrix to sampled countries
resizedmatrix<-ancestrymatrix[rownames(ancestrymatrix) %in% rownames(adjdata),]
resizedmatrix<-resizedmatrix[,rownames(ancestrymatrix) %in% rownames(adjdata)]

#For every column and every row, make each cell the corresponding cell from the ancestry matrix multiplied by the
#row from our data
t<-length(rownames(resizedmatrix))
j<-length(resizedmatrix)
df<-data.frame(matrix(NA,ncol=t))
result<-data.frame(matrix(NA,nrow=length(rownames(resizedmatrix)),ncol=2))
names(result)<-c("Country","AdjustedScore")
result$Country<-rownames(resizedmatrix)
for(h in 1:t){
  for(i in 1:j){
    df[h,i]<-resizedmatrix[h,i]*adjdata[i,]
  }
  result[h,2]<-sum(df[h,])
}
mydata$adjustagtran<-result[,2]


#Scale and centre variables
library(magrittr)
library(dplyr)
mydata<-mydata %>% mutate_each_(funs(as.numeric(scale(.))),vars=c("WorldBankGDP2011","VliertIngroupFav",
                                                                  "MurraySchallerDisease", 
                                                                  "StateHistPutSite",
                                                                  "q_rule_lawNP",
                                                                  "EuropeanDescentCompl",
                                                                  "PWAgTran",
                                                                  "Latitude",
                                                                  "adjuststatehist",
                                                                  "adjustagtran"))