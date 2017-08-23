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
                  "tropicalNP",
                  "Latitude",
                  "LangFam2017")]
mydata<-mydata[complete.cases(mydata),]
#Remove Guyana, Jamaica, Mauritius, Paraguay and Trinidad - concerns over categorising language family.
mydata<-mydata[-c(43,53,66,78,97),]
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
                                                                  "tropicalNP",
                                                                  "adjuststatehist",
                                                                  "adjustagtran"))


write.csv(mydata,file="finaldata.csv")

######Graphs for paper
setwd("~/desktop/Data")

mydata<-read.csv("finaldata.csv",header=T)
names(mydata)

library(ggplot2)

plot1<-ggplot(mydata,aes(x=statehist,y=gdp))+
  geom_point()+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14))+
  geom_smooth(method="lm",alpha=0.1,colour="black",size=0.5)+
  xlab("State History")+
  ylab("GDP")


plot2<-ggplot(mydata,aes(x=inst,y=gdp))+
  geom_point()+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14))+
  geom_smooth(method="lm",alpha=0.1,colour="black",size=0.5)+
  xlab("Institution Quality")+
  ylab("GDP")


plot3<-ggplot(mydata,aes(x=statehist,y=inst))+
  geom_point()+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14))+
  geom_smooth(method="lm",alpha=0.1,colour="black",size=0.5)+
  # geom_rug(data=mydata,aes(y=instres),sides="l")+
  #geom_smooth(data=subset(mydata,langfam=="Niger-Congo"|langfam=="Balto-Slavic"|langfam=="Germanic"),
  #             aes(statehist,inst,color=langfam),method=lm,se=FALSE)+
  xlab("State History")+
  ylab("Institution Quality")

res<-residuals(lm(gdp~inst,mydata))
mydata$res<-res

plot4<-ggplot(mydata,aes(x=statehist,y=res))+
  geom_point()+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14))+
  geom_smooth(method="lm",alpha=0.1,colour="black",size=0.5)+
  xlab("State History")+
  ylab("GDP~Institutions Model Residuals")

library(cowplot)
plot_grid(plot1,plot2,plot3,plot4,labels="AUTO",label_size=20,vjust=1)

ggsave("plot1.pdf",path="~/desktop/",scale=1,dpi=300,limitsize=TRUE)



#Rug?
#need a new column containing intercepts
lm1<-lmer(inst~statehist+(1|langfam),mydata)
lmres<-coef(lm1)$langfam

lmres$langfam<-rownames(lmres)
rownames(lmres)<-NULL
lmres$inter<-lmres$`(Intercept)`
lmres$`(Intercept)`<-NULL
mydata$instres <- lmres[match(paste(mydata$langfam),paste(lmres$langfam)),"inter"]


  
ggplot(mydata,aes(x=statehist,y=inst))+
  geom_point()+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14))+
  geom_smooth(method="lm",alpha=0.1,colour="black",size=0.5)+
 # geom_rug(data=mydata,aes(y=instres),sides="l")+
  #geom_smooth(data=subset(mydata,langfam=="Niger-Congo"|langfam=="Balto-Slavic"|langfam=="Germanic"),
 #             aes(statehist,inst,color=langfam),method=lm,se=FALSE)+
  xlab("State History")+
  ylab("Institution Quality")




