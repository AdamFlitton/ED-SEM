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
                  "LangFam2017",
                  "ContinentNorris",
                  "ReligionNorris")]
mydata<-mydata[complete.cases(mydata),]
#Remove Guyana, Hong Kong, Jamaica, Mauritius, Paraguay and Trinidad - concerns over categorising language family.
mydata<-mydata[-c(43,45,53,66,78,97),]
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

library(lme4)
library(sjPlot)
library(ggplot2)
library(sjstats)


#How much variation in GDP is attributable to shared history when controlling for fixed effects:
fit1<-lmer(q_rule_lawNP~(1|ReligionNorris),data=mydata)
icc(fit1)
summary(fit1)

fit2<-lmer(WorldBankGDP2011~q_rule_lawNP+VliertIngroupFav+adjustagtran+adjuststatehist+Latitude+MurraySchallerDisease+
             EuropeanDescentCompl+(1|LangFam2017),data=mydata)
fit2<-lmer(q_rule_lawNP~VliertIngroupFav+adjustagtran+
             Latitude+EuropeanDescentCompl+adjuststatehist+
             (1|ReligionNorris),REML=FALSE,data=mydata)

icc(fit2)
summary(fit2)

fit3<-lm(WorldBankGDP2011~q_rule_lawNP+VliertIngroupFav+adjustagtran+adjuststatehist+Latitude+MurraySchallerDisease+
           EuropeanDescentCompl,data=mydata)
fit1<-lm(q_rule_lawNP~VliertIngroupFav+adjustagtran+
           Latitude+EuropeanDescentCompl+adjuststatehist,data=mydata)
summary(fit3)

#Plotting single level, random intercept and random slopes models for gdp~statehistory and langfam
library(tibble)

#Intercepts and slopes for single level model.
fit1<-lm(q_rule_lawNP~adjuststatehist,data=mydata)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(mydata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

#Intercepts and slopes for random slopes model.
fit2<-lmer(q_rule_lawNP~adjuststatehist+(adjuststatehist|LangFam2017),data=mydata)

df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)

#Intercepts and slopes for random intercepts model.
fit3<-lmer(q_rule_lawNP~adjuststatehist+(1|LangFam2017),data=mydata)

df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

#Combine intercepts and slopes for each model
df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(mydata, by = "LangFam2017")

#Plot
p_model_comparison <- ggplot(subset(df_models,LangFam2017 %in% c("Afro-Asiatic","Balto-Slavic","Germanic",
                                                                 "Italic","Niger-Congo"))) + 
  aes(x = adjuststatehist, y = q_rule_lawNP) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_sh, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("LangFam2017",scales='free') +
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top",panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14),
        strip.background=element_blank(),strip.text=element_text(size=14),
        legend.text=element_text(size=12))+
  scale_x_continuous(limits=c(-2.7,2.7))+
  scale_y_continuous(limits=c(-2.2,2.2))+
  xlab("State History")+
  ylab("Institution Quality")


p_model_comparison  

#Same plots but for gdp~agtran with continent:

fit1<-lm(mydata$WorldBankGDP2011~mydata$adjustagtran)

df_single_level<-data_frame(
  Model="Single Level",
  ContinentNorris=unique(mydata$ContinentNorris),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(WorldBankGDP2011~adjustagtran+
             (adjustagtran|ContinentNorris),REML=FALSE,mydata)


df_random_slope <- coef(fit2)[["ContinentNorris"]] %>% 
  as_tibble() %>% 
  rownames_to_column("ContinentNorris") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Slope")
df_random_slope$ContinentNorris<-factor(df_random_slope$ContinentNorris)


fit3<-lmer(WorldBankGDP2011~adjustagtran+
             (1|ContinentNorris),REML=FALSE,mydata)

df_random_intercept <- coef(fit3)[["ContinentNorris"]] %>% 
  as_tibble() %>% 
  rownames_to_column("ContinentNorris") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$ContinentNorris<-factor(df_random_intercept$ContinentNorris)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(mydata, by = "ContinentNorris")

p_model_comparison <- ggplot(subset(df_models,ContinentNorris %in% c("Africa","Asia","Europe","Americas"))) + 
  aes(x = adjustagtran, y = WorldBankGDP2011) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_sh, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("ContinentNorris",scales='free') +
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top",panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14),
        strip.background=element_blank(),strip.text=element_text(size=14),
        legend.text=element_text(size=12))+
  scale_x_continuous(limits=c(-2.7,2.7))+
  scale_y_continuous(limits=c(-2.2,2.2))+
  xlab("Timing of Agriculture")+
  ylab("GDP")

p_model_comparison  

#Model comparison
#State history and langfam
fit1<-lm(q_rule_lawNP~VliertIngroupFav+adjustagtran+
           Latitude+EuropeanDescentCompl+adjuststatehist,data=mydata)
fit2<-lmer(q_rule_lawNP~VliertIngroupFav+adjustagtran+
             Latitude+EuropeanDescentCompl+adjuststatehist+
             (adjuststatehist|LangFam2017),REML=FALSE,data=mydata)
fit3<-lmer(q_rule_lawNP~VliertIngroupFav+adjustagtran+
             Latitude+EuropeanDescentCompl+adjuststatehist+
             (1|LangFam2017),REML=FALSE,data=mydata)


AIC(fit1,fit2,fit3)
anova(fit3,fit2)

#Agtran and continent
fit1<-lm(WorldBankGDP2011~q_rule_lawNP+VliertIngroupFav+adjustagtran+
           Latitude+EuropeanDescentCompl+adjuststatehist+MurraySchallerDisease,data=mydata)
fit2<-lmer(WorldBankGDP2011~q_rule_lawNP+VliertIngroupFav+adjustagtran+
             Latitude+EuropeanDescentCompl+adjuststatehist+MurraySchallerDisease+
             (adjustagtran|ContinentNorris),REML=FALSE,data=mydata)
fit3<-lmer(WorldBankGDP2011~q_rule_lawNP+VliertIngroupFav+adjustagtran+
             Latitude+EuropeanDescentCompl+adjuststatehist+MurraySchallerDisease+
             (1|ContinentNorris),REML=FALSE,data=mydata)

AIC(fit1,fit2,fit3)
anova(fit3,fit2)






####################residuals plots
thedata<-mydata
res1<-residuals(lm(thedata$WorldBankGDP2011~thedata$q_rule_lawNP+thedata$VliertIngroupFav+thedata$adjustagtran+
                     thedata$Latitude+thedata$EuropeanDescentCompl+thedata$MurraySchallerDisease))


#append residuals to thedata
thedata$residsingle<-res1

#not sure why in below model r=0.11, when in normal single/multilevel multiple regression, statehist is 0.22

fit1<-lm(thedata$residsingle~thedata$adjuststatehist)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(thedata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(residsingle~adjuststatehist+(adjuststatehist|LangFam2017),
           data=thedata)

df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)


fit3<-lmer(residsingle~adjuststatehist+(1|LangFam2017),data=thedata)

df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(thedata, by = "LangFam2017")

p_model_comparison <- ggplot(subset(df_models,LangFam2017 %in% c("Afro-Asiatic","Balto-Slavic","Germanic",
                                                                 "Italic","Niger-Congo"))) + 
  aes(x = adjuststatehist, y = residsingle) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_sh, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap("LangFam2017",scales='free') +
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top",panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14),
        strip.background=element_blank(),strip.text=element_text(size=14),
        legend.text=element_text(size=12))+
  scale_x_continuous(limits=c(-2.7,2.7))+
  scale_y_continuous(limits=c(-2.2,2.2))+
  xlab("State History")+
  ylab("GDP")


p_model_comparison   

p_model_comparison <- ggplot(df_models) + 
  aes(x = adjuststatehist, y = residsingle) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_sh, color = Model),
              size = .75) + 
  geom_point() +
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top",panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14),
        strip.background=element_blank(),strip.text=element_text(size=14),
        legend.text=element_text(size=14))

p_model_comparison  














