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

write.csv(mydata,file="varpartdata.csv")

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

#I think we do need residuals. The guide you have has no other variables. We do. The models show coefficients
#for x controlling for all the other xs. So the data need to be this too.
#But residuals for what model? We've got 3.
#Maybe we need to plot without controls?
fit1<-lm(mydata$q_rule_lawNP~mydata$adjuststatehist)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(mydata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(q_rule_lawNP~adjuststatehist+
             (adjuststatehist|LangFam2017),REML=FALSE,mydata)


df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)


fit3<-lmer(q_rule_lawNP~adjuststatehist+
             (1|LangFam2017),REML=FALSE,mydata)

df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(mydata, by = "LangFam2017")

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
  scale_y_continuous(limits=c(-2.8,2.8))+
  xlab("State History")+
  ylab("Institution Quality")

p_model_comparison  








################WITH CONTROLS
fit1<-lm(mydata$adjuststatehist~mydata$adjustagtran+mydata$Latitude+mydata$MurraySchallerDisease)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(mydata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(adjuststatehist~adjustagtran+mydata$Latitude+mydata$MurraySchallerDisease+
             (adjustagtran|LangFam2017),REML=FALSE,mydata)


df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)


fit3<-lmer(adjuststatehist~adjustagtran+mydata$Latitude+mydata$MurraySchallerDisease+
             (1|LangFam2017),REML=FALSE,mydata)

df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(mydata, by = "LangFam2017")

p_model_comparison <- ggplot(subset(df_models,LangFam2017 %in% c("Afro-Asiatic","Balto-Slavic","Germanic",
                                                                 "Italic","Niger-Congo"))) + 
  aes(x = adjustagtran, y = adjuststatehist) + 
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
  scale_y_continuous(limits=c(-2.8,2.8))+
  xlab("Timing of Agriculture")+
  ylab("State History")

p_model_comparison  


#######STATEHIST ON INST

fit1<-lm(mydata$q_rule_lawNP~mydata$adjuststatehist+mydata$Latitude+mydata$adjustagtran+mydata$VliertIngroupFav+
           mydata$EuropeanDescentCompl)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(mydata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(q_rule_lawNP~adjuststatehist+Latitude+adjustagtran+VliertIngroupFav+
           EuropeanDescentCompl+(adjuststatehist|LangFam2017),REML=FALSE,mydata)

df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)

fit3<-lmer(q_rule_lawNP~adjuststatehist+Latitude+adjustagtran+VliertIngroupFav+
           EuropeanDescentCompl+(1|LangFam2017),REML=FALSE,mydata)


df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjuststatehist) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(mydata, by = "LangFam2017")

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
  scale_y_continuous(limits=c(-2.8,2.8))+
  xlab("Timing of Agriculture")+
  ylab("State History")

p_model_comparison  


fit1<-lm(mydata$adjuststatehist~mydata$adjustagtran+mydata$Latitude+mydata$MurraySchallerDisease)
fit2<-lmer(adjuststatehist~adjustagtran+Latitude+MurraySchallerDisease+
             (adjustagtran|ContinentNorris),REML=FALSE,mydata)
fit3<-lmer(adjuststatehist~adjustagtran+Latitude+MurraySchallerDisease+
             (1|ContinentNorris),REML=FALSE,mydata)

AIC(fit1,fit2,fit3)
anova(fit2,fit1)

fit1<-lm(mydata$q_rule_lawNP~mydata$adjustagtran+mydata$Latitude+mydata$adjuststatehist+mydata$VliertIngroupFav+
           mydata$EuropeanDescentCompl)
fit2<-lmer(q_rule_lawNP~adjustagtran+Latitude+adjuststatehist+VliertIngroupFav+EuropeanDescentCompl+
             (adjuststatehist|LangFam2017),REML=FALSE,mydata)
fit3<-lmer(q_rule_lawNP~adjustagtran+Latitude+adjuststatehist+VliertIngroupFav+EuropeanDescentCompl+
             (1|LangFam2017),REML=FALSE,mydata)



###############Controlled agtran->gdp
fit1<-lm(mydata$WorldBankGDP2011~mydata$adjustagtran+mydata$adjuststatehist+mydata$Latitude+mydata$q_rule_lawNP+
           mydata$VliertIngroupFav+mydata$EuropeanDescentCompl+mydata$MurraySchallerDisease)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(mydata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(WorldBankGDP2011~q_rule_lawNP+adjuststatehist+Latitude+adjustagtran+VliertIngroupFav+MurraySchallerDisease+
             EuropeanDescentCompl+(adjustagtran|LangFam2017),REML=FALSE,mydata)

df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)

fit3<-lmer(WorldBankGDP2011~q_rule_lawNP+adjuststatehist+Latitude+adjustagtran+VliertIngroupFav+MurraySchallerDisease+
             EuropeanDescentCompl+(1|LangFam2017),REML=FALSE,mydata)


df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(mydata, by = "LangFam2017")

p_model_comparison <- ggplot(subset(df_models,LangFam2017 %in% c("Afro-Asiatic","Balto-Slavic","Germanic",
                                                                 "Italic","Niger-Congo"))) + 
  aes(x = adjustagtran, y = WorldBankGDP2011) + 
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
  scale_y_continuous(limits=c(-2.8,2.8))+
  xlab("Timing of Agriculture")+
  ylab("State History")

p_model_comparison  





fit1<-lmer(mydata$WorldBankGDP2011~mydata$adjustagtran+mydata$adjuststatehist+mydata$Latitude+mydata$q_rule_lawNP+
           mydata$VliertIngroupFav+mydata$EuropeanDescentCompl+mydata$MurraySchallerDisease+(1|mydata$LangFam2017),
           REML=F)
fit2<-lmer(mydata$WorldBankGDP2011~mydata$adjustagtran+mydata$adjuststatehist+mydata$Latitude+mydata$q_rule_lawNP+
           mydata$VliertIngroupFav+mydata$EuropeanDescentCompl+mydata$MurraySchallerDisease+(1|mydata$ContinentNorris),
           REML=F)
fit3<-lmer(mydata$WorldBankGDP2011~mydata$adjustagtran+mydata$adjuststatehist+mydata$Latitude+mydata$q_rule_lawNP+
           mydata$VliertIngroupFav+mydata$EuropeanDescentCompl+mydata$MurraySchallerDisease+(1|mydata$ReligionNorris),
           REML=F)
fit4<-lm(mydata$WorldBankGDP2011~mydata$adjustagtran+mydata$adjuststatehist+mydata$Latitude+mydata$q_rule_lawNP+
           mydata$VliertIngroupFav+mydata$EuropeanDescentCompl+mydata$MurraySchallerDisease)

fit5<-lmer(mydata$WorldBankGDP2011~(1|mydata$LangFam2017),
           REML=F)
fit6<-lmer(mydata$WorldBankGDP2011~(1|mydata$ContinentNorris),
           REML=F)
fit7<-lmer(mydata$WorldBankGDP2011~(1|mydata$ReligionNorris),
           REML=F)
fit8<-lm(mydata$WorldBankGDP2011~1)



AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8)






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

thedata<-mydata[,c(1,11,14,15)]

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
anova(fit2,fit1)


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
anova(fit2,fit3)


#Agtran and statehist by langfam
fit1<-lm(adjuststatehist~adjustagtran+
           Latitude+MurraySchallerDisease,data=mydata)
fit2<-lmer(adjuststatehist~adjustagtran+
             Latitude+MurraySchallerDisease+
             (adjustagtran|LangFam2017),REML=FALSE,data=mydata)
fit3<-lmer(adjuststatehist~adjustagtran+
             Latitude+MurraySchallerDisease+
             (1|LangFam2017),REML=FALSE,data=mydata)





####################residuals plots
thedata<-mydata
res1<-residuals(lm(thedata$WorldBankGDP2011~thedata$q_rule_lawNP+thedata$VliertIngroupFav+thedata$adjustagtran+
                     thedata$Latitude+thedata$EuropeanDescentCompl+thedata$MurraySchallerDisease))

res1<-residuals(lm(thedata$adjuststatehist~thedata$MurraySchallerDisease+
                     thedata$Latitude))


#append residuals to thedata
thedata$residsingle<-res1


fit1<-lm(thedata$residsingle~thedata$adjustagtran)

df_single_level<-data_frame(
  Model="Single Level",
  LangFam2017=unique(thedata$LangFam2017),
  Intercept=coef(fit1)[1],
  Slope_sh=coef(fit1)[2])

fit2<-lmer(residsingle~adjustagtran+(adjustagtran|LangFam2017),
           data=thedata)

df_random_slope <- coef(fit2)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Slope")
df_random_slope$LangFam2017<-factor(df_random_slope$LangFam2017)


fit3<-lmer(residsingle~adjustagtran+(1|LangFam2017),data=thedata)

df_random_intercept <- coef(fit3)[["LangFam2017"]] %>% 
  as_tibble() %>% 
  rownames_to_column("LangFam2017") %>% 
  rename(Intercept = `(Intercept)`, Slope_sh = adjustagtran) %>% 
  add_column(Model = "Random Intercept")
df_random_intercept$LangFam2017<-factor(df_random_intercept$LangFam2017)

df_models <- bind_rows(df_single_level, df_random_slope, df_random_intercept) %>% 
  left_join(thedata, by = "LangFam2017")

p_model_comparison <- ggplot(subset(df_models,LangFam2017 %in% c("Afro-Asiatic","Balto-Slavic","Germanic",
                                                                 "Italic","Niger-Congo"))) + 
  aes(x = adjustagtran, y = residsingle) + 
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
  scale_x_continuous()+
  scale_y_continuous()+
  xlab("Timing of Agriculture")+
  ylab("State History")


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






###################ROLE OF CLIMATIC VARIABILITY
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
                  "VliertClimateHarshness",
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
                                                                  "VliertClimateHarshness",
                                                                  "adjuststatehist",
                                                                  "adjustagtran"))

library(lme4)
library(sjPlot)
library(ggplot2)
library(sjstats)



mod1<-lmer(WorldBankGDP2011~q_rule_lawNP+adjuststatehist+VliertClimateHarshness+MurraySchallerDisease+
             EuropeanDescentCompl+VliertIngroupFav+Latitude+adjustagtran+
             adjustagtran:VliertClimateHarshness+(1|LangFam2017),mydata)
library(car)
Anova(mod1)


##############Graphs for paper

#gdp graph
titles<-c("Measure of Shared Cultural History","Proportion of Variance")
measure<-c("Language Family","Continent","Historical Religion","Language Family",
           "Continent", "Historical Religion",
           "Language Family","Continent","Historical Religion","Language Family",
           "Continent", "Historical Religion")
prop<-c(43,48,60,85,73,100,
        57,52,40,15,27,0)
stacker<-c(1,1,1,1,1,1,2,2,2,2,2,2)
labeller<-c("No Controls","No Controls","No Controls","With Controls","With Controls","With Controls",
            "No Controls","No Controls","No Controls","With Controls","With Controls","With Controls")

df<-data.frame(measure,prop,stacker,labeller)
df$stacker<-as.factor(df$stacker)
df$measure<-as.factor(df$measure)
df$labeller<-as.factor(df$labeller)

ggplot(df,aes(x=measure,y=prop,fill=stacker))+
  geom_bar(stat='identity')+
  scale_x_discrete(limits = c("Language Family","Continent","Historical Religion"))+
  scale_fill_manual("legend",values = c("1" = "grey", "2" = "black"))+
  coord_cartesian(ylim=c(0,100))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,100,10))+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14),legend.position="none",
        strip.background = element_rect(fill="white"),strip.text.x = element_text(size=14))+
  xlab("Measure of Shared Cultural History")+
  ylab("Proportion of Variation in GDP Accounted for")+
  facet_grid(.~labeller)


#inst qual graph
titles<-c("Measure of Shared Cultural History","Proportion of Variance")
measure<-c("Language Family","Continent","Historical Religion","Language Family",
           "Continent", "Historical Religion",
           "Language Family","Continent","Historical Religion","Language Family",
           "Continent", "Historical Religion")
prop<-c(48,68,65,74,100,91,
        52,32,35,26,0,9)
stacker<-c(1,1,1,1,1,1,2,2,2,2,2,2)
labeller<-c("No Controls","No Controls","No Controls","With Controls","With Controls","With Controls",
            "No Controls","No Controls","No Controls","With Controls","With Controls","With Controls")

df<-data.frame(measure,prop,stacker,labeller)
df$stacker<-as.factor(df$stacker)
df$measure<-as.factor(df$measure)
df$labeller<-as.factor(df$labeller)

ggplot(df,aes(x=measure,y=prop,fill=stacker))+
  geom_bar(stat='identity')+
  scale_x_discrete(limits = c("Language Family","Continent","Historical Religion"))+
  scale_fill_manual("legend",values = c("1" = "grey", "2" = "black"))+
  coord_cartesian(ylim=c(0,100))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,100,10))+
  theme(panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=18),axis.text=element_text(size=14),legend.position="none",
        strip.background = element_rect(fill="white"),strip.text.x = element_text(size=14))+
  xlab("Measure of Shared Cultural History")+
  ylab("Prop. of Variation in Inst. Quality Accounted for")+
  facet_grid(.~labeller)



