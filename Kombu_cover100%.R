rm(list=ls())
library(nlme)
library(ggplot2)
library(propagate)

Kombu<-read.csv("Kombu.csv",stringsAsFactors = TRUE)
Kombu$Season <- factor(Kombu$Season, 
                       levels = c("Flourish","Decline") )
names(Kombu)

####### Hokkaido @ Makombu##########
Kombu_Hokkaido<-subset(Kombu,Kombu$Jap_name=="Onikombu"|Kombu$Jap_name=="Makombu"&Kombu$Region=="Hokkaido")
Kombu_Hokkaido$DW_g_m2

###線形モデル#####
model1_HD<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_Hokkaido)
summary(model1_HD)
AIC(model1_HD)
BIC(model1_HD)

####累乗モデル#####
model2_HD<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_Hokkaido)
summary(model2_HD)
AIC(model2_HD)
BIC(model2_HD)

####指数モデル#####
model3_HD<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_Hokkaido,
                trace=TRUE)
summary(model3_HD)
AIC(model3_HD)
BIC(model3_HD)



####### Hokkaido##########
library(ggplot2)
library(propagate)

#Model1
dummy_100<- expand.grid(Cover=100)
pred1_HD100_makombu<- predictNLS(model1_HD,newdata=dummy_100,interval = c("confidence"))
pred1_HD100_makombu
pred1_HD100_makombu_vector<-pred1_HD100_makombu[[1]]
pred1_HD100_makombu_vector<-data.frame(ID="pred1_HD100_makombu",Region="Hokkaido",
                                       Region="HD",
                                       Type="Makombu",
                                       Equation="Linear",
                                       pred1_HD100_makombu_vector)
pred1_HD100_makombu_vector

#Model2
pred2_HD100_makombu<- predictNLS(model2_HD,newdata=dummy_100,interval = c("confidence"))
pred2_HD100_makombu
pred2_HD100_makombu[[1]]
pred2_HD100_makombu[[1]][[3]]

pred2_HD100_makombu_vector<-pred2_HD100_makombu[[1]]
pred2_HD100_makombu_vector<-data.frame(ID="pred2_HD100_makombu",Region="Hokkaido",
                                       Region="HD",
                                       Type="Makombu",
                                       Equation="Power",
                                       pred2_HD100_makombu_vector)
pred2_HD100_makombu_vector

# save width 600 * Height 500 
#Model 3
pred3_HD100_makombu<- predictNLS(model3_HD,newdata=dummy_100,interval = c("confidence"))
pred3_HD100_makombu
pred3_HD100_makombu_vector<-pred3_HD100_makombu[[1]]
pred3_HD100_makombu_vector<-data.frame(ID="pred3_HD100_makombu",Region="Hokkaido",
                                       Region="HD",
                                       Type="Makombu",
                                       Equation="Exponent",
                                       pred3_HD100_makombu_vector)

MakombuHD100<-rbind(pred1_HD100_makombu_vector,
                    pred2_HD100_makombu_vector,
                    pred3_HD100_makombu_vector)

####### Hokkaido @ Nagakombu ##########
Kombu_Hokkaido_Naga<-subset(Kombu,Kombu$Jap_name=="Nagakombu"&Kombu$Region=="Hokkaido")
Kombu_Hokkaido_Naga$DW_g_m2

###線形モデル#####
model1_HDN<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_Hokkaido_Naga)
summary(model1_HDN)
AIC(model1_HDN)
BIC(model1_HDN)

####累乗モデル#####
model2_HDN<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_Hokkaido_Naga)
summary(model2_HDN)
AIC(model2_HDN)
BIC(model2_HDN)

####指数モデル#####
model3_HDN<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_Hokkaido_Naga,
                trace=TRUE)
summary(model3_HDN)
AIC(model3_HDN)
BIC(model3_HDN)


####### Hokkaido_Naga##########
library(ggplot2)

#Model1
dummy_100<- expand.grid(Cover=100)
pred1_HD100_Naga<- predictNLS(model1_HDN,newdata=dummy_100,interval="confidence")
pred1_HD100_Naga
pred1_HD100_Naga_vector<-pred1_HD100_Naga[[1]]
pred1_HD100_Naga_vector<-data.frame(ID="pred1_HD100_Naga",Region="Hokkaido",
                                       Region="HD",
                                       Type="Nagakombu",
                                       Equation="Linear",
                                       pred1_HD100_Naga_vector)
pred1_HD100_Naga_vector

#Model2
pred2_HD100_Naga<- predictNLS(model2_HDN,newdata=dummy_100,interval="confidence")
pred2_HD100_Naga
pred2_HD100_Naga_vector<-pred2_HD100_Naga[[1]]
pred2_HD100_Naga_vector<-data.frame(ID="pred2_HD100_Naga",Region="Hokkaido",
                                    Region="HD",
                                    Type="Nagakombu",
                                    Equation="Power",
                                    pred2_HD100_Naga_vector)
pred2_HD100_Naga_vector

#Model 3
pred3_HD100_Naga<- predictNLS(model3_HDN,newdata=dummy_100,interval="confidence")
pred3_HD100_Naga
pred3_HD100_Naga_vector<-pred3_HD100_Naga[[1]]
pred3_HD100_Naga_vector<-data.frame(ID="pred3_HD100_Naga",Region="Hokkaido",
                                    Region="HD",
                                    Type="Nagakombu",
                                    Equation="Exponent",
                                    pred3_HD100_Naga_vector)
pred3_HD100_Naga_vector

NagaHD100<-rbind(pred1_HD100_Naga_vector,
                    pred2_HD100_Naga_vector,
                    pred3_HD100_Naga_vector)


#width 600 * Height 500 save




####### TohokuPacific @ Kombu##########
Kombu_TohokuPacific<-subset(Kombu,Kombu$Region=="TohokuPacific")
Kombu_TohokuPacific$DW_g_m2

###線形モデル#####
model1_TP<- nls(DW_g_m2~ a*Cover,
                start = list(a=100),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_TohokuPacific)
summary(model1_TP)
AIC(model1_TP)
BIC(model1_TP)

####累乗モデル#####
model2_TP<- nls(DW_g_m2~ a*Cover^b,
                start = list(a=10,b = 1),
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_TohokuPacific)
summary(model2_TP)
AIC(model2_TP)
BIC(model2_TP)

####指数モデル#####
model3_TP<- nls(DW_g_m2~ a*exp(b*Cover),
                start = list(a=40,b = 0.05), 
                control = list(maxiter = 50000, warnOnly = TRUE),
                data = Kombu_TohokuPacific,
                trace=TRUE)
summary(model3_TP)
AIC(model3_TP)
BIC(model3_TP)



####### TohokuPacific##########
library(ggplot2)

#Model1
pred1_TP100_Makombu<- predictNLS(model1_TP,newdata=dummy_100,se.fit=T)
pred1_TP100_Makombu_vector<-pred1_TP100_Makombu[[1]]
pred1_TP100_Makombu_vector<-data.frame(ID="pred1_TP100_Makombu",Region="Tohoku Pacific",
                                    Region="TP",
                                    Type="Makombu",
                                    Equation="Linear",
                                    pred1_TP100_Makombu_vector)
pred1_TP100_Makombu_vector

#Model2
pred2_TP100_Makombu<- predictNLS(model2_TP,newdata=dummy_100,se.fit=T)
pred2_TP100_Makombu_vector<-pred2_TP100_Makombu[[1]]
pred2_TP100_Makombu_vector<-data.frame(ID="pred2_TP100_Makombu",Region="Tohoku Pacific",
                                       Region="TP",
                                       Type="Makombu",
                                       Equation="Power",
                                       pred2_TP100_Makombu_vector)
pred2_TP100_Makombu_vector


#Model 3
pred3_TP100_Makombu<- predictNLS(model3_TP,newdata=dummy_100,se.fit=T)
pred3_TP100_Makombu_vector<-pred3_TP100_Makombu[[1]]
pred3_TP100_Makombu_vector<-data.frame(ID="pred3_TP100_Makombu",Region="Tohoku Pacific",
                                       Region="TP",
                                       Type="Makombu",
                                       Equation="Exponent",
                                       pred3_TP100_Makombu_vector)
pred3_TP100_Makombu_vector

MakombuTP100<-rbind(pred1_TP100_Makombu_vector,
                 pred2_TP100_Makombu_vector,
                 pred3_TP100_Makombu_vector)



######Plot line  all species  ##########
rm(Kombu100)
Kombu100<-rbind(MakombuHD100,NagaHD100,MakombuTP100)
Kombu100$Equation <- factor(Kombu100$Equation, 
                       levels = c("Linear","Power","Exponent") )
Kombu100
library(dplyr)

Kombu100<-mutate(Kombu100,RegionType = interaction(Region.1, Type))

Kombu100$RegionType <- factor(Kombu100$RegionType, 
                            levels = c("HD.Makombu","HD.Nagakombu","TP.Makombu") )
Kombu100
RegionType
library(ggplot2)
plot_all_kombu100<-ggplot(data=Kombu100, 
                          aes(x=RegionType, y=Prop.Mean.1,color= Equation,shape= Equation)) + 
  xlab("Region Type")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6200))+ 
  geom_point(stat = "identity", position = position_dodge(width = 0.9),size=3) +
  geom_errorbar(aes(ymin = Prop.Mean.1 - Prop.sd.1, ymax = Prop.Mean.1 + Prop.sd.1),
                width = 0.5, position = position_dodge(width = 0.9))+
  theme(axis.title =element_text(size=20),axis.text = element_text(color="black",size=20))+
  theme(legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_kombu100


