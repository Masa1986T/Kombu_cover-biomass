rm(list=ls())
library(nlme)
library(ggplot2)

Kombu<-read.csv("Kombu.csv",stringsAsFactors = TRUE)
Kombu$Season <- factor(Kombu$Season, 
                       levels = c("Flourish","Decline") )
names(Kombu)

####### Hokkaido @ Makombu##########
Kombu_Hokkaido<-subset(Kombu,Kombu$Jap_name=="Onikombu"|Kombu$Jap_name=="Makombu"&Kombu$Region=="Hokkaido")
Kombu_Hokkaido$DW_g_m2

###線形モデル#####
model1_HD1<- glm(DW_g_m2~ Cover,
                data = Kombu_Hokkaido)

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


####二次関数モデル#####
model4_HD<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=1, b=1,c=1), data =  Kombu_Hokkaido)
summary(model4_HD)
AIC(model4_HD)
BIC(model4_HD)



####### Hokkaido##########
library(ggplot2)

#Model1
dummy_Hokkaido<- expand.grid(Cover=seq(min(Kombu_Hokkaido$Cover),
                                            max(Kombu_Hokkaido$Cover),length=1000))

pred_HD<- predict(model1_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW<-pred_HD

plot_Hokkaido1<-ggplot(data=Kombu_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido1

#Model2
pred_Hokkaido2<- predict(model2_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW2<-pred_Hokkaido2

plot_Hokkaido2<-ggplot(data=Kombu_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido2

# save width 600 * Height 500 
#Model 3
pred_Hokkaido3<- predict(model3_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW3<-pred_Hokkaido3

plot_Hokkaido3<-ggplot(data=Kombu_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido3


#Model4
pred_Hokkaido4<- predict(model4_HD,newdata=dummy_Hokkaido,se.fit=T)
dummy_Hokkaido$DW4<-pred_Hokkaido4

plot_Hokkaido4<-ggplot(data=Kombu_Hokkaido, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido4

#width 600 * Height 500 save

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


####二次関数モデル#####
model4_HDN<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=1, b=1,c=1), data =  Kombu_Hokkaido_Naga)
summary(model4_HDN)
AIC(model4_HDN)
BIC(model4_HDN)



####### Hokkaido_Naga##########
library(ggplot2)

#Model1
dummy_Hokkaido_Naga<- expand.grid(Cover=seq(min(Kombu_Hokkaido_Naga$Cover),
                                       max(Kombu_Hokkaido_Naga$Cover),length=1000))
pred_HD_Naga<- predict(model1_HDN,newdata=dummy_Hokkaido_Naga,
                             se.fit=T,interval="confidence")
pred_HD_Naga$fit


pred_Hokkaido_Nagah<- pred_Hokkaido_Naga$fit+pred$se.fit*1.96
pred_Hokkaido_Nagal<- pred_Hokkaido_Naga$fit-pred$se.fit*1.96

dummy_Hokkaido_Naga$DW<-pred_Hokkaido_Naga

plot_Hokkaido_Naga1<-ggplot(data=Kombu_Hokkaido_Naga, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido_Naga, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido_Naga1

#Model2
pred_Hokkaido_Naga2<- predict(model2_HDN,newdata=dummy_Hokkaido_Naga,se.fit=T)
dummy_Hokkaido_Naga$DW2<-pred_Hokkaido_Naga2

plot_Hokkaido_Naga2<-ggplot(data=Kombu_Hokkaido_Naga, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido_Naga, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido_Naga2

# save width 600 * Height 500 
#Model 3
pred_Hokkaido_Naga3<- predict(model3_HDN,newdata=dummy_Hokkaido_Naga,
                              se.fit=T,interval="confidence")

pred_Hokkaido_Naga3$fit
pred_Hokkaido_Naga3h<- pred_Hokkaido_Naga3$fit+pred_Hokkaido_Naga3$se.fit*1.96
pred_Hokkaido_Naga3l<- pred_Hokkaido_Naga3$fit-pred_Hokkaido_Naga3$se.fit*1.96
dummy_Hokkaido_Naga$DW3<-pred_Hokkaido_Naga3

plot_Hokkaido_Naga3<-ggplot(data=Kombu_Hokkaido_Naga, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido_Naga, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido_Naga3


#Model4
pred_Hokkaido_Naga4<- predict(model4_HDN,newdata=dummy_Hokkaido_Naga,se.fit=T)
dummy_Hokkaido_Naga$DW4<-pred_Hokkaido_Naga4

plot_Hokkaido_Naga4<-ggplot(data=Kombu_Hokkaido_Naga, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido_Naga, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido_Naga4

#width 600 * Height 500 save

##### Makombu + Nagakombu Model 3 
pred_Hokkaido_Naga3<- predict(model3_HDN,newdata=dummy_Hokkaido_Naga,se.fit=T)
dummy_Hokkaido_Naga$DW3<-pred_Hokkaido_Naga3

Kombu_Hokkaido2<-rbind(Kombu_Hokkaido,Kombu_Hokkaido_Naga)

plot_Hokkaido_Ma_Naga<-ggplot(data=Kombu_Hokkaido2, aes(x=Cover, y=DW_g_m2,shape =Type)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=4,color="#00AFBB")+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 12000,length=7),limits=c(0,12000))+
  scale_shape_manual(labels = c("Makombu", "Nagakombu"),values = c(16,2))+
  labs(color='Season')+
  geom_line(data=dummy_Hokkaido, aes(x=Cover, y=DW3),linewidth=1,linetype = 1,color="black",inherit.aes = FALSE)+
  geom_line(data=dummy_Hokkaido_Naga, aes(x=Cover, y=DW3),linewidth=1,linetype = 2,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_Hokkaido_Ma_Naga


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


####二次関数モデル#####
model4_TP<- nls(DW_g_m2~ a*Cover^2 + b*Cover + c,
                start = list(a=1, b=1,c=1), data =  Kombu_TohokuPacific)
summary(model4_TP)
AIC(model4_TP)
BIC(model4_TP)



####### TohokuPacific##########
library(ggplot2)

#Model1
dummy_TohokuPacific<- expand.grid(Cover=seq(min(Kombu_TohokuPacific$Cover),
                                             max(Kombu_TohokuPacific$Cover),length=1000))

pred_TohokuPacific<- predict(model1_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW<-pred_TohokuPacific

plot_TohokuPacific1<-ggplot(data=Kombu_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific1

#Model2
pred_TohokuPacific2<- predict(model2_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW2<-pred_TohokuPacific2

plot_TohokuPacific2<-ggplot(data=Kombu_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW2),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific2

# save width 600 * Height 500 
#Model 3
pred_TohokuPacific3<- predict(model3_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW3<-pred_TohokuPacific3

plot_TohokuPacific3<-ggplot(data=Kombu_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW3),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific3


#Model4
pred_TohokuPacific4<- predict(model4_TP,newdata=dummy_TohokuPacific,se.fit=T)
dummy_TohokuPacific$DW4<-pred_TohokuPacific4

plot_TohokuPacific4<-ggplot(data=Kombu_TohokuPacific, aes(x=Cover, y=DW_g_m2,color = Season)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  geom_point(size=3.5)+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 1000,length=5),limits=c(0,1000))+
  scale_color_manual(labels = c("Flourish", "Decline"),values = c("#00AFBB","#FC4E07"))+
  labs(color='Season')+
  geom_line(data=dummy_TohokuPacific, aes(x=Cover, y=DW4),linewidth=1,color="black",inherit.aes = FALSE)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.7), 
        legend.title=element_text(size=23),legend.text =  element_text(size = 22))
plot_TohokuPacific4

#width 600 * Height 500 save

######Plot line  all species  ##########
dummy_Hokkaido1<- expand.grid(Cover=seq(min(Kombu_Hokkaido$Cover),
                                       max(Kombu_Hokkaido$Cover),length=1000))
pred_Hokkaido3<- predict(model3_HD,newdata=dummy_Hokkaido1,se.fit=T)
dummy_Hokkaido1$DW<-pred_Hokkaido3
dummy_Hokkaido1$RegionClass<-"HD Makombu"

dummy_Hokkaido_Naga1<- expand.grid(Cover=seq(min(Kombu_Hokkaido_Naga$Cover),
                                            max(Kombu_Hokkaido_Naga$Cover),length=1000))
pred_Hokkaido_Naga3<- predict(model3_HDN,newdata=dummy_Hokkaido_Naga1,se.fit=T)
dummy_Hokkaido_Naga1$DW<-pred_Hokkaido_Naga3
dummy_Hokkaido_Naga1$RegionClass<-"HD Nagakombu"


dummy_TohokuPacific1<- expand.grid(Cover=seq(min(Kombu_TohokuPacific$Cover),
                                            max(Kombu_TohokuPacific$Cover),length=1000))
pred_TohokuPacific2<- predict(model2_TP,newdata=dummy_TohokuPacific1,se.fit=T)
dummy_TohokuPacific1$DW<-pred_TohokuPacific2
dummy_TohokuPacific1$RegionClass<-"TP Makombu"

dummy_Kombu<-rbind(dummy_Hokkaido1,dummy_Hokkaido_Naga1,dummy_TohokuPacific1)
dummy_Kombu$RegionClass<-factor(dummy_Kombu$RegionClass, 
                                 levels = c("HD Makombu","HD Nagakombu","TP Makombu") )

plot_all_kombu<-ggplot(data=dummy_Kombu, aes(x=Cover, y=DW,linetype = RegionClass,color= RegionClass)) + 
  xlab("Coverage (%)")+ 
  ylab(expression(paste("Biomass (dry weight g/ ",{m^2},")",sep="")))+
  scale_x_continuous(breaks=seq(0, 100,length=6),limits=c(0,100))+
  scale_y_continuous(breaks=seq(0, 6000,length=7),limits=c(0,6000))+ 
  geom_line(linewidth=1)+
  theme_classic(base_size = 24, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=23))+
  theme(legend.justification=c(0.02,0.02), legend.position=c(0.05,0.55), 
        legend.title=element_text(size=20),legend.text =  element_text(size = 20))
plot_all_kombu
