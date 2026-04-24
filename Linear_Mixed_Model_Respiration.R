#Linear Mixes Effects Model for CO2 concentration measured with CO2 Sensor Equipped
#mesocosms vs a traditional Gas Chromatograph. 

#Load Necessary Libraries and Packages
library(Rmisc)
library(ggplot2)
library(rstatix)
library(lme4)
library(lmerTest)

#load in data
CONC<-read.delim(file.choose())

#Check Structure of the data
str(CONC)
head(CONC)

#Need to set ID, Sensor, Run, Timestep, and Method as "Factors" 
CONC<-CONC%>%convert_as_factor(ID,Sensor,Run, Timestep,Method)

#check your work
str(CONC)

#Get Summary Statistics, group by Timestep and Method
CONC.summary<-summarySE(CONC,measurevar="Concentration",groupvars=c("Timestep","Method"))
CONC.summary$Timestep<-ordered(CONC.summary$Timestep,levels=c("t0","t12","t24","t48","t72","t240","t408","t576"))

#plot the data to visualize
CONC1.plot<-ggplot(CONC.summary,aes(x=Timestep,y=Concentration,colour=Method,group=Method))+geom_errorbar(aes(ymin=Concentration-se,ymax=Concentration+se),width=.2)+geom_line()+theme_bw()+geom_point(size=4)
CONC1.plot

#as we can see from the SE overlap, it doesn't look like the sensor measurement and GC measurement are significant
#but we need to run a mixed model or repeat measure anova to confirm, but as there are data points missing
#from the sensor data, a linear mixed model is the better approach

#run the linear mixed model: 
CONC1.lmer<-lmer(Concentration~Timestep*Method + (1|Sensor), data=CONC)
#Here we're treating Timestep and Method as Fixed Effects, and Sensor as random, since there are 
#theoretically an infinite number of sensors that can be used the sensors used here are just some of the 
#sensors in a larger pool of potential sensors

#run an Anova on the linear mixed model
CONC1.lmer.Anova<-Anova(CONC1.lmer)
CONC1.lmer.Anova
#We see here that Timestep is the only significant factor, which is exactly as expected for incubations
#and there are no interaction effects between timestep and method

#Now we'll run anova using a type III ANOVA using Satterthwaite's method
CONC1.lmer.anova<-anova(CONC1.lmer)
CONC1.lmer.anova
#Again, only timestep is significant, but since it's not a variable we're interested in (again,
#a change in concentration with time is expected), we'll se intercepts for every timestep (treat it as random)

CONC2.lmer<-lmer(Concentration~Method + (Timestep|Sensor),data=CONC,REML=FALSE)
anova(CONC2.lmer)                 
#Now having only checked on the Method, we find that there is no significant effect of method on the concentration