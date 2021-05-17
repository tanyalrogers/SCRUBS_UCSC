#More GGPlot Customizations + stat functions
#Matt Kustra

# 1. Getting Ready --------------------------------------------------------
# * 1.a load up libraries -------------------------------------------------
library(tidyverse)

# * 1.b Load up data set --------------------------------------------------
Data<-read.csv("Data/example_data.csv")
glimpse(Data)
#Sow we have sites, tide heights and 3 different animals
# 2.Making a simple scatterplot -------------------------------------------
#let's make a simple scatter plot of siite /tide.height
ggplot(Data,aes(x=mussels,y=snails))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(site ~ tide.height)

# 3. Change order of factors ----------------------------------------------
#Order of tide.height looks weird so let's change that
Data<-Data%>%
  #to change order inside the levels vector choose the desired order.
  mutate(tide.height=factor(tide.height,levels=c("low","high")))
#Now plot again
ggplot(Data,aes(x=mussels,y=snails))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(site ~ tide.height)

# 4. Change facet labels --------------------------------------------------
#First make a vector that has the desired labels
tide.labs<-c("Low tide","High tide")
#now name the vector as the original values
names(tide.labs)<-c("low","high")
#First make a vector that has the desired labels
#\n makes a new line
site.labs<-c("Monterey","Santa Cruz","Half Moon \n Bay")
#now name the vector as the original values
names(site.labs)<-c("A","B","C")
#now lets change it
ggplot(Data,aes(x=mussels,y=snails))+
  geom_point()+
  geom_smooth(method="lm")+
  #add the labeller to facet grid
  facet_grid(site ~ tide.height,labeller =labeller(tide.height=tide.labs,site=site.labs))

# 5. Make facet axes free ----------------------------------------------------------
#not always but sometimes it makes sense too allow axes scale to vary between 
baseplot<-ggplot(Data,aes(x=mussels,y=snails))+
  geom_point()+
  geom_smooth(method="lm")+
  #by default scales is fixed but you can change it to either
  #"free" for both axes
  #"free_x" for just x axis or "free_y" for just y axis
  facet_grid(site ~ tide.height,labeller =labeller(tide.height=tide.labs,site=site.labs),scales="free")

# 6. bquotes for Math expressions -------------------------------------------------------------
#bquotes is sort of confusing at first but is useful
#https://www.r-bloggers.com/2018/03/math-notation-for-r-plot-titles-expression-and-bquote/ is a good tutorial
#strings "string" and are seperated by ~
#variables are indicated by .(varname)
#math expression are unquoted
#some simple math expressions
#  ^ is super script
#  [ ] is sub script
#  you can just spell out greek letters
#more info: https://astrostatistics.psu.edu/su07/R/html/grDevices/html/plotmath.html
#lets put the units for our figure
baseplot+xlab(bquote("Mussels ("~m^-2 ~")"))+
  ylab(bquote("Snails ("~m^-2 ~")"))
#or as fraction
baseplot+xlab(bquote(frac(Mussels,m^2)))+
  ylab(bquote(frac(Snails,m^2)))
#let's just save the one plot before we move onto theme custimization 
plot2<-baseplot+xlab(bquote("Mussels ("~m^-2 ~")"))+
  ylab(bquote("Snails ("~m^-2 ~")"))

# 7. Theme customizations -------------------------------------------------------
#As we talked about last time we have defualt themes 
#theme classic is always nice
plot2+theme_classic()
#but how can we do more things?
#use theme()
?theme()
#wow lots of things to custimize.
#At least to me it's not very intuitive...google is your friend
#in general to customize a certain aspect you will always use
#theme(thing_to_customize=element_xxx(something=yy,somethingelse=zz))
#for example, text is too small let's make all text larger
#using text will change all of the text
#to change all text size text=element_text(size=xx)
?element_text()
plot2+theme_classic()+theme(text=element_text(size=12,face="bold"))
#by defualt text of axis is grey not black, let's change that
plot2+theme_classic()+theme(text=element_text(size=12,face="bold"),axis.text=element_text(color="black"))
#notice that it didn't bold the bquote labels to do that you have to do work inside bquote bquote(bold(...))
#lets add panel border
#need to use element_rect() this time
plot2+theme_classic()+theme(text=element_text(size=12,face="bold"),axis.text=element_text(color="black"),
                            #must specify fill = NA
      panel.border =element_rect(color="black",size=1,fill=NA) )
#thats pretty good, but annoying to write that everytime!
#luckily we can save themes
mytheme<-theme_classic()+theme(text=element_text(size=12,face="bold"),axis.text=element_text(color="black"),
                                #must specify fill = NA
                                panel.border =element_rect(color="black",size=1,fill=NA),
                               legend.position = "top")
plot2+mytheme
#also we can even set custom themes to be the defualt
theme_set(mytheme)
#now what does our plot look like
plot2

# 8. Quick bit on summary_functions ---------------------------------------
#often we want to plot summary data overlain on plots such as violin plots
#so lets make a violin plot
glimpse(Data)
basevp<-ggplot(Data,aes(x=tide.height,y=snails,fill=tide.height))+
  geom_violin(alpha=0.5)+
  facet_grid(. ~ site,labeller =labeller(site=site.labs))+
  xlab("Tide Height")+
  ylab(bquote(bold("Snails ("~m^-2 ~")")))
#let's add mean and sd
#when it's just one thing being calculated use fun
#stat_summary(fun=some function,fun.args=xxx,geom="")
#can also specify the geom used, each summary has a default
#let's add the mean
basevp+stat_summary(fun=mean,geom="point") 
#when its multiple use stat_summary(fun.data=some function,fun.args=xxx)
basevp+stat_summary(fun.data =mean_se,geom="errorbar",width=0.3)+#and the mean
  stat_summary(fun=mean,geom="point") 
#by defualt it only shows onoe standard erro, but we can show more by changing fun.args
basevp+stat_summary(fun.data =mean_se,geom="errorbar",width=0.3,fun.args=list(mult=1.96))+#this makes in 95% CI
  stat_summary(fun=mean,geom="point")+
  stat_summary(fun=mean,geom="line") 

vp<-basevp+stat_summary(fun.data =mean_se,geom="errorbar",width=0.3)+#lets remove the point fromo the legend.
  stat_summary(fun=mean,geom="point",show.legend = F) 

# 9. Customize colors -----------------------------------------------------
#now we got our violin plot let's change colors/fill
#scale_fill_manual()
#name="what we want the legend name"
#values=vector of the colors
#breaks= vector of the order of things
#labels= what to be shown
#http://sape.inf.usi.ch/quick-reference/ggplot2/colour
vp2<-vp+scale_fill_manual(name="Tide Height:",values=c("cyan2","plum3"),breaks=c("low","high"),labels=c("Low","High"))
#important note...this cannoot change the order of things!
# 10. Change axis ticks ---------------------------------------------------
#similar to how we changed color
#all with the scale_..._...() functions
#so change discrete x axis:
vp2+scale_x_discrete(breaks=c("low","high"),labels=c("Low","high"))
#change continous y axis
#limits=c(min,max)
vp2+scale_y_continuous(breaks=c(0,5,10,15,20),limits=c(0,20))
vpfinal<-vp2+scale_x_discrete(breaks=c("low","high"),labels=c("Low","high"))+scale_y_continuous(breaks=c(0,5,10,15,20),limits=c(0,20))

vpfinal
