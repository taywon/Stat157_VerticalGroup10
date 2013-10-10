##################################################################
### Introduction                                                 #
#           Group 10                                             #
#           Disi Koa : Administrator (Curator)                   #
#           Tay Shin :Producer (Analyzer)                        #
#           Sam Kirschner : Entrepreneur (Visualizer)            #
#           Hong Chan Shon :Integrator (Presenter)               #
#                                                               #
# You DON'T have to download the data. This code will scrap      #
# the data from google spread sheet. For the sake of             #
# reproducibility, please use most updated version of the        #
# R programming language.                                        #
##################################################################

##################################################################
### Download requried packages                                   #
#                                                                #
#  Important: once you run this section, do not run it again     #
#  1.'RCurl' package will allow us to access Google spreadsheet  #
#  2.'worldcloud' package will provide our wordcolud plot        #
##################################################################

install.packages('RCurl')     #**once you run, please do not run this line again.
install.packages('wordcloud') #**once you run, please do not run this line again.

##################################################################
### Install requried packages & Download the data                #
#                                                                #
#  1.install package into local R repository                     #
#  2.obtain a data from google spread sheet                      #
##################################################################

library(RCurl)            #loading packages
library(wordcloud)        #loading packages
options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'Rcurl')))
url.="https://docs.google.com/spreadsheet/pub?key=0Amx5sIkwVHb7dG1oWVVOUlVRdDZ3aUt6c2RLUFBLVkE&output=csv"
myCsv = getURL(url.,cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
DATA = read.csv(textConnection(myCsv)) #download the data

##################################################################
### Function Declare                                             #
#                                                                #
# Functions that we made for specific propose                    #
# 1.datacleaning                                                 #
# 2.counting numbers of languages you know                       #
# 3.assign numerical value to Always, often, not often, never    #
##################################################################

datacleaning=function(value){
  temp2=0
  for(i in 1:48){
    temp=unlist(strsplit(as.character(DATA$What.is.your.learning.style.)[i],split="\n"))
    if(length(temp)==0){
      temp2[i]=-1
    }else if(length(temp)>30){
      temp2[i]=-1
    }else if(length(temp)<3){
      temp2[i]=-1
    }else{
      ind=grep(pattern=value,x=temp)
      if(length(ind)!=0){
        temp2[i]=temp[ind][1]
      }else{
        temp2[i]=-1
      }
    }
  }
  return(temp2)
}

extract=function(data){
  out=0
  for(i in 1:48){
    temp=unique(strsplit(data,split=" ")[[i]])[length(unique(strsplit(data,split=" ")[[i]]))]
    if(is.na(temp)==1){
      out[i]=-1
    }else{
      out[i]=temp
    }
  } 
  out2=as.numeric(out)
  return(out2)
}


assignValue=function(data){
  nd=matrix(0,48,4)
  for(i in 1:48){
    for(k in 1:4){
      if(data[i,k]=='Always'){
        nd[i,k]=(4)
      }else if(data[i,k]=='Often'){
        nd[i,k]=(3)
      }else if(data[i,k]=='Sometimes'){
        nd[i,k]=(2)
      }else if(data[i,k]=='Not Often'){
        nd[i,k]=(1)
      }else if(data[i,k]=='Never'){
        nd[i,k]=(0)
      }
    }
  }
  producer=nd[,1]
  administrator=nd[,2]
  entrepreneur=nd[,3]
  integrator=nd[,4]
  return(data.frame(producer,administrator,entrepreneur,integrator))
}

##################################################################
### Data cleaning                                                #
#                                                                #
# Columns that we need are                                       #
# 1.What computer langagues do you know                          #
# 2.What is your learning style                                  #
##################################################################

#subsetting the data
sub_data=cbind(as.character(DATA$What.computer.language.s..do.you.know.),
               as.character(DATA$What.is.your.learning.style.))

#counting numbers of langagues you know and save it to new.lang
new.lang=0
for(i in 1:48){ 
  new.lang[i]=length(unlist(strsplit(sub_data[i],split=',')))
}

#using the function that we built. 
#It detects input value and outputs corresponding numerical value
new.v=datacleaning('Visual')
new.a=datacleaning('Aural')
new.r=datacleaning('Read')
new.k=datacleaning('Kinesthetic')

#using the function that we built. It cleans the data
c.new.v=extract(new.v)
c.new.a=extract(new.a)
c.new.r=extract(new.r)
c.new.k=extract(new.k)

#subsetting the row which has appropriate value. By using above two functions, I denote 
#the bad row as -1. So By doing dot product of index where the value if -1, we get 
#a final index that considers all bad rows
ind=as.logical(as.numeric(c.new.v==-1)*
                 as.numeric(c.new.a==-1)*
                 as.numeric(c.new.r==-1)*
                 as.numeric(c.new.k==-1))


#subsetting the data
sub_data2=cbind(as.character(DATA$How.often.do.you.take.the.following.roles.in.group.projects...A.producer..This.person.knows.how.to.get.the.job.done.A.a0..),
                as.character(DATA$How.often.do.you.take.the.following.roles.in.group.projects...An.administrator..He.or.she.is.able.to.plan.and.organize..),
                as.character(DATA$How.often.do.you.take.the.following.roles.in.group.projects...An.integrator..This.person.can.take.an.individual.goal.and.transform.it.into.a.group.goal.A.a0..),
                as.character(DATA$How.often.do.you.take.the.following.roles.in.group.projects...An.entrepreneur..This.individual.has.vision.and.creative.problem.solving.abilities..))

#Final data set
F_dset=data.frame(new.lang,c.new.v,c.new.a,c.new.r,c.new.k,assignValue(sub_data2))[!ind,]


##################################################################
### Data Analysis  + little bit of visualization                 #
#                                                                #
# Do simple and multi- linear regression to see if there is      #
# linear relationship with numbers of languages you know and     #
# your learning style. Futhermore, we used stepwise method to    #
# see if there are other vairables that could have               #
# linear relationship with numbers of langagues you know         # 
##################################################################

#set location
loc <- par("usr")

#model1 regress new.lang on c.new.v
mod1=lm(new.lang~c.new.v,data=F_dset)
summary(mod1)
plot(new.lang~c.new.v,data=F_dset,ylab="Lang",xlab="Visual",pch=16,col='blue')
title("Languages vs Visual", sub = "P-value 0.2872 | Corr=0.1973719",
      cex.main = 2,   font.main= 4, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")
abline(mod1,col='red') #fitting regression line on our plot

#model2 regress new.lang on c.new.a
mod2=lm(new.lang~c.new.a,data=F_dset)
summary(mod2)
plot(new.lang~c.new.a,F_dset,ylab="Lang",xlab="Aural",col='blue',pch=16)
title("Languages vs Aural", sub = "P-value 0.5748 | Corr=0.1047782",
      cex.main = 2,   font.main= 4, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")
abline(mod2,col='red') #fitting regression line on our plot

#model3 regress new.lang on c.new.r
mod3=lm(new.lang~c.new.r,data=F_dset)
summary(mod3)
plot(new.lang~c.new.r,F_dset,ylab="Lang",xlab="Read/Write",col='blue',pch=16)
title("Languages vs Read/write", sub = "P-value 0.7804 | Corr=0.05218476",
      cex.main = 2,   font.main= 4, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")
abline(mod3,col='red') #fitting regression line on our plot

#model4 regress new.lang on c.new.k
mod4=lm(new.lang~c.new.k,data=F_dset)
summary(mod4)
plot(new.lang~c.new.k,F_dset,ylab="Lang",xlab="Kinesthetic",col='blue',pch=16)
title("Languages vs Kinesthetic", sub = "P-value 0.5328 | Corr=0.11642303",
      cex.main = 2,   font.main= 4, col.main= "black",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")
abline(mod4,col='red') #fitting regression line on our plot

#model5 regress new.lang on c.new.v+c.new.a+c.new.r+c.new.k
mod5=lm(new.lang~c.new.v+c.new.a+c.new.r+c.new.k,data=F_dset)
summary(mod5)

#using stepwise method to find the variables. In particular, trying to find 
#linear combination of the variables which have strong relationship with 
#number of languages you know. 
mod0=lm(new.lang~-1,data=F_dset)
mod6=lm(new.lang~.,data=F_dset)
B=step(mod0, scope=list(lower=mod0, upper=mod6), direction="forward",data=F_dset,steps=20)
summary(B) #model is significant. 

##################################################################
### Visualization Overview                                              #
#                                                                #
# All the code is generalized so that you can understand         #
# how you would implement this for larger sets of data           #
# when trying to visualize the same characteristics              #
##################################################################


##################################################################
### Visualization -Histogram                                     #
#                                                                #
# Go through each person's responses, and figure out what        #
# their strongest score was for. The number in the vector is     #
# visual,the second is aural, the third is readWrite, and        #
# the fourth is kinesthetic.                                     #
##################################################################


#Find Highest score for each person
topStyles=c()

#Also track how many programming languages each learning type knows
languages=F_dset$new.lang 
langNumbers=c()

#Iterate through all the responses, and record which is the maximum score for each respondent
for(response in 1:length(F_dset$c.new.v)){
  row=c(F_dset$c.new.v[response],F_dset$c.new.a[response],F_dset$c.new.r[response],F_dset$c.new.k[response])
  topStyles=c(topStyles,which(row==max(row)))
  langNumbers=c(langNumbers,rep(languages[response],length(which(row==max(row)))))
}

#Choose some pretty colors that work well together for a histogram
turqoise=colors()[8]
blue=colors()[122]
purple=colors()[96]
red=colors()[448]

#Also label the learning style histogram
styleLabels=c("visual","aural","reading/ writing","kinesthetic")
styleTitle="Histogram of Top Learning Styles in Stat 157"
xStyle="Learning Style"

#plot them together on a histogram so you can see the spread of people's learning style
hist(topStyles,0:length(unique(topStyles)),col=c(turqoise,blue,purple,red),labels=styleLabels,ylim=c(0,20),main=styleTitle,xlab=xStyle)


##################################################################
### Visualization -wordcloud + piechart                          #
#                                                                #
# A plot of learning styles versus the number of languages       # 
# people know can be telling too. You can see that the spread    # 
# isn't really dependent on learning style. (aural doesn't       #
# have many students or observations)                            #
##################################################################

plotTitle="Coding Languages Known Per Learning Type"
xLabel="Visual, Aural, Reading/Writing, and Kinesthetic Learning Styles"
yLabel="Number of Programming Languages Known"
plot(topStyles,langNumbers,main=plotTitle,xlab=xLabel,ylab=yLabel,xaxt='n')

#Select out the languages from the data
languagesList=as.character(DATA$What.computer.language.s..do.you.know.)
languagesVector=c()
for(line in 1:length(languagesList)){
  #Split the respones by the commas seperating the languages
  langs=as.vector(strsplit(x=as.matrix(languagesList[line]),split=","))
  print(langs[[1]])
  #make sure to clean out any leftover spaces
  langs=gsub(" ","",langs[[1]])
  languagesVector=c(languagesVector,langs)
}

#now, find the subset of all the unique languages that people know, 
#and their frequencies among students
uniqueLangs=unique(languagesVector)
uniqueFreqs=c()
for(lang in uniqueLangs){
  uniqueFreqs=c(uniqueFreqs,sum(languagesVector==lang))
}

#We can then generate a word cloud using the words and their frequencies, 
#which gives you a sense by size of how much the class understands each 
#language. Everytime you run this, the cloud looks different too
wordcloud(uniqueLangs,uniqueFreqs,min.freq=1)

#A pie chart also does a good job of showing what chunk of the class knows what language
#Use cex to alter the size of the labels if they get jumbled on top of each other
pieTitle="Frequency of Programming Languages"
pie(uniqueFreqs,uniqueLangs,main=pieTitle,cex=.6)
