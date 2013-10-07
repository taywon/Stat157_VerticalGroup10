#Stat 157 HW2 for Questionnaire
#Done by Sam Kirschner, in the Visualizer role.
#Part of Vertical Team 10, with Tay Won Shin, Disi Koa, and Hong Chan Shon
#NOTE: For the sake of reproducibility, this is using version 2.5.1 of the R programming language.
#Additionally, all the code is generalized so that you can understand how you would implement this
#for larger sets of data when trying to visualize the same characteristics

#Load the data from your machine (here, cleaned by Disi)
load("C:/Users/Sam Kirschner/Downloads/stat157dataset.RData")



###Learning Style Visualization###

#Select out the columns for learning style, with integers for scores
visual=nnd[[3]]
aural=nnd[[4]]
readWrite=nnd[[5]]
kinesthetic=nnd[[6]]     

#Go through each person's responses, and figure out what their strongest score was for. The number in the vector is visual,
#the second is aural, the third is readWrite, and the fourth is kinesthetic. 

#Find Highest score for each person
topStyles=c()

#Also track how many programming languages each learning type knows
languages=nnd[[2]] 
langNumbers=c()

#Iterate through all the responses, and record which is the maximum score for each respondent
for(response in 1:length(visual)){
  row=c(visual[response],aural[response],readWrite[response],kinesthetic[response])
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



###Programming Language and Learning Style Visualization###

#A plot of learning styles versus the number of languages people know can be telling too
#You can see that the spread isn't really dependent on learning style (aural doesn't have many students or observations)
plotTitle="Coding Languages Known Per Learning Type"
xLabel="Visual, Aural, Reading/Writing, and Kinesthetic Learning Styles"
yLabel="Number of Programming Languages Known"
plot(topStyles,langNumbers,main=plotTitle,xlab=xLabel,ylab=yLabel)



###Programming Language Visualization###

#This part requires the wordcloud package to be installed to R. You can do so using the following command:
#install.packages("wordcloud")
#Load it using the following command:
#library("wordcloud", lib.loc="C:/Users/Sam Kirschner/Documents/R/win-library/2.15")
#You may also have to download other packages to support this, and you can do it in a similar fashion.

#Select out the languages from the data
languagesList=DATA[[3]]
languagesVector=c()
for(line in 1:length(languagesList)){
  #Split the respones by the commas seperating the languages
  langs=as.vector(strsplit(x=as.matrix(languagesList[line]),split=","))
  print(langs[[1]])
  #make sure to clean out any leftover spaces
  langs=gsub(" ","",langs[[1]])
  languagesVector=c(languagesVector,langs)
}

#now, find the subset of all the unique languages that people know, and their frequencies among students
uniqueLangs=unique(languagesVector)
uniqueFreqs=c()
for(lang in uniqueLangs){
  uniqueFreqs=c(uniqueFreqs,sum(languagesVector==lang))
}

#We can then generate a word cloud using the words and their frequencies, which gives you a sense by size
#of how much the class understands each language. Everytime you run this, the cloud looks different too
wordcloud(uniqueLangs,uniqueFreqs,min.freq=1)

#A pie chart also does a good job of showing what chunk of the class knows what language
#Use cex to alter the size of the labels if they get jumbled on top of each other
pieTitle="Frequency of Programming Languages"
pie(uniqueFreqs,uniqueLangs,main=pieTitle,cex=.6)





