#Stat 157 HW2 for Questionnaire
#Done by Sam Kirschner, in the Visualizer role.
#Part of Vertical Team 10, with Tay Won Shin, Disi Koa, and Hong Chan Shon
#NOTE: For the sake of reproducibility, this is using version 2.5.1 of the R programming language.
#Additionally, all the code is generalized so that you can understand how you would implement this
#for larger sets of data when trying to visualize the same characteristics

#Load the data as cleaned by Disi
load("C:/Users/Sam Kirschner/Downloads/stat157dataset.RData")


###Learning Style Visualization###
#Select out the columns for learning style, with integers for scores
     
visual=nnd[[3]]
aural=nnd[[4]]
readWrite=nnd[[5]]
kinesthetic=nnd[[6]]
     
#Find Highest score for each person
topStyles=c()

#Go through each person's responses, and figure out what their strongest score was for. The number in the vector is visual,
#the second is aural, the third is readWrite, and the fourth is kinesthetic. 
for(response in 1:length(visual)){
  
  row=c(visual[response],aural[response],readWrite[response],kinesthetic[response])
  topLearningStyle=which(row==max(row))
  topStyles=c(topStyles,topLearningStyle)
}
#Choose some pretty colors that work well together for a histogram
turqoise=colors()[8]
blue=colors()[122]
purple=colors()[96]
red=colors()[448]
#plot them together on a histogram so you can see the spread of people's learning style
hist(topStyles,0:length(unique(topStyles)),col=c(turqoise,blue,purple,red))


###Programming Language Visualization###
#This part requires the wordcloud package to be installed to R. You can do so using the following command:
#install.packages("wordcloud")
languagesList=DATA[[3]]
languagesVector=c()
for(line in 1:length(languagesList)){
  langs=as.vector(strsplit(x=as.matrix(languagesList[line]),split=","))
  print(langs[[1]])
  langs=gsub(" ","",langs[[1]])
  languagesVector=c(languagesVector,langs)
}
uniqueLangs=unique(languagesVector)
uniqueFreqs=c()
for(lang in uniqueLangs){
  uniqueFreqs=c(uniqueFreqs,sum(languagesVector==lang))
}
wordcloud(uniqueLangs,uniqueFreqs)
     
