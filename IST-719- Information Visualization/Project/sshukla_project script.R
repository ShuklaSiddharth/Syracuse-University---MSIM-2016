#
#IST-719
#Information Visualization
#Author: Siddharth Shukla
#Purpose: Final project
#

#Total 21 types of Events
#DownloadEvent, FollowEvent, ForkEvent, GistEvent,GollumEvent, IssueEvent, PublicEvent, WatchEvent

#Reading Events csv file
Events.List <- read.csv("\\\\hd.ad.syr.edu\\03\\ba402c\\Documents\\Siddharth\\Info Viz\\Project\\EventsActivity.csv")

View(Events.List)

Events.List

str(Events.List)

Events.List$Year <- as.factor(Events.List$Year)



#Re-ordering factor levels for Year & Event in a certatain order

Events.List$Event = factor(Events.List$Event
                          ,levels(Events.List$Event)[c(2,3,9,7,5,8,4,6,10,1)]
                            )

Events.List$Year = factor(Events.List$Year
                          ,levels(Events.List$Year)[c(5,4,3,2,1)]
                          )

str(Events.List)


max(Events.List$Count)

#Plot 1 -  Radial Bar Chart for Event count
install.packages("ggplot2")
library(ggplot2)
library(RColorBrewer)
library(gcookbook)
library(plyr)



# Circular one
options(scipen=999)
ggplot(Events.List, aes(x = Event, y = Count ,fill = Year)) + 
  geom_bar(width = .75, stat="identity") +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,max(Events.List$Count)+40000000))+
  geom_text(data = Events.List, hjust = 1.25, size = 3, aes(x = Event, y = 0, label = Event)) +
  theme(axis.text.y = element_blank() , axis.ticks = element_blank())+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  #scale_fill_gradientn(colours = rainbow(5))




# Plot 2
Create.Event.List <- read.csv("\\\\hd.ad.syr.edu\\03\\ba402c\\Documents\\Siddharth\\Info Viz\\Project\\CreateEventCount.csv")

str(Create.Event.List)

View(Create.Event.List)

Create.Event.List$Year <- as.factor(Create.Event.List$Year)


Create.Event.Matrix <- tapply(Create.Event.List$Count
                            , list(Create.Event.List$CreateEventType, Create.Event.List$Year)
                            , FUN = sum
                            )

#line plot
plot(Create.Event.Matrix[1,]
     , type = "b"
     , ylim = c(0, 20000000)
     , col = "red"
     , lwd = 2
     , main = "Create event over time"
     , adj = 0
     , xlab = "Year"
     , ylab = 'Create Event Types'
     , xaxt = 'n'
     , border = FALSE
     , bty = 'n'
    )
lines(Create.Event.Matrix[2,], col = "blue", lwd = 2, type = "b")
lines(Create.Event.Matrix[3,], col = "green", lwd = 2, type = "b")


legend('topleft'
       , legend = rownames(Create.Event.Matrix)
       , lwd = 2
       , lty = 1
       , pch = 1
       , col = c("red","blue","green")
       , bty = 'n'
)

axis(1, at=c(1,2,3,4,5), labels = colnames(Create.Event.Matrix))


#barplot
barplot(Create.Event.Matrix
        , beside = T
        , col = c("red","blue","green")
        , ylim = c(0, 20000000)
        , legend.text = FALSE
)

legend('topleft'
       , legend = rownames(Create.Event.Matrix)
       , lwd = 2
       , pch = 15
       , col = c("red","blue","green")
       , bty = 'n'
)


#line shadow
Create.Event.List <- read.csv("\\\\hd.ad.syr.edu\\03\\ba402c\\Documents\\Siddharth\\Info Viz\\Project\\CreateEventCount.csv")
options(scipen=999)
ggplot(Create.Event.List, aes(x=Year, y=Count, fill=CreateEventType)) +
geom_area(colour="black", size=.2, alpha=.4) +
scale_fill_brewer(palette="Blues", breaks=rev(levels(Create.Event.List$CreateEventType))) +
theme_bw() +
theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
ylim(0,30000000)




#plot 3
#word colud
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
library(tm)
library(SnowballC)
library(wordcloud)

license <- read.csv("\\\\hd.ad.syr.edu\\03\\ba402c\\Documents\\Siddharth\\Info Viz\\Project\\licenses.csv"
                    ,stringsAsFactors = FALSE)

length(unique(license$license))

pal <- brewer.pal(9,"BuGn")

str(license)

View(license)

licenseCorpus <- Corpus(VectorSource(license$license))


wordcloud(licenseCorpus
          , random.order = FALSE
          , random.color = TRUE
          , scale = c(5,1)
          , colors = pal
)





#barplot
options(scipen=999)
barplot(table(license$license), horiz = TRUE)
axis(2, las =)



#heatmap
license.df <- as.data.frame(table(license$license))
license.df$finalname <- paste(license.df$Var1, license.df$Freq, sep = ", ")


install.packages("treemap")
library(treemap)

treemap(license.df
        ,index = c("finalname")
        ,vSize = "Freq"
        ,vColor = "Freq"
        , force.print.labels = TRUE
        )

legend('bottom'
       , legend = license.df$Var1
)


data("GNI2014")
View(GNI2014)

# total unique licenses = 15
#total unique languages = 320
install.packages("rjson")
library(rjson)

languages <- fromJSON(file = "\\\\hd.ad.syr.edu\\03\\ba402c\\Documents\\Siddharth\\Info Viz\\Project\\languages.json")


languages.dataframe <- as.data.frame(languages)


View(languages.dataframe)







#plot 4
#word colud
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
library(tm)
library(SnowballC)
library(wordcloud)

languages <- read.csv("\\\\hd.ad.syr.edu\\03\\ba402c\\Documents\\Siddharth\\Info Viz\\Project\\languages1.txt")


pal <- brewer.pal(9,"BuGn")

str(languages)

View(languages)

languageCorpus <- Corpus(VectorSource(languages))


wordcloud(languageCorpus
          , random.order = FALSE
          , random.color = TRUE
          , scale = c(5,1)
          , colors = pal
)
