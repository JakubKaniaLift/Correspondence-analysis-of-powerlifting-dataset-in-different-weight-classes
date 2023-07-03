#Correspondence-analysis-of-powerlifting-dataset-in-different-weight-classes

#Needed libraries
library("MASS")
library("ca")
library("RColorBrewer")
library("tidyverse")
library("dplyr")
library("ggplot2")

colors <- rev(brewer.pal(11,"Spectral")) #vector of colors

#Loading data
lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")
lifts
str(lifts)

#Setting male and female datasets
lifts_m <- lifts[lifts$sex=='M',]
lifts_f <- lifts[lifts$sex=='F',]

#Checking the amount of competitors in every weight class
table(lifts_m$weight_class_kg)
table(lifts_f$weight_class_kg)

#Sorting tables to see the amount of participants in each weight class
sort(table(lifts_m$weight_class_kg))
sort(table(lifts_f$weight_class_kg))

#Picking columns
##Men class
lifts_m <- lifts_m[,c(1,8,9,10,11,12)]
colnames(lifts_m) <- c('Name','Bodyweight','Weight_class','Squat','Bench','DL')

###Cleaning data from NAs
lifts_m <- na.omit(lifts_m)

###Head
head(lifts_m)

##Female class
lifts_f <- lifts_f[,c(1,8,9,10,11,12)]
colnames(lifts_f) <- c('Name','Bodyweight','Weight_class','Squat','Bench','DL')

###Cleaning data from NAs
lifts_f <- na.omit(lifts_f)

###Head
head(lifts_f)

#_______________________________________________________________________________
#Wilks coefficient
##Coefficients vectors
a_men <- c(-216.0475144, 16.2606339, -0.002388645,
           -0.00113732, 7.01863E-06, -1.291E-08)

a_female <- c(594.31747775582, -27.23842536447, 0.82112226871,
             -0.00930733913, 0.00004731582, -0.00000009054)


#Wilks coefficient - men
wilks_men <- function(total, bodyweight) {
  coefficient <- a_men[1]
  for (i in 2:length(a_men)) {
    coefficient <- coefficient + (a_men[i] * (bodyweight ^ (i-1)))
  }
  500 / coefficient
}

#Wilks coefficient - female
wilks_female <- function(total, bodyweight) {
  coefficient <- a_female[1]
  for (i in 2:length(a_female)) {
    coefficient <- coefficient + (a_female[i] * (bodyweight ^ (i-1)))
  }
  500 / coefficient
}

#Calculating Wilks score for all competitors
##Men
for (i in 1:nrow(lifts_m)){
  lifts_m$Wilks[i] <- wilks_men(sum(lifts_m[i,c(4,5,6)]),as.numeric(lifts_m$Bodyweight)) * sum(lifts_m[i,c(4,5,6)])
}

head(lifts_m,10)

##Women
for (i in 1:nrow(lifts_f)){
  lifts_f$Wilks[i] <- wilks_female(sum(lifts_f[i,c(4,5,6)]),as.numeric(lifts_f$Bodyweight)) * sum(lifts_f[i,c(4,5,6)])
}

head(lifts_f,10)

#_______________________________________________________________________________
#Picking the best scores for each weight class
##Men

men_best <- lifts_m %>% 
  group_by(Weight_class) %>%
  filter(Wilks == max(Wilks))

men_best <- men_best %>% arrange(Bodyweight)
print(men_best,n = nrow(men_best))

##Female
female_best <- lifts_f %>% 
  group_by(Weight_class) %>%
  filter(Wilks == max(Wilks))

female_best <- female_best %>% arrange(Bodyweight)
print(female_best,n = nrow(female_best))

#_______________________________________________________________________________
#Heatmaps for men and female lifts
##Men
men_best_m <- as.matrix(men_best[1:nrow(men_best),as.numeric(c(4,5,6))])

###Chisq test for men best
chisq.test(men_best_m)

###Heat-map
P = men_best_m/sum(men_best_m)
PP = outer(rowSums(P),colSums(P))
E = (P-PP)/sqrt(PP)
head(E)

heatmap(E,scale="none",Colv=NA,col= brewer.pal(8,"Blues"))

##Female
female_best_m <- as.matrix(female_best[1:nrow(female_best),as.numeric(c(4,5,6))])

###Chisq test for female best
chisq.test(female_best_m)

###Heat-map
heatmap(female_best_m,scale="none",Colv=NA,col= brewer.pal(8,"Blues"))

#_______________________________________________________________________________
#Plots for Correspondence-analysis
par(bty = 'n',yaxt="n",xaxt="n")

##Men
plot(ca::ca(men_best_m),
     col = men_best$Weight_class,
     xlab = "",
     ylab = "")

##Female
plot(ca::ca(female_best_m),
     col = female_best$Weight_class,
     xlab = "",
     ylab = "")