#Multidimensional scaling and correspondence analysis 
#of powerlifting dataset in different weight classes

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

lifts <- lifts[lifts$event == 'SBD',]
lifts <- lifts[lifts$place != 'DQ',]
lifts <- lifts[lifts$meet_name == 'World Masters Powerlifting Championships',]
lifts

str(lifts)

#Checking NAs
sapply(lifts, function(x) sum(is.na(x)))

#Removing NAs
lifts <- lifts %>% drop_na(best3squat_kg)
lifts <- lifts %>% drop_na(best3bench_kg)
lifts <- lifts %>% drop_na(best3deadlift_kg)
lifts <- lifts %>% drop_na(bodyweight_kg)

sapply(lifts, function(x) sum(is.na(x)))

#Setting male and female datasets
table(lifts$sex)
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
lifts_m <- lifts_m[,c(1,8,9,10,11,12,14)]
colnames(lifts_m) <- c('Name','Bodyweight','Weight_class','Squat','Bench','DL','Date')

lifts_m

##Female class
lifts_f <- lifts_f[,c(1,8,9,10,11,12,14)]
colnames(lifts_f) <- c('Name','Bodyweight','Weight_class','Squat','Bench','DL','Date')

lifts_f

#_______________________________________________________________________________
#Wilks coefficient

##Coefficients vectors
a_men <- c(47.46178854,
           8.472061379,
           0.07369410346,
           -0.001395833811,
           7.07665973070743e-6,
           -1.20804336482315e-8)

a_female <- c(-125.4255398,
              13.71219419,
              -0.03307250631,
              -0.001050400051,
              9.38773881462799e-6,
              -2.3334613884954e-8)


##Wilks coefficient - men
wilks_men <- function(total, bodyweight) {
  coefficient <- a_men[1]
  for (i in 2:length(a_men)) {
    coefficient <- coefficient + (a_men[i] * (bodyweight ^ (i-1)))
  }
  600 / coefficient
}

##Wilks coefficient - female
wilks_female <- function(total, bodyweight) {
  coefficient <- a_female[1]
  for (i in 2:length(a_female)) {
    coefficient <- coefficient + (a_female[i] * (bodyweight ^ (i-1)))
  }
  600 / coefficient
}

#Calculating Wilks score for all competitors
##Men
for (i in 1:nrow(lifts_m)){
  lifts_m$Wilks[i] <- wilks_men(sum(lifts_m[i,c(4,5,6)]),as.numeric(lifts_m$Bodyweight)) * sum(lifts_m[i,c(4,5,6)])
}

lifts_m

##Women
for (i in 1:nrow(lifts_f)){
  lifts_f$Wilks[i] <- wilks_female(sum(lifts_f[i,c(4,5,6)]),as.numeric(lifts_f$Bodyweight)) * sum(lifts_f[i,c(4,5,6)])
}

lifts_f

#_______________________________________________________________________________
#Picking the best scores for each weight class over all events

##Men
men_best <- lifts_m %>% 
  group_by(Weight_class) %>%
  filter(Wilks == max(Wilks))

men_best <- men_best %>% arrange(Name,Date)
print(men_best,n = nrow(men_best))

###Removing duplicates
men_best <- men_best[c(-3,-10),]
men_best

##Female
female_best <- lifts_f %>% 
  group_by(Weight_class) %>%
  filter(Wilks == max(Wilks))

female_best <- female_best %>% arrange(Name,Date)
print(female_best,n = nrow(female_best))

#_______________________________________________________________________________
#Multidimensional scaling

##Men
###Distance matrix
d <- dist(men_best)
d

#Plot
cmds <- cmdscale(d, k=2, add=T, eig =T, x.ret=T)
x <- cmds$points [ ,1]
y <- cmds$points [ ,2]
plot(x, y, type = 'n')
text(x, y, labels=rownames(men_best))

#Groups
g1 <- text(x[3], y[3], col = 'red', labels = 3)
g2 <- text(x[c(5,16,7)], y[c(5,16,7)], col = 'blue', labels = c(5,16,7))
g3 <- text(x[c(11,13,1,6,8)], y[c(11,13,1,6,8)], col = 'green', labels = c(11,13,1,6,8))
g4 <- text(x[c(2,10,14,4,15,12)], y[c(2,10,14,4,15,12)], col = 'orange', labels = c(2,10,14,4,15,12))
g5 <- text(x[18], y[18], col = 'purple', labels = 18)
g6 <- text(x[17], y[17], col = 'black', labels = 17)
g7 <- text(x[9], y[9], col = 'brown', labels = 9)

##Female
###Distance matrix
d <- dist(female_best)
d

#Plot
cmds <- cmdscale(d, k=2, add=T, eig =T, x.ret=T)
x <- cmds$points [ ,1]
y <- cmds$points [ ,2]
plot(x, y, type = 'n')
text(x, y, labels=rownames(female_best))

#Groups
g1 <- text(x[c(9,13)], y[c(9,13)], col = 'red', labels = c(9,13))
g2 <- text(x[8], y[8], col = 'blue', labels = 8)
g3 <- text(x[c(1,6)], y[c(1,6)], col = 'green', labels = c(1,6))
g4 <- text(x[c(2,11)], y[c(2,11)], col = 'orange', labels = c(2,11))
g5 <- text(x[c(5,4,14,12,15,16,7)], y[c(5,4,14,12,15,16,7)], col = 'purple', labels = c(5,4,14,12,15,16,7))
g6 <- text(x[c(10,17)], y[c(10,17)], col = 'black', labels = c(10,17))
g7 <- text(x[3], y[3], col = 'brown', labels = 3)

#_______________________________________________________________________________
#Correspondence analysis
##Heatmaps for best men and female in each weight class

###Men
men_best_m <- as.matrix(men_best[1:nrow(men_best),as.numeric(c(4,5,6))])
rownames(men_best_m) <- men_best$Name

####Chisq test for men best
chisq.test(men_best_m)

####Heat-map
P = men_best_m/sum(men_best_m)
PP = outer(rowSums(P),colSums(P))
E = (P-PP)/sqrt(PP)
head(E)

heatmap(E,scale="none",Colv=NA,col= colors)

###Female
female_best_m <- as.matrix(female_best[1:nrow(female_best),as.numeric(c(4,5,6))])
rownames(female_best_m) <- female_best$Name

####Chisq test for female best
chisq.test(female_best_m)

####Heat-map
P = female_best_m/sum(female_best_m)
PP = outer(rowSums(P),colSums(P))
E = (P-PP)/sqrt(PP)
head(E)

heatmap(E,scale="none",Colv=NA,col= colors)

#_______________________________________________________________________________
#Plots for Correspondence-analysis (best in each weight class)

##Men
plot(ca::ca(men_best_m),
     xlab = "",
     ylab = "")

##Female
plot(ca::ca(female_best_m),
     xlab = "",
     ylab = "")

#_______________________________________________________________________________
#Picking data for best: men 82.5kg class and female 52Kg class in 2007-06-10 event
sort(table(lifts$date))

nrow(lifts_m[lifts_m$Date == '2007-06-10',])
nrow(lifts_f[lifts_f$Date == '2007-06-10',])

lifts_m <- lifts_m[lifts_m$Date == '2007-06-10',]
lifts_f <- lifts_f[lifts_f$Date == '2007-06-10',]

sort(table(lifts_m$Weight_class))
sort(table(lifts_f$Weight_class))

##Men
men_100 <- lifts_m %>% 
  filter(Weight_class == '100') %>%
  arrange(desc(Wilks))

men_100

##Female
female_100 <- lifts_f %>%
  filter(Weight_class == "67.5") %>%
  arrange(desc(Wilks))

female_100

men_100_m <- as.matrix(men_100[1:nrow(men_100),as.numeric(c(4,5,6))])
female_100_m <- as.matrix(female_100[1:nrow(female_100),as.numeric(c(4,5,6))])

rownames(men_100_m) <- men_100$Name
rownames(female_100_m) <- female_100$Name

men_100_m
female_100_m

#_______________________________________________________________________________
#Multidimensional scaling

##Men
###Distance matrix
d <- dist(men_100)
d

#Plot
cmds <- cmdscale(d, k=2, add=T, eig =T, x.ret=T)
x <- cmds$points [ ,1]
y <- cmds$points [ ,2]
plot(x, y, type = 'n')
text(x, y, labels=rownames(men_100))

#Groups
g1 <- text(x[1], y[1], col = 'red', labels = 1)
g2 <- text(x[2], y[2], col = 'blue', labels = 2)
g3 <- text(x[19], y[19], col = 'green', labels = 19)
g4 <- text(x[27], y[27], col = 'orange', labels = 27)
g5 <- text(x[31], y[31], col = 'purple', labels = 31)
g6 <- text(x[34], y[34], col = 'black', labels = 34)
g7 <- text(x[c(3,7,10,9,8)], y[c(3,7,10,9,8)], col = 'brown', labels = c(3,7,10,9,8))
g8 <- text(x[c(26,30)], y[c(26,30)], col = 'pink', labels = c(26,30))
g9 <- text(x[c(14,15)], y[c(14,15)], col = 'grey', labels = c(14,15))
g10 <- text(x[c(29,32,33)], y[c(29,32,33)], col = 'lightblue', labels = c(29,32,33))
g11 <- text(x[c(4,5,6)], y[c(4,5,6)], col = 'lightgreen', labels = c(4,5,6))
g12 <- text(x[c(12,11,13)], y[c(12,11,13)], col = 'yellow', labels = c(12,11,13))
g13 <- text(x[c(23,22,25,28,21,20,18,16,17,24)], y[c(23,22,25,28,21,20,18,16,17,24)], col = 'darkblue', labels = c(23,22,25,28,21,20,18,16,17,24))

##Female
###Distance matrix
d <- dist(female_100)
d

#Plot
cmds <- cmdscale(d, k=2, add=T, eig =T, x.ret=T)
x <- cmds$points [ ,1]
y <- cmds$points [ ,2]
plot(x, y, type = 'n')
text(x, y, labels=rownames(female_100))

#Groups
g1 <- text(x[1], y[1], col = 'red', labels = 1)
g2 <- text(x[c(2,3)], y[c(2,3)], col = 'blue', labels = c(2,3))
g3 <- text(x[6], y[6], col = 'green', labels = 6)
g4 <- text(x[11], y[11], col = 'orange', labels = 11)
g5 <- text(x[c(7,9,10)], y[c(7,9,10)], col = 'purple', labels = c(7,9,10))
g6 <- text(x[c(4,5)], y[c(4,5)], col = 'black', labels = c(4,5))
g7 <- text(x[8], y[8], col = 'brown', labels = 8)


#_______________________________________________________________________________
#Correspondence analysis
##Heatmaps for best: men 82.5kg class and female 52Kg class in 2007-06-10 event

###Men
####Chisq test for men best
chisq.test(men_100_m)

####Heat-map
P = men_100_m/sum(men_100_m)
PP = outer(rowSums(P),colSums(P))
E = (P-PP)/sqrt(PP)
head(E)

heatmap(E,scale="none",Colv=NA,col= colors)

###Female
####Chisq test for female best
chisq.test(female_100_m)

####Heat-map
heatmap(female_100_m,scale="none",Colv=NA,col= colors)

#_______________________________________________________________________________
#Plots for best: men 82.5kg class and female 52Kg class in 2007-06-10 event
par(bty = 'n',yaxt="n",xaxt="n")

##Men
plot(ca::ca(men_100_m),
     xlab = "",
     ylab = "")

##Female
plot(ca::ca(female_100_m),
     xlab = "",
     ylab = "")
