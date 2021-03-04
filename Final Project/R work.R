getwd()
# Get data
setwd("C:/Users/aarme/OneDrive/Desktop/520/dsc520/exercises/Final Project")
dir()
MidEas <- read.csv("xAPI-Edu-Data.csv")
Port <- read.csv("student-por.csv")
Mat <- read.csv("student-mat.csv")
UniSta <- read.csv("StudentsPerformance.csv")
head(Port, 6)
library(dplyr)
library(ggplot2)
#ggplot(Port, aes(x= age,y = G3, color= school)) + geom_point()
# random exploration
GPS <- subset(Port, school== 'GP')
MSS <- subset(Port, school == 'MS')
mean(Port$G3)
mean(GPS$G3)
mean(MSS$G3)
GPM <- subset(Mat, school == 'GP')
MSM <- subset(Mat, school == 'MS')
mean(Mat$G3)
mean(GPM$G3)
mean(MSM$G3)
sum(GPM$reason == "reputation")
sum(MSS$reason == "reputation")
sum(GPS$reason == "reputation")
cor(Port[,31:33])
# convert to factors 
Port$guardian <- factor(Port$guardian, levels = c('other', 'mother', 'father'))
Port <- mutate_if(Port, is.character, as.factor)

cor(Port$G3, Port[sapply(Port, is.numeric)])
cor(Port$G3, as.numeric(Port$famsup))
cor.test(Port$G3, as.numeric(Port$schoolsup))
Port2 <- mutate_if(Port, is.factor, as.numeric)
cor(Port2$G3, Port2)
cor(Port2$G3, Port2)
Port2$outact <- (Port2$freetime + Port2$goout + Port2$Dalc + Port2$Walc)/4
cor()
MidEas$Grade <- sapply(strsplit(MidEas$GradeID, '-'), unlist)[2,] %>% as.numeric
MidEas$Class <- factor(MidEas$Class, levels = c('L', 'M', 'H')) %>% as.numeric
cor(MidEas$Class, MidEas$Grade)
x <- lm(G3 ~ address + Pstatus + Medu + Fedu + Mjob + Fjob + guardian + famrel, data = Port)
summary(x)
x2 <- lm(G3 ~ reason + traveltime + studytime + schoolsup + famsup + paid + nursery + higher + internet, data = Port)
summary(x2)
x3 <- lm(G3 ~ activities + romantic + freetime + goout + Dalc + Walc, data = Port )
summary(x3)
x3.1 <- lm(G3 ~ outact + romantic + activities, data = Port2)
summary(x3.1)
Port2 <- Port2[,-c(1:3,31,32)]

# correlations and maps
library(reshape)
cor(port_df$G3, port_df$Medu)
cor(usa_df.num$parental.level.of.education, usa_df.num$math.score)
cor(port_df.num$G3,port_df.num)
relmat <- cor(port_df.num, method = "kendall")
shap_relmat <- melt(relmat)

ggplot(data = shap_relmat, aes(x= X1, y= X2, fill= value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 10, hjust = 1)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")
shap_relmat2 <- subset(shap_relmat, value >.1 | value < -.1, select = 1:3)
ggplot(data = shap_relmat, aes(x= X1, y= X2, fill= value)) + geom_tile() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 10, hjust = 1)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Kendall\nCorrelation") 
ggplot(data = shap_relmat2, aes(x= X1, y= X2, fill= value)) + geom_tile() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 10, hjust = 1)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Kendall\nCorrelation")
usccor<- cor(usa_df.num, method = "kendall")
muscor <- melt(usccor)
ggplot(data = muscor, aes(x= X1, y= X2, fill= value)) + geom_tile() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 10, hjust = 1)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Kendall\nCorrelation") 
usccor
midcor <- round(cor(mideast_df.num, method = "kendal"), 3)
melmidcor <- melt(midcor)
ggplot(data = melmidcor, aes(x= X1, y= X2, fill= value)) + geom_tile() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 10, hjust = 1)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Kendall\nCorrelation") 
library(pastecs)
round(stat.desc(port_df.num,basic = F, norm = TRUE), 2)
plot(port_df$G3)
hist(port_df$G3)
boxplot(port_df$G3)
hist(mideast_df.num$Class)
hist(usa_df$math.score)
boxplot(usa_df$math.score)
plot(usa_df$math.score)
plot(usa_df$reading.score)
plot(usa_df$writing.score)
plot(math_df$G3)
hist(math_df$G3)
boxplot(math_df$G3)
hist(usa_df$avg.score)
plot(usa_df$avg.score)

mideast_df$class.part <- (mideast_df$raisedhands + mideast_df$VisITedResources + 
                            mideast_df$AnnouncementsView + mideast_df$Discussion)/4
mideast_df.num$class.part <- mideast_df$class.part
cor(mideast_df.num)

sum(port_df$Pstatus == "A")
sum(port_df$guardian == "other")
sum(port_df$Pstatus != "T" & port_df$guardian == "mother")
port_df$guardian <-ifelse(port_df$guardian != "other" & port_df$Pstatus == "T" , "both", as.character(port_df$guardian))
port_df$guardian <- factor(port_df$guardian, levels = c("other", "mother", "father", "both"))
port_df.num$guardian <- as.numeric(port_df$guardian)
cor(port_df.num)
x <- lm(G3 ~ guardian, data = port_df)
summary(x)
predict(x)
ggplot(data = port_df, aes(x= guardian,y= G3)) + geom_point(aes(y= predict(x)), color = "red") +
  geom_point()
library(class)
cc <- (knn.cv(port_df.num, port_df.num$G3, k= 3))
table(real = port_df$G3, pred = cc)
table(real = port_df$G3, as.numeric(cc)+4)
sum(port_df$G3 == cc)
((port_df.num$G3 - as.numeric(cc)))
port_df.num$pred <- as.numeric(ccc) +4
off <- port_df$G3 - cc
port_df.num$off <- port_df.num$G3 - (as.numeric(ccc) + 4)
sum(port_df.num$off^2 < 10)
ccc <- knn.cv(port_df.num[,21:27], cl = port_df.num$G3, k= 3)
colnames(port_df)
as.character(colnames(port_df))
bet <- lm(G3 ~ ., data = port_df )
summary(bet)
predict(bet)
table(real= port_df$G3, pred= round(predict(bet)))
better <- step(bet, direction = "both")
summary(better)
predict(better)
table(real = port_df$G3, pred= round(predict(better)))
full <- cor(port_df.num)
melfull <- melt(full)
ggplot(data = melfull, aes(x= X1, y= X2, fill= value)) + geom_tile() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 10, hjust = 1)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Kendall\nCorrelation") 
cor(port_df.num$age, port_df.num)
ggplot(data = port_df, aes(x= age, y= G3)) +geom_count(color = 'blue', shape= 18) + scale_size(name = 'count')
mm <- knn(port_df.num, math_df.num, cl= port_df$G3, k= 3)
table(real= math_df.num$G3, pred= mm)
cor(math_df$age, math_df$Dalc)
cor(math_df$age, math_df$G3)
cor(math_df$Dalc, math_df$G3)
port_df.num$other.act <- port_df.num$goout + port_df.num$traveltime + port_df.num$freetime + port_df.num$Dalc +
  port_df.num$Walc + port_df.num$romantic
cor.test(port_df.num$traveltime, port_df.num$studytime, method = "kendall")
cor.test(port_df.num$freetime, port_df.num$studytime,method = "kendall" )
cor.test(port_df.num$traveltime, port_df.num$freetime,method = "kendall" )
cor.test(port_df.num$age, port_df.num$traveltime, method = "kendall")
cor(port_df.num$age, port_df.num$other.act)
cor(port_df.num$other.act, port_df.num$G3)
cor.test(port_df.num$age, port_df.num$G3)
ggm::pcor.test(ggm::pcor(c("G3", "age", "other.act"),var(port_df.num)),1,649)
ggm::pcor(c("G3", "age", "other.act"),var(port_df.num))
ggm::parcor(port_df.num)
library(ggm)
v <- var(port_df.num)
parcor(v)
cor(port_df.num$studytime, port_df.num$otheract)
x<- lm(G3 ~., data = port_df)
table(real= port_df$G3, pred=round(predict(x)))
sum(port_df$G3 == round(predict(x)))
sum((port_df$G3 - round(predict(x)))^2 <= 9)
v <- knn.cv(port_df.num, cl= port_df.num$G3, k= 5)
cor(port_df.num$absences, port_df.num$traveltime)
cor(port_df.num$G3, port_df.num)
cor(port_df.num$address, port_df.num$traveltime)
pcor.test(pcor(c("traveltime","G3", "address"),var(port_df.num)),1,633)
x<- filter(port_df, address == 'R') %>% mutate_all(as.numeric)
cor(x$traveltime, x$G3)
mean(x$G3)
mean(xu$G3)
mean(x$traveltime)
mean(xu$traveltime)
cor(xu$G3,xu)
cor(x$G3, x)
cor(port)
ggplot(port_df, aes(y= G3,x= absences, color= address )) + geom_count(shape= 18, position= position_dodge(width = .75)) +geom_smooth(method = "lm")

ggplot(port_df, aes(y= G3,x= traveltime, color= address )) + geom_count(shape= 18, position= position_dodge(width = .2)) +geom_smooth(method = "lm") + scale_size(name = "count")
ggplot(port_df, aes(y= G3,x= absences, color= as.factor(traveltime) )) + geom_count(shape= 18, position= position_dodge(width = .75)) + geom_smooth(method = "lm") 
cor(port_df.num$G3, port_df.num)

ggplot(port_df, aes(y= G3,x= otheract, color= guardian )) + geom_count(shape= 18, position= position_dodge(width = .5)) +geom_smooth(method = "lm")
ggplot(port_df, aes(y= G3,x= studytime, color= guardian )) + geom_count(shape= 18, position= position_dodge(width = .5)) +geom_smooth(method = "lm")
ggplot(port_df, aes(y= G3,x= absences, color= higher )) + geom_count(shape= 18, position= position_dodge(width = 1)) +geom_smooth(method = "lm")
ggplot(port_df, aes(y= G3,x= traveltime, color= higher )) + geom_count(shape= 18, position= position_dodge(width = 1)) +geom_smooth(method = "lm")
cor(port_df.num$higher, port_df.num$reason)
ggplot(port_df, aes(reason, color= higher)) + geom_histogram(stat = 'count', position = position_dodge(width = 1))
ggplot(port_df, aes(y= G3,x= traveltime, color= reason )) + geom_count(shape= 18, position= position_dodge(width = 1)) +geom_smooth(method = "lm")
ggplot(math_df, aes(y= G3,x= traveltime, color= higher )) + geom_count(shape= 18, position= position_dodge(width = 1)) +geom_smooth(method = "lm")
ggplot(math_df, aes(reason, color= higher)) + geom_histogram(stat = 'count', position = position_dodge(width = 1))
cor(port_df$G3, port_df$Medu&port_df$Fedu)
cor(port_df$G3, port_df$Medu)
cor(port_df$G3, port_df$Fedu)
ggplot(port_df, aes(y= G3,x=studytime , color= as.factor(Fedu))) + geom_count(shape= 18, position= position_dodge(width = 1)) +geom_smooth(method = "lm")
ggplot(port_df, aes(y= G3,x=absences , color= as.factor(Fedu))) + geom_count(shape= 18, position= position_dodge(width = 1)) +geom_smooth(method = "lm")
cor(port_df.num$otheract, port_df.num$absences)
cor(port_df.num$Dalc, port_df.num$absences)
cor(port_df.num)
cor(port_df.num$studytime, port_df.num$otheract)