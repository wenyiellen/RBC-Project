# Plotting step
rm(list=ls())
library(dplyr)
library(ggplot2)
library(ROCR)
library(mice)
library(ggthemes)
library(ggmosaic)
library(tidyr)
library(plyr)
library(caret)
library(corrplot)
library(tidyr)
library(VIM)
library(randomForest)

# Import Dataset
data = read.csv("/Users/Lucas/Documents/case_competition/Credit card data for participants.csv", header=TRUE)

# Clean Dataset
clean_data = data %>%
  mutate(Credit_Limit = ifelse(Credit_Limit>= 200 & Credit_Limit <= 40000, Credit_Limit, NA)) %>%
  mutate(Sex = ifelse(Sex == 1 | Sex == 2 , Sex, NA)) %>%
  mutate(Education = ifelse(Education %in% c(1,2,3,4), Education, NA)) %>%
  mutate(Marital_Status = ifelse(Marital_Status %in% c(1,2,3), Marital_Status, NA)) %>%
  mutate(Age = ifelse(Age <= 100 & Age >= 14, Age, NA)) %>%
  mutate(Jan_Repay_Status = ifelse(Jan_Repay_Status >= -3 & Jan_Repay_Status <= 12, Jan_Repay_Status, NA)) %>%
  mutate(Feb_Repay_Status = ifelse(Feb_Repay_Status >= -3 & Feb_Repay_Status <= 12, Feb_Repay_Status, NA)) %>%
  mutate(Mar_Repay_Status = ifelse(Mar_Repay_Status >= -3 & Mar_Repay_Status <= 12, Mar_Repay_Status, NA)) %>%
  mutate(Apr_Repay_Status = ifelse(Apr_Repay_Status >= -3 & Apr_Repay_Status <= 12, Apr_Repay_Status, NA)) %>%
  mutate(May_Repay_Status = ifelse(May_Repay_Status >= -3 & May_Repay_Status <= 12, May_Repay_Status, NA)) %>%
  mutate(Jun_Repay_Status = ifelse(Jun_Repay_Status >= -3 & Jun_Repay_Status <= 12, Jun_Repay_Status, NA)) %>%

# Repleace corrupted value by NA and replace Repay_Status < 0 with 0
mutate(Jan_Repay_Status = ifelse(Jan_Repay_Status <= 0, 0, Jan_Repay_Status)) %>%
  mutate(Feb_Repay_Status = ifelse(Feb_Repay_Status <= 0, 0, Feb_Repay_Status)) %>%
  mutate(Mar_Repay_Status = ifelse(Mar_Repay_Status <= 0, 0, Mar_Repay_Status)) %>%
  mutate(Apr_Repay_Status = ifelse(Apr_Repay_Status <= 0, 0, Apr_Repay_Status)) %>%
  mutate(May_Repay_Status = ifelse(May_Repay_Status <= 0, 0, May_Repay_Status)) %>%
  mutate(Jun_Repay_Status = ifelse(Jun_Repay_Status <= 0, 0, Jun_Repay_Status)) %>%
  mutate(Previous_Payment_Prior_Jan = ifelse(Previous_Payment_Prior_Jan >= -50000 & Previous_Payment_Prior_Jan <= 50000, Previous_Payment_Prior_Jan, NA)) %>%
  mutate(Previous_Payment_Prior_Feb = ifelse(Previous_Payment_Prior_Feb >= -50000 & Previous_Payment_Prior_Feb <= 50000, Previous_Payment_Prior_Feb, NA)) %>%
  mutate(Previous_Payment_Prior_Mar = ifelse(Previous_Payment_Prior_Mar >= -50000 & Previous_Payment_Prior_Mar <= 50000, Previous_Payment_Prior_Mar, NA)) %>%
  mutate(Previous_Payment_Prior_Apr = ifelse(Previous_Payment_Prior_Apr >= -50000 & Previous_Payment_Prior_Apr <= 50000, Previous_Payment_Prior_Apr, NA)) %>%
  mutate(Previous_Payment_Prior_May = ifelse(Previous_Payment_Prior_May >= -50000 & Previous_Payment_Prior_May <= 50000, Previous_Payment_Prior_May, NA)) %>%
  mutate(Previous_Payment_Prior_Jun = ifelse(Previous_Payment_Prior_Jun >= -50000 & Previous_Payment_Prior_Jun <= 50000, Previous_Payment_Prior_Jun, NA)) %>%
  mutate(Jan_Statement = ifelse(Jan_Statement >= -50000 & Jan_Statement <= 50000, Jan_Statement, NA)) %>%
  mutate(Feb_Statement = ifelse(Feb_Statement >= -50000 & Feb_Statement <= 50000, Feb_Statement, NA)) %>%
  mutate(Mar_Statement = ifelse(Mar_Statement >= -50000 & Mar_Statement <= 50000, Mar_Statement, NA)) %>%
  mutate(Apr_Statement = ifelse(Apr_Statement >= -50000 & Apr_Statement <= 50000, Apr_Statement, NA)) %>%
  mutate(May_Statement = ifelse(May_Statement >= -50000 & May_Statement <= 50000, May_Statement, NA)) %>%
  mutate(Jun_Statement = ifelse(Jun_Statement >= -50000 & Jun_Statement <= 50000, Jun_Statement, NA)) %>%
  mutate(July_Payment_Status = ifelse(July_Payment_Status %in% c(0, 1), July_Payment_Status, NA)) %>%
  filter(!is.na(July_Payment_Status)) # remove rows with NA values in July_Payment_Status

# Remove duplicates, add IDs, reorder columns
clean_data_final = clean_data[!duplicated(clean_data),]
clean_data_final_withid = clean_data_final %>% mutate(id = c(1:nrow(clean_data_final)))
clean_data_final_withid = clean_data_final_withid[,c(25, 1:24)]

# Turn all decimal into integer
dataF = clean_data_final_withid
dataF[!dataF%%1==0] = round(na.omit(dataF[!dataF%%1==0]))


# Reshape Dataset
reshape_data <- gather(clean_data_final_withid,
                       key = "attribute",
                       value = "value",
                       -id, -Credit_Limit, -Sex, -Education, -Marital_Status, -Age, -July_Payment_Status)
reshape_data = reshape_data %>% arrange(id)


# Pie chart, get some basic idea of how data is distributed
A <- c(3422, 12746) 
cols <- c("grey50", "white")
percentlabels<- round(100*A/sum(A), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(A, main="Single v.s. Default", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Default","Paid on time"), cex=0.8, fill=cols)

B <- c(3275, 10569) 
cols <- c("grey50", "white")
percentlabels<- round(100*B/sum(B), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(B, main="Married v.s. Default", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Default","Paid on time"), cex=0.8, fill=cols)

C <- c(2967, 9135) 
cols <- c("grey50", "white")
percentlabels<- round(100*C/sum(C), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(C, main="Male v.s. Default", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Default","Paid on time"), cex=0.8, fill=cols)

D <- c(3829, 14466) 
cols <- c("grey50", "white")
percentlabels<- round(100*D/sum(D), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(D, main="Female v.s. Default", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Default","Paid on time"), cex=0.8, fill=cols)

# Dealing with missing values
# first let's see the pattern of missing values
pattern = md.pattern(dataF)

# show some graphs of missing values' distribution
p = dataF[,c('Credit_Limit', 'Sex', 'Education', 'Marital_Status', 'Age', 'Jan_Statement', 'Feb_Statement')]
aggr_plot <- aggr(p, col=c('navyblue','red'), 
                  numbers=FALSE, sortVars=TRUE, labels=names(p), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Imputation of data using MICE package
tempData <- mice(dataF, meth='pmm', seed=123)
summary(tempData)
# check if the density plot of imputed data meets the original density
densityplot(tempData)
# fill in imputed data
completedData <- mice::complete(tempData,1)
dataF = completedData
pattern = md.pattern(dataF) # check if all missing values are gone


################################################ "feature engineering" ################################################
# show histgram of age, not a lot information found in this histogram
ggplot(dataF, aes(x = Age, fill = as.factor(July_Payment_Status), color = as.factor(July_Payment_Status))) +
  geom_histogram()

# Plot credit limit vs july payment histogram
ggplot(dataF, aes(x = Credit_Limit, fill = as.factor(July_Payment_Status), color = as.factor(July_Payment_Status))) +
  geom_histogram(binwidth = 3500) +
  scale_x_continuous(breaks=seq(0,32000,5000)) +
  stat_bin(binwidth=3000, geom="text", colour="black", size=3.5,
           aes(label=..count.., group=July_Payment_Status, y=0.8*(..count..)))

# group credit limit with fowllowing rule: >18000: 4, >10000: 3, > 4500: 2, else 1
dataC = dataF %>% mutate(Credit_Limit = ifelse(Credit_Limit > 18000, 4, ifelse(Credit_Limit > 10000, 3, ifelse(Credit_Limit > 4500, 2, 1))))
# show mosaic plot after grouping
mosaicplot(table(dataC$Credit_Limit, dataC$July_Payment_Status), main='Credit Limit by July Payment Status', shade=TRUE)
dataF = dataC


# derive new column of value 1: default on all 6 month, 0: otherwise
dataNotPayOnTime = dataF %>% 
  filter(Jun_Repay_Status > 0) %>% filter(Feb_Repay_Status > 0) %>%
  filter(Mar_Repay_Status > 0) %>% filter(Apr_Repay_Status > 0) %>%
  filter(May_Repay_Status > 0) %>% filter(Jun_Repay_Status > 0)
dataF = dataF %>% mutate(allNotPayOnTime = ifelse(dataF$id %in% dataNotPayOnTime$id, 1, 0))
# show histogram
ggplot(dataF, aes(x = allNotPayOnTime, fill = as.factor(July_Payment_Status), color = as.factor(July_Payment_Status))) +
  geom_histogram(stat='count', position='dodge')



# derive new column of value 0: paid on time for all Jan to June, 1: otherwise
dataPayOnTime = dataF %>% 
  filter(Jan_Repay_Status == 0) %>% filter(Feb_Repay_Status == 0) %>%
  filter(Mar_Repay_Status == 0) %>% filter(Apr_Repay_Status == 0) %>%
  filter(May_Repay_Status == 0) %>% filter(Jun_Repay_Status == 0)

dataPayStatus = dataF %>% mutate(allPayOnTime = ifelse(dataF$id %in% dataPayOnTime$id, 0, 1))
# plot the histogram of allPayOnTime, filled with July_Payment_Status, to show the their distribution
ggplot(dataPayStatus, aes(x = allPayOnTime, fill = as.factor(July_Payment_Status), color = as.factor(July_Payment_Status))) +
  geom_histogram(stat='count', position='dodge')
# Credit card holder who paid on time from  Jan to June 
# consists of only 89.3% of all who paid on time in June.
# Credit card holder who have at lease one month not paying 
# on time from  Jan to June consists of almost half of whom default on July.
dataF$allPayOnTime = dataPayStatus$allPayOnTime
# group the Age variable
dataA <- dataF %>% mutate(Age = ifelse(Age > 60, 'old', 
                                       ifelse(Age > 40, 'midOld', 
                                              ifelse(Age > 25, 'mid', 'young'))))
# show histogram, filled with July_Payment_status
ggplot(dataA, aes(x = Age, fill = as.factor(July_Payment_Status), color = as.factor(July_Payment_Status))) +
  geom_histogram(stat='count', position = 'dodge')

# Find the young female group to see if they are more likely to default on July
dataYF = dataA %>% mutate(youngFemale = ifelse(Age == 'young' & Sex == 2, 1, 0))
# show histogram
ggplot(dataYF, aes(x = youngFemale, fill = as.factor(July_Payment_Status), color = as.factor(July_Payment_Status))) +
  geom_histogram(stat='count') +
  stat_bin(binwidth=1, geom="text", colour="black", size=3.5,
           aes(label=..count.., group=July_Payment_Status, y=0.8*(..count..)))

# find how many times a user defaults from Jan to June
statusCol = c('Jan_Repay_Status', 'Feb_Repay_Status', 'Mar_Repay_Status', 'Apr_Repay_Status', 'May_Repay_Status', 'Jun_Repay_Status')
dataNotPayOnTime = dataF[!dataF$id %in% dataPayOnTime$id, ]
countDefault = 0
for (i in c(1:nrow(dataF))) {
  for (j in c(1:length(statusCol))) {
    if (dataF[i, statusCol[j]] > 0) {
      countDefault = countDefault + 1
      dataF[i, 'countDefault'] = countDefault
    }
  }
  countDefault = 0
}
dataF$countDefault[is.na(dataF$countDefault)] = 0
# show histogram
ggplot(dataF, aes(x = countDefault, fill = factor(July_Payment_Status))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(0:6))
# find average previous payment prior that month
dataAvg = dataF %>% mutate(Avg_pay_prior = 
                             (Previous_Payment_Prior_Apr+Previous_Payment_Prior_Feb+Previous_Payment_Prior_Mar+
                                Previous_Payment_Prior_May+Previous_Payment_Prior_Jan+Previous_Payment_Prior_Jun) / 6)
dataF = dataAvg


clean_data_omitNA <- na.omit(dataF) # Omit NA values in the dataset in computation
cor = cor(clean_data_omitNA) # find pair correlation


################################################ find pairwise correlation ################################################
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
# return: p value matrix
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(dataF)

# produce correlation plot with significance level < 0.01 blank
pdf("Plot12.pdf", height = 10, width = 10)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor, method="color", col=col(200),
         tl.cex = 0.7, number.cex = 0.6,
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", #tl.srt=45, #Text label color and rotation
         # Combine with significance
         #p.mat = p.mat, 
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)
dev.off()
par(mfrow = c(1,1))


################################################ "Train, test, predict" ################################################
# Prediction models
# reference: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
smp_size <- floor(0.75 * nrow(dataF))
set.seed(123)
train_ind <- sample(seq_len(nrow(dataF)), size = smp_size)
train <- dataF[train_ind, ]
test <- dataF[-train_ind, ]

train$July_Payment_Status = as.factor(train$July_Payment_Status)
test$July_Payment_Status = as.factor(test$July_Payment_Status)

# Construct logistic regression model with all variables in the model
model1 <- glm(July_Payment_Status ~ Sex + Marital_Status + Age + Education + Credit_Limit +
                Previous_Payment_Prior_Jan + Previous_Payment_Prior_Feb +
                Previous_Payment_Prior_Mar + Previous_Payment_Prior_Apr
              + Previous_Payment_Prior_May + Previous_Payment_Prior_Jun + Jan_Repay_Status + Feb_Repay_Status
              + Mar_Repay_Status + Apr_Repay_Status + May_Repay_Status + Jun_Repay_Status + Jan_Statement + 
                Feb_Statement + Mar_Statement + Apr_Statement + May_Statement + 
                Jun_Statement + allNotPayOnTime + allPayOnTime,family=binomial(link='logit'),data=train)

summary(model1)

# Construct random forest model
rf_model <- randomForest(factor(July_Payment_Status) ~ .-id - Previous_Payment_Prior_Apr -
                           Previous_Payment_Prior_Jan - Previous_Payment_Prior_Feb - Previous_Payment_Prior_Mar -
                           Previous_Payment_Prior_May - Previous_Payment_Prior_Jun,
                         data = na.omit(train))

testNoNA = na.omit(test)
prediction = predict(rf_model, testNoNA, type = 'class')
solution <- data.frame(CustomerID = testNoNA$id, payment_Status = prediction)
testData <- data.frame(payment_Status = testNoNA$July_Payment_Status)

# show confusion matrix
cMatrix = confusionMatrix(prediction, testData$payment_Status)
cMatrix
# reference http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()




