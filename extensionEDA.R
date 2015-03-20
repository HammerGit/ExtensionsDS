##### Setup main data frames, add test and train #####
extmain <- read.csv("extmain.csv")
extmain$group <- runif(dim(extmain)[1])
test <- subset(extmain, extmain$group <= 0.1)
train <- subset(extmain, extmain$group > 0.1)
str(train)
glimpse(test)
table(train$Extended)
prop.table(table(train$Extended))
summary(train)
table(train$RateGroup)
prop.table(table(train$RateGroup))

############## Feature Engineering  ######################

train$Extended <- as.factor(train$Extended)

# Change Starts and Ends to Date type 
train$StartDate <- as.Date(train$StartDate, format = "%m/%d/%Y")
train$EndDate <- as.Date(train$EndDate, format = "%m/%d/%Y")

train$ExtStartDate <- as.Date(train$ExtStartDate, format = "%m/%d/%Y")
train$ExtEndDate <- as.Date(train$ExtEndDate, format = "%m/%d/%Y")

# Create a new variable to show the DURATION in days. Includes all calendar days
train$OrigDuration <- difftime(train$EndDate, train$StartDate, units = "days")
train$ExtDuration <- difftime(train$ExtEndDate, train$ExtStartDate, units = "days")
train$EmpType <- as.character(train$EmpType)

train$EmpType[train$EmpType == "Subcontractor: Partner Sourced"] <- "Subcontractor"
train$EmpType[train$EmpType == "Subcontractor: Corporate"] <- "Subcontractor"
train$EmpType[train$EmpType == "Subcontractor: Independent/1099"] <- "Subcontractor"
train$EmpType[train$EmpType == "Subcontractor: GLS"] <- "Subcontractor"

train$EmpType <- as.factor(train$EmpType)

breaks <- c(50, 100, 125, 130, 135, 140, 145, 150, 175, 200)
RateGroups <- cut(train$Rate, breaks= breaks)
summary(RateGroups)
train$RateGroup <- RateGroups

##### Plots to evaluate relationships #####

# side by side bar chart
ggplot(train) + geom_bar(aes(x= RateGroup, fill= Extended),position = "dodge")

# 100% fill bar chart
ggplot(train) + geom_bar(aes(x= RateGroup, fill= Extended),position = "fill")


ggplot(train, aes(x= RateGroup, y=ExtNum)) +
  geom_point() +
  stat_smooth(method = "lm")
  #coord_flip()


#   facet_wrap(~EmpType + Practice ) +
#   geom_histogram(binwidth = 5) +
#   xlab("Rate") +
#   ylab("Total Count")
  ##theme(axis.text.x = element_text(angle = 45, vjust = 1.0))






##### Modelling #####

fit <- rpart(Extended ~ RateGroup + ServiceOffering + OrigDuration + EmpType,
             data = train, method = "class")

fancyRpartPlot(fit)


















