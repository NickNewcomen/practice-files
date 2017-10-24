# R introduction
# 1.2 RStudio & coding
x <- 3
x

# 1.3 Asking for help
help(mean)
mean(y) 

# 2.1 Setting up a working directory
setwd("S:/HQ/102PF/Shared/CJG_OMS/OMS/Analytical Services/RACC/Statistical Methods and Development/4. Training and development/R training/Introduction")
getwd()

# 2.2 Importing data
offenders <- read.csv("Offenders_Chicago_Police_Dept_Main.csv")

# 2.3 Packages
install.packages("foreign")
library("foreign")
help(package=foreign)

# 2.4 Viewing & summarizing the dataset
View(offenders)
str(offenders)
head(offenders, 10)
tail(offenders, 2)
offenders[c(500, 502),4:5]
summary(offenders)
sapply(offenders, sd)
tapply(offenders$WEIGHT, offenders$GENDER, mean)

# 2.5 Using dataset variables
offenders$GENDER
offenders[,4]
offenders$weight_kg <- offenders$WEIGHT*0.45359
summary(offenders$weight_kg)
class(offenders$weight_kg)

# 3.1 Dates
str(offenders)
Sys.Date()
offenders$birth_date_formatted <-  as.Date(offenders$BIRTH_DATE, "%m/%d/%Y")
offenders$b_wkday <- weekdays(offenders$birth_date_formatted)
offenders$b_qtr <- quarters(offenders$birth_date_formatted)
library(lubridate)
offenders$b_year <- year(offenders$birth_date_formatted)
offenders$b_month <- month(offenders$birth_date_formatted)
offenders$b_day <- day(offenders$birth_date_formatted)
offenders$days_before_01_01_2000 <- as.Date("2000-01-01") - offenders$birth_date_formatted

# 3.3 Reordering the factor class, coercing classes, the logical class and Ifelse
levels(offenders$GENDER)
offenders$GENDER <- relevel(offenders$GENDER, "MALE")
levels(offenders$GENDER)
offenders$HEIGHT <- as.numeric(offenders$HEIGHT)
offenders$HEIGHT <- as.integer(offenders$HEIGHT) 
offenders$BLOCK <- as.character(offenders$BLOCK)
offenders$BLOCK <- as.factor(offenders$BLOCK)  
l <- 4 > 3
l
class(l)
offenders$height_over_6feet  <- ifelse(offenders$HEIGHT>=600, 1, 0)

# 4.1 Reordering the factor class, coercing classes, the logical class and Ifelse
levels(offenders$GENDER)
offenders$GENDER <- relevel(offenders$GENDER, "MALE")
offenders$HEIGHT <- as.numeric(offenders$HEIGHT) 
offenders$HEIGHT <- as.integer(offenders$HEIGHT) 
offenders$BLOCK <- as.character(offenders$BLOCK)  
offenders$BLOCK <- as.factor(offenders$BLOCK)  
l <- 4 > 3
class(l)
offenders$height_over_6feet  <- ifelse(offenders$HEIGHT>=600, 1, 0)

# 4.2 Conditional summary statistics & further statistical functions
summary(offenders[c(50:500, 502),4:5])
summary(offenders$HEIGHT[offenders$RACE=="WHITE"])
summary(offenders$HEIGHT[offenders$WEIGHT>200])
table(offenders$RACE)
table(offenders$RACE)/length(offenders$RACE)

# 5.1 Merging datasets
offenders_merge <- merge(offenders, offenders_age, by=c("LAST", "BIRTH_DATE", "VICTIM_MINOR"))
height1 <- offenders_merge[1:10, "HEIGHT"]
height2 <- offenders_merge[11:20, "HEIGHT"]
weight1 <- offenders_merge[1:10, "WEIGHT"]
height <- rbind(height1, height2)
height_weight <- cbind(height1, weight1)

# 5.2 Handling missing values
Miss <- is.na(offenders_merge$HEIGHT)
length(Miss[Miss == TRUE])
complete.cases(offenders_merge)
offenders_merge_missings <- offenders_merge[!complete.cases(offenders_merge),] 
offenders_merge_completes <- offenders_merge[complete.cases(offenders_merge),]

# 5.3 Exporting data
write.csv(offenders_merge, file = "Offenders_Chicago_Police_Dept_.csv")