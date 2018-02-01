'# 
HW2: Class One Survey Analysis
Name: Bing-Jie Yen
Date: Jan 25,2018 #'


install.packages("Hmisc")
install.packages("plyr")
install.packages("xtable")
library(xtable)
library(plyr)
library(Hmisc)
library(knitr)
'# 1. Import class one survey data from our Github site (The dataset called 'Class_One_Survey.csv' is located in the class one folder), calling the R
# dataframe that you create C1survey.'

setwd("D:/Dropbox/@2018 Spring/ADA/data") 
C1survey<-read.csv(file.choose (), sep=",", header = TRUE)
C1survey<-as.data.frame(C1survey)

'# 2. Determine a. the number of observations (i.e. the number of people who labelled
out the survey) and b. the number of variables in the dataframe.#'
# There are 14 observationsin the C1 survey data frame.
summary(C1survey)
describe(C1survey)


'# 3. Generate and display a list of column names, calling your list varlist.'
varlist<-list(C1survey)
varlist

'# 4. a. Rename the column variables to something shorter and that is descriptive
of what the variable is about (for example like_dogs for the 'Do you like dogs?'
question variable) and 
b. write code to display that they are renamed.#'

names(C1survey)[names(C1survey) == 'Do.you.like.cats.'] <- 'like_cats'
label(C1survey$like_cats) <- "Do you like cats?" 
names(C1survey)[names(C1survey) == 'Do.you.like.Dogs.'] <- 'like_dogs'
label(C1survey$like_dogs) <- "Do you like cats?" 
names(C1survey)[names(C1survey) == 'If.you.were.stranded.on.a.desert.island..what.is.the.one.item.would.you.want.to.have.with.you.'] <- 'desert_love'
label(C1survey$desert_love) <- "If.you.were.stranded.on.a.desert.island..what.is.the.one.item.would.you.want.to.have.with.you." 

names(C1survey)[names(C1survey) == 'If.you.were.to.create.a.slogan.for.your.life..what.would.the.slogan.be...Examples...Eat..drink..and.be.merry..for.tomorrow..we.all.die....Bite.off.more.than.you.can.chew....There.are.far.better.things.ahead.than.any.we.leave.behind...'] <- 'slogan'
label(C1survey$slogan) <- "If.you.were.to.create.a.slogan.for.your.life..what.would.the.slogan.be...Examples...Eat..drink..and.be.merry..for.tomorrow..we.all.die....Bite.off.more.than.you.can.chew....There.are.far.better.things.ahead.than.any.we.leave.behind..." 
names(C1survey)[names(C1survey) == 'What.is.your.major.public.health.area.of.interest..e.g..cancer..heart.disease..maternal.child.health..disparities..etc...'] <- 'PH_interests'
label(C1survey$PH_interests) <- "What.is.your.major.public.health.area.of.interest..e.g..cancer..heart.disease..maternal.child.health..disparities..etc..." 
describe(C1survey)




'5. Write code to determine and display the number of factor, integer, numerical,
and character variables there are in the C1survey dataset.'
summary(str(C1survey))
class(C1survey)
table(sapply(C1survey,class)) 

'6. 
a. Using code, check the height and weight variables for any unusual or missing values. If you ???nd any, 
b. describe what you will do with the unusual
  values in a comment before or immediately following the code that does it and
c. after you have cleaned up any unusual values, ???nd the mean height in cm and
weight in kg.'
# a. there are two unusual values in weight. In this case, two data points might be entered with kilograms.
describe(C1survey$What.is.your.height.in.cm.) #
describe(C1survey$What.is.your.weight.in.grams.) # 
# b. I generate a new variable "weight_grams" and replace the weight imported in kilograms to grams
C1survey$weight_grams<-ifelse(C1survey$What.is.your.weight.in.grams.<1000,C1survey$What.is.your.weight.in.grams.*1000,C1survey$What.is.your.weight.in.grams.)
mean(C1survey$weight_grams)   
# c.
describe(C1survey$weight_grams) 

'# 7. a. Create new variables called weight_kg and height_m that gives weight in
kg instead of grams and height in meters instead of cm and b. determine the
mean weight in kg and the mean height in meters.#'
weight_kg<-C1survey$weight_grams/1000
height_m<-C1survey$What.is.your.height.in.cm./100
mean(weight_kg) # the mean weight in kg is 57.81
mean(height_m)  # the mean height in m is 1.67


'# 8. a. Derive a BMI variable (kg/m2) from the height_m and weight_kg vari-
  ables called BMI (adding it to the C1 survey dataset). 
b. Determine the median BMI. 
c. Make another variable called BMI_cat (adding it to the C1 survey
dataset) that divides BMI into >median and <=median BMI. 
d. Label the levels. 
e. Determine how many people are in each category of BMI_cat using the table function.#'
C1survey$BMI<-weight_kg/(height_m^2)
summary(C1survey$BMI) # median of BMI is 21.54
C1survey$BMI_cat<-ifelse(C1survey$BMI>21.54, "BMI higher than median","BMI eqaul or lower than median")
#Frequency table : there are 7 people with BMI higher than median and there are 7 people with BMI equal or lower than median
ftable(C1survey$BMI_cat)  

'# 9. a. Create a new dataset called C1survey_BMI_below that includes only
individuals with BMI below the median and b. write code to check that your
dataset only includes individuals with BMI below the median.#'
C1survey_BMI_below<-subset(C1survey,C1survey$BMI<median(C1survey$BMI))
C1survey_BMI_below$BMI #all values are lower than 21.54.


'# 10. Pick your favorite variable to analyze, come up with a question you want
to answer with that variable, generate the code, and provide an answer to your
question. Describe what you did using comments (i.e. #s).#'
# I want to see how diverse this class is.
summary(C1survey$What.is.your.home.country.)
# Combine all the answer which states the home country is USA, and other answered responses stay the same. 
# missing values are named as "Unkown"
C1survey$homecountry<-ifelse(C1survey$What.is.your.home.country.=='United States'|
                               C1survey$What.is.your.home.country.=="United States of America"|
                               C1survey$What.is.your.home.country.=="US"|
                               C1survey$What.is.your.home.country.=="USA","U.S.A",
                             ifelse(C1survey$What.is.your.home.country.=="Ethiopia","Ethiopia",
                                    ifelse(C1survey$What.is.your.home.country.=="Taiwan","Taiwan",
                                           ifelse(C1survey$What.is.your.home.country.=="China","China","Unknown"))))
C1survey$homecountry<-as.factor(C1survey$homecountry)                                   
describe(C1survey$homecountry)

# there are 12 observations with home country in United States and it accounts for 85.7% of whole class.

