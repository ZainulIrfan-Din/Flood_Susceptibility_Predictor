#SET WORKING DIRECTORY
setwd("C:/Users/Asus/Documents/UTP/Coursework/2 - UG/s8 May 22/FYP2/CODING")


#PACKAGES INSTALLATION
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("shiny")
# install.packages("caTools")
# install.packages("shinythemes")
# install.packages("shinyWidgets")
# install.packages("shinydashboard")


#calling package library
library(ggplot2) #graph usage
library(dplyr) #data manipulation purposes (select, etc.)
library(shiny)   #webpage creation
library(caTools) #sample.split usage
library(shinythemes)   #to theme a Shiny application
library(shinyWidgets) #to theme Shiny dashboard background
library(shinydashboard) #to enable Shiny dashboard
library(leaflet) #for interactive map

require(ggplot2) #for line chart

#remove all dataframe for testing purposes
# rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"]) 


#load dataset
id <- "12zIXohg9lpWCr-bxEdF_d1rNwY4iuqPD" # google file ID
dataset <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), stringsAsFactors = FALSE, header = TRUE)


#DATA PREPARATION
#remove first irrelevant row
dataset = dataset[-1,]

#separate dataframe according to state & district
#Selangor (Gombak = G, Hulu Selangor = H, Kuala Selangor = K)
dataset_SGG = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Hulu.Selangor..Tanjung.Malim..2,
                                        Kuala.Selangor..Rantau.Panjang.:Jerantut..Kuala.Tahan..2))   #Gombak (Batu 10)
dataset_SGH = subset(dataset, select=-c(Gombak..Batu.10.:Jerantut..Kuala.Tahan..2))   #Hulu Selangor (Tanjung Malim)
dataset_SGK = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Gombak..Batu.10..2,
                                        Dungun..Kuala.Jengai.:Jerantut..Kuala.Tahan..2))   #Kuala Selangor (Rantau Panjang)

#Terengganu (Dungun = D, Hulu Terengganu = H, Setiu = S)
dataset_TGD = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Kuala.Selangor..Rantau.Panjang..2,
                                        Hulu.Terengganu..Kuala.Ping.:Jerantut..Kuala.Tahan..2))   #Dungun (Kuala Jengai)
dataset_TGH = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Dungun..Kuala.Jengai..2,
                                        Setiu..Kg.Besut.:Jerantut..Kuala.Tahan..2))   #Hulu Terengganu (Kuala Ping)
dataset_TGS = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Hulu.Terengganu..Kuala.Ping..2,
                                        Kuantan..Cherating.:Jerantut..Kuala.Tahan..2))   #Setiu (Kg Besut)

#Pahang (Bentong = B, Jerantut = J, Kuantan = K)
dataset_PHGK = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Setiu..Kg.Besut..2,
                                         Bentong..Kuala.Marong.:Jerantut..Kuala.Tahan..2))   #Kuantan (Cherating)
dataset_PHGB = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Kuantan..Cherating..2,
                                         Jerantut..Kuala.Tahan.:Jerantut..Kuala.Tahan..2))   #Bentong (Kuala Marong)
dataset_PHGJ = subset(dataset, select=-c(Hulu.Selangor..Tanjung.Malim.:Bentong..Kuala.Marong..2))   #Jerantut (Kuala Tahan)


#Rename the column to the correct name
names(dataset_SGG) <- c('Date', 'Rainfall', 'Water Level', 'Warning')
names(dataset_SGH) <- c('Date', 'Rainfall', 'Water Level', 'Warning')
names(dataset_SGK) <- c('Date', 'Rainfall', 'Water Level', 'Warning')

names(dataset_TGD) <- c('Date', 'Rainfall', 'Water Level', 'Warning')
names(dataset_TGH) <- c('Date', 'Rainfall', 'Water Level', 'Warning')
names(dataset_TGS) <- c('Date', 'Rainfall', 'Water Level', 'Warning')

names(dataset_PHGB) <- c('Date', 'Rainfall', 'Water Level', 'Warning')
names(dataset_PHGJ) <- c('Date', 'Rainfall', 'Water Level', 'Warning')
names(dataset_PHGK) <- c('Date', 'Rainfall', 'Water Level', 'Warning')


#remove rows that have at least one empty value (NA)
dataset_SGG[dataset_SGG == ""] <- NA
dataset_SGG <- dataset_SGG[rowSums(is.na(dataset_SGG)) == 0,]

dataset_SGH[dataset_SGH == ""] <- NA
dataset_SGH <- dataset_SGH[rowSums(is.na(dataset_SGH)) == 0,]

dataset_SGK[dataset_SGK == ""] <- NA
dataset_SGK <- dataset_SGK[rowSums(is.na(dataset_SGK)) == 0,]


dataset_TGD[dataset_TGD == ""] <- NA
dataset_TGD <- dataset_TGD[rowSums(is.na(dataset_TGD)) == 0,]

dataset_TGH[dataset_TGH == ""] <- NA
dataset_TGH <- dataset_TGH[rowSums(is.na(dataset_TGH)) == 0,]

dataset_TGS[dataset_TGS == ""] <- NA
dataset_TGS <- dataset_TGS[rowSums(is.na(dataset_TGS)) == 0,]


dataset_PHGB[dataset_PHGB == ""] <- NA
dataset_PHGB <- dataset_PHGB[rowSums(is.na(dataset_PHGB)) == 0,]

dataset_PHGJ[dataset_PHGJ == ""] <- NA
dataset_PHGJ <- dataset_PHGJ[rowSums(is.na(dataset_PHGJ)) == 0,]

dataset_PHGK[dataset_PHGK == ""] <- NA
dataset_PHGK <- dataset_PHGK[rowSums(is.na(dataset_PHGK)) == 0,]


#Convert column to the correct type
dataset_SGG$Rainfall <- as.character(dataset_SGG$Rainfall)
dataset_SGG$Rainfall <- as.numeric(dataset_SGG$Rainfall)
dataset_SGG$`Water Level` <- as.character(dataset_SGG$`Water Level`)
dataset_SGG$`Water Level` <- as.numeric(dataset_SGG$`Water Level`)
dataset_SGG$Warning <- as.character(dataset_SGG$Warning)
dataset_SGG$Warning <- as.integer(dataset_SGG$Warning)
dataset_SGG$Warning <- as.factor(dataset_SGG$Warning)

dataset_SGH$Rainfall <- as.character(dataset_SGH$Rainfall)
dataset_SGH$Rainfall <- as.numeric(dataset_SGH$Rainfall)
dataset_SGH$`Water Level` <- as.character(dataset_SGH$`Water Level`)
dataset_SGH$`Water Level` <- as.numeric(dataset_SGH$`Water Level`)
dataset_SGH$Warning <- as.character(dataset_SGH$Warning)
dataset_SGH$Warning <- as.integer(dataset_SGH$Warning)
dataset_SGH$Warning <- as.factor(dataset_SGH$Warning)

dataset_SGK$Rainfall <- as.character(dataset_SGK$Rainfall)
dataset_SGK$Rainfall <- as.numeric(dataset_SGK$Rainfall)
dataset_SGK$`Water Level` <- as.character(dataset_SGK$`Water Level`)
dataset_SGK$`Water Level` <- as.numeric(dataset_SGK$`Water Level`)
dataset_SGK$Warning <- as.character(dataset_SGK$Warning)
dataset_SGK$Warning <- as.integer(dataset_SGK$Warning)
dataset_SGK$Warning <- as.factor(dataset_SGK$Warning)


dataset_TGD$Rainfall <- as.character(dataset_TGD$Rainfall)
dataset_TGD$Rainfall <- as.numeric(dataset_TGD$Rainfall)
dataset_TGD$`Water Level` <- as.character(dataset_TGD$`Water Level`)
dataset_TGD$`Water Level` <- as.numeric(dataset_TGD$`Water Level`)
dataset_TGD$Warning <- as.character(dataset_TGD$Warning)
dataset_TGD$Warning <- as.integer(dataset_TGD$Warning)
dataset_TGD$Warning <- as.factor(dataset_TGD$Warning)

dataset_TGH$Rainfall <- as.character(dataset_TGH$Rainfall)
dataset_TGH$Rainfall <- as.numeric(dataset_TGH$Rainfall)
dataset_TGH$`Water Level` <- as.character(dataset_TGH$`Water Level`)
dataset_TGH$`Water Level` <- as.numeric(dataset_TGH$`Water Level`)
dataset_TGH$Warning <- as.character(dataset_TGH$Warning)
dataset_TGH$Warning <- as.integer(dataset_TGH$Warning)
dataset_TGH$Warning <- as.factor(dataset_TGH$Warning)

dataset_TGS$Rainfall <- as.character(dataset_TGS$Rainfall)
dataset_TGS$Rainfall <- as.numeric(dataset_TGS$Rainfall)
dataset_TGS$`Water Level` <- as.character(dataset_TGS$`Water Level`)
dataset_TGS$`Water Level` <- as.numeric(dataset_TGS$`Water Level`)
dataset_TGS$Warning <- as.character(dataset_TGS$Warning)
dataset_TGS$Warning <- as.integer(dataset_TGS$Warning)
dataset_TGS$Warning <- as.factor(dataset_TGS$Warning)


dataset_PHGB$Rainfall <- as.character(dataset_PHGB$Rainfall)
dataset_PHGB$Rainfall <- as.numeric(dataset_PHGB$Rainfall)
dataset_PHGB$`Water Level` <- as.character(dataset_PHGB$`Water Level`)
dataset_PHGB$`Water Level` <- as.numeric(dataset_PHGB$`Water Level`)
dataset_PHGB$Warning <- as.character(dataset_PHGB$Warning)
dataset_PHGB$Warning <- as.integer(dataset_PHGB$Warning)
dataset_PHGB$Warning <- as.factor(dataset_PHGB$Warning)

dataset_PHGJ$Rainfall <- as.character(dataset_PHGJ$Rainfall)
dataset_PHGJ$Rainfall <- as.numeric(dataset_PHGJ$Rainfall)
dataset_PHGJ$`Water Level` <- as.character(dataset_PHGJ$`Water Level`)
dataset_PHGJ$`Water Level` <- as.numeric(dataset_PHGJ$`Water Level`)
dataset_PHGJ$Warning <- as.character(dataset_PHGJ$Warning)
dataset_PHGJ$Warning <- as.integer(dataset_PHGJ$Warning)
dataset_PHGJ$Warning <- as.factor(dataset_PHGJ$Warning)

dataset_PHGK$Rainfall <- as.character(dataset_PHGK$Rainfall)
dataset_PHGK$Rainfall <- as.numeric(dataset_PHGK$Rainfall)
dataset_PHGK$`Water Level` <- as.character(dataset_PHGK$`Water Level`)
dataset_PHGK$`Water Level` <- as.numeric(dataset_PHGK$`Water Level`)
dataset_PHGK$Warning <- as.character(dataset_PHGK$Warning)
dataset_PHGK$Warning <- as.integer(dataset_PHGK$Warning)
dataset_PHGK$Warning <- as.factor(dataset_PHGK$Warning)


#Narrowing predictor variable factor
dataset_SGG$Warning <- ifelse(dataset_SGG$Warning == 1 | dataset_SGG$Warning == 2 | 
                                dataset_SGG$Warning == 3 | dataset_SGG$Warning == 4, 1, 0)
dataset_SGG$Warning <- factor(dataset_SGG$Warning, levels = c(0, 1))

dataset_SGH$Warning <- ifelse(dataset_SGH$Warning == 1 | dataset_SGH$Warning == 2 | 
                                dataset_SGH$Warning == 3 | dataset_SGH$Warning == 4, 1, 0)
dataset_SGH$Warning <- factor(dataset_SGH$Warning, levels = c(0, 1))

dataset_SGK$Warning <- ifelse(dataset_SGK$Warning == 2 | dataset_SGK$Warning == 3 | 
                                dataset_SGK$Warning == 4, 1, 0)
dataset_SGK$Warning <- factor(dataset_SGK$Warning, levels = c(0, 1))


dataset_TGD$Warning <- ifelse(dataset_TGD$Warning == 2 | dataset_TGD$Warning == 3 | 
                                dataset_TGD$Warning == 4, 1, 0)
dataset_TGD$Warning <- factor(dataset_TGD$Warning, levels = c(0, 1))

dataset_TGH$Warning <- ifelse(dataset_TGH$Warning == 2 | dataset_TGD$Warning == 3 | 
                                dataset_TGH$Warning == 4, 1, 0)
dataset_TGH$Warning <- factor(dataset_TGH$Warning, levels = c(0, 1))

dataset_TGS$Warning <- ifelse(dataset_TGS$Warning == 2 | dataset_TGS$Warning == 3 | 
                                dataset_TGS$Warning == 4, 1, 0)
dataset_TGS$Warning <- factor(dataset_TGS$Warning, levels = c(0, 1))


dataset_PHGB$Warning <- ifelse(dataset_PHGB$Warning == 1 | dataset_PHGB$Warning == 2 | 
                                 dataset_PHGB$Warning == 3 | dataset_PHGB$Warning == 4, 1, 0)
dataset_PHGB$Warning <- factor(dataset_PHGB$Warning, levels = c(0, 1))

dataset_PHGJ$Warning <- ifelse(dataset_PHGJ$Warning == 2 | dataset_PHGJ$Warning == 3 | 
                                 dataset_PHGJ$Warning == 4, 1, 0)
dataset_PHGJ$Warning <- factor(dataset_PHGJ$Warning, levels = c(0, 1))

dataset_PHGK$Warning <- ifelse(dataset_PHGK$Warning == 1 | dataset_PHGK$Warning == 2 | 
                                 dataset_PHGK$Warning == 3 | dataset_PHGK$Warning == 4, 1, 0)
dataset_PHGK$Warning <- factor(dataset_PHGK$Warning, levels = c(0, 1))


#LOGISTIC REGRESSION
#Split data to test and train
SGG_sample = sample.split(dataset_SGG$Warning, SplitRatio = 0.7)
train_SGG = subset(dataset_SGG, SGG_sample == TRUE)
test_SGG = subset(dataset_SGG, SGG_sample == FALSE)

SGH_sample = sample.split(dataset_SGH$Warning, SplitRatio = 0.7)
train_SGH = subset(dataset_SGH, SGH_sample  == TRUE)
test_SGH = subset(dataset_SGH, SGH_sample  == FALSE)

SGK_sample = sample.split(dataset_SGK, SplitRatio = 0.7)
train_SGK = subset(dataset_SGK, SGK_sample == TRUE)
test_SGK = subset(dataset_SGK, SGK_sample == FALSE)


TGD_sample = sample.split(dataset_TGD$Warning, SplitRatio = 0.7)
train_TGD = subset(dataset_TGD, TGD_sample  == TRUE)
test_TGD = subset(dataset_TGD, TGD_sample  == FALSE)

TGH_sample = sample.split(dataset_TGH$Warning, SplitRatio = 0.7)
train_TGH = subset(dataset_TGH, TGH_sample  == TRUE)
test_TGH = subset(dataset_TGH, TGH_sample  == FALSE)

TGS_sample = sample.split(dataset_TGS$Warning, SplitRatio = 0.7)
train_TGS = subset(dataset_TGS, TGS_sample  == TRUE)
test_TGS = subset(dataset_TGS, TGS_sample  == FALSE)


PHGB_sample = sample.split(dataset_PHGB$Warning, SplitRatio = 0.7)
train_PHGB = subset(dataset_PHGB, PHGB_sample  == TRUE)
test_PHGB = subset(dataset_PHGB, PHGB_sample  == FALSE)

PHGJ_sample = sample.split(dataset_PHGJ$Warning, SplitRatio = 0.7)
train_PHGJ = subset(dataset_PHGJ, PHGJ_sample  == TRUE)
test_PHGJ = subset(dataset_PHGJ, PHGJ_sample  == FALSE)

PHGK_sample = sample.split(dataset_PHGK$Warning, SplitRatio = 0.7)
train_PHGK = subset(dataset_PHGK, PHGK_sample  == TRUE)
test_PHGK = subset(dataset_PHGK, PHGK_sample  == FALSE)


#Train model using the training data
Logistic_SGG <- glm(Warning ~ Rainfall + `Water Level`, data=train_SGG, family="binomial")
summary(Logistic_SGG)

Logistic_SGH <- glm(Warning ~ Rainfall + `Water Level`, data=train_SGH, family="binomial")
summary(Logistic_SGH)

Logistic_SGK <- glm(Warning ~ Rainfall + `Water Level`, data=train_SGK, family="binomial")
summary(Logistic_SGK)


Logistic_TGD <- glm(Warning ~ Rainfall + `Water Level`, data=train_TGD, family="binomial")
summary(Logistic_TGD)

Logistic_TGH <- glm(Warning ~ Rainfall + `Water Level`, data=train_TGH, family="binomial")
summary(Logistic_TGH)

Logistic_TGS <- glm(Warning ~ Rainfall + `Water Level`, data=train_TGS, family="binomial")
summary(Logistic_TGS)


Logistic_PHGB <- glm(Warning ~ Rainfall + `Water Level`, data=train_PHGB, family="binomial")
summary(Logistic_PHGB)

Logistic_PHGJ <- glm(Warning ~ Rainfall + `Water Level`, data=train_PHGJ, family="binomial")
summary(Logistic_PHGJ)

Logistic_PHGK <- glm(Warning ~ Rainfall + `Water Level`, data=train_PHGK, family="binomial")
summary(Logistic_PHGK)


#Run test data through the model
res_SGG <- predict(Logistic_SGG, test_SGG, type="response")
res_SGG
res_SGG <- predict(Logistic_SGG, train_SGG, type="response")
res_SGG

res_SGH <- predict(Logistic_SGH, test_SGH, type="response")
res_SGH
res_SGH <- predict(Logistic_SGH, train_SGH, type="response")
res_SGH

res_SGK <- predict(Logistic_SGK, test_SGK, type="response")
res_SGK
res_SGK <- predict(Logistic_SGK, train_SGK, type="response")
res_SGK


res_TGD <- predict(Logistic_TGD, test_TGD, type="response")
res_TGD
res_TGD <- predict(Logistic_TGD, train_TGD, type="response")
res_TGD

res_TGH <- predict(Logistic_TGH, test_TGH, type="response")
res_TGH
res_TGH <- predict(Logistic_TGH, train_TGH, type="response")
res_TGH

res_TGS <- predict(Logistic_TGS, test_TGS, type="response")
res_TGS
res_TGS <- predict(Logistic_TGS, train_TGS, type="response")
res_TGS


res_PHGB <- predict(Logistic_PHGB, test_PHGB, type="response")
res_PHGB
res_PHGB <- predict(Logistic_PHGB, train_PHGB, type="response")
res_PHGB

res_PHGJ <- predict(Logistic_PHGJ, test_PHGJ, type="response")
res_PHGJ
res_PHGJ <- predict(Logistic_PHGJ, train_PHGJ, type="response")
res_PHGJ

res_PHGK <- predict(Logistic_PHGK, test_PHGK, type="response")
res_PHGK
res_PHGK <- predict(Logistic_PHGK, train_PHGK, type="response")


#Classify susceptibility
sus_SGG <- ifelse(res_SGG > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_SGG

sus_SGH <- ifelse(res_SGH > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_SGH

sus_SGK <- ifelse(res_SGK > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_SGK


sus_TGD <- ifelse(res_TGD > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_TGD

sus_TGH <- ifelse(res_TGH > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_TGH

sus_TGS <- ifelse(res_TGS > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_TGS


sus_PHGB <- ifelse(res_PHGB > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_PHGB

sus_PHGJ <- ifelse(res_PHGJ > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_PHGJ

sus_PHGK <- ifelse(res_PHGK > 0.5, "Higher Susceptibility", "Lower Susceptibility")
sus_PHGK


#Model Validation
confmatrix_SGG <- table(Actual_Value=train_SGG$Warning, Predicted_Value = res_SGG > 0.5)
confmatrix_SGG

confmatrix_SGH <- table(Actual_Value=train_SGH$Warning, Predicted_Value = res_SGH > 0.5)
confmatrix_SGH

confmatrix_SGK <- table(Actual_Value=train_SGK$Warning, Predicted_Value = res_SGK > 0.5)
confmatrix_SGK


confmatrix_TGD <- table(Actual_Value=train_TGD$Warning, Predicted_Value = res_TGD > 0.5)
confmatrix_TGD

confmatrix_TGH <- table(Actual_Value=train_TGH$Warning, Predicted_Value = res_TGH > 0.5)
confmatrix_TGH

confmatrix_TGS <- table(Actual_Value=train_TGS$Warning, Predicted_Value = res_TGS > 0.5)
confmatrix_TGS


confmatrix_PHGB <- table(Actual_Value=train_PHGB$Warning, Predicted_Value = res_PHGB > 0.5)
confmatrix_PHGB

confmatrix_PHGJ <- table(Actual_Value=train_PHGJ$Warning, Predicted_Value = res_PHGJ > 0.5)
confmatrix_PHGJ

confmatrix_PHGK <- table(Actual_Value=train_PHGK$Warning, Predicted_Value = res_PHGK > 0.5)
confmatrix_PHGK


#Prediction Accuracy
(confmatrix_SGG[[1,1]] + confmatrix_SGG[[2,2]]) / sum(confmatrix_SGG)
(confmatrix_SGH[[1,1]] + confmatrix_SGH[[2,2]]) / sum(confmatrix_SGH)
(confmatrix_SGK[[1,1]] + confmatrix_SGK[[2,2]]) / sum(confmatrix_SGK)


(confmatrix_TGD[[1,1]] + confmatrix_TGD[[2,2]]) / sum(confmatrix_TGD)
(confmatrix_TGH[[1,1]] + confmatrix_TGH[[2,2]]) / sum(confmatrix_TGH)
(confmatrix_TGS[[1,1]] + confmatrix_TGS[[2,2]]) / sum(confmatrix_TGS)


(confmatrix_PHGB[[1,1]] + confmatrix_PHGB[[2,2]]) / sum(confmatrix_PHGB)
(confmatrix_PHGJ[[1,1]] + confmatrix_PHGJ[[2,2]]) / sum(confmatrix_PHGJ)
(confmatrix_PHGK[[1,1]] + confmatrix_PHGK[[2,2]]) / sum(confmatrix_PHGK)

#WEBPAGE DEVELOPMENT
stateData = read.table(
 text = "State District
 Selangor 'Gombak (Batu 10)'
 Selangor 'Hulu Selangor (Tanjung Malim)'
 Selangor 'Kuala Selangor (Rantau Panjang)'
 Terengganu 'Dungun (Kuala Jengai)'
 Terengganu 'Hulu Terengganu (Kuala Ping)'
 Terengganu 'Setiu (Kg Besut)'
 Pahang 'Bentong (Kuala Marong)'
 Pahang 'Jerantut (Kuala Tahan)'
 Pahang 'Kuantan (Cherating)'",
 header = TRUE, stringsAsFactors = FALSE)  #for dropdown list

ui = shinyUI(
  
  navbarPage(
    theme = shinytheme("flatly"), collapsible = F,
    
    tags$head(tags$style(HTML('.navbar-static-top {background-color: #3DA7ED;}',
                              '.navbar-default .navbar-nav>.active>a {background-color: #3DA7ED;}',
                              '.navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
                                color: #FFFFFF !important;
                                background-color: #0B82D2 !important;
                                }'))),
    
    title = span("Flood Susceptibility Predictor", style = "color: white"),
    
    tabPanel("Susceptibility Checker",
             fluidRow( 
                 setBackgroundColor(
                 color = c("#F4F4F4", "#C7D0D8"),
                 gradient = "linear",
                 direction = "top",
                 shinydashboard = FALSE),
               
               column(2, 
                      div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:20px; margin-bottom: 30px; color: white; text-align: center;",icon("calendar", class = "about-icon fa-pull-right"),icon("calendar", class = "about-icon fa-pull-left"), 
                          span(format(Sys.time(), "%a, %e %b %Y"), style = "font-size:19px") ),
                      
                      div(style="background:#0B82D2;border-radius: 100px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; padding:20px; margin-bottom: 10px; color: white; text-align: center;", 
                          conditionalPanel("input.district != ''", div(style='font-size: 19px;text-align: center',icon("cloud-rain", class = "about-icon fa-pull-right"),icon("cloud-rain", class = "about-icon fa-pull-left"), "Rainfall:", textOutput("rain", inline = TRUE), "(mm)") ), 
                          conditionalPanel("input.district == ''", div(style='font-size: 19px;',icon("cloud-rain", class = "about-icon fa-pull-right"),icon("cloud-rain", class = "about-icon fa-pull-left"), span(tags$strong(p("No Data")), style = "color: #FF2525; text-align: center"))) ),
                      
                      div(style="background:#0B82D2; border-radius: 100px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; padding:20px; margin-bottom: 10px; color: white; text-align: center;", 
                          conditionalPanel("input.district != ''", div(style='font-size: 19px;text-align: center',icon("ruler-vertical", class = "about-icon fa-pull-right"),icon("ruler-vertical", class = "about-icon fa-pull-left"), "Water Level:", textOutput("level", inline = TRUE), "(m)" ) ), 
                          conditionalPanel("input.district == ''", div(style='font-size: 19px;',icon("ruler-vertical", class = "about-icon fa-pull-right"),icon("ruler-vertical", class = "about-icon fa-pull-left"), span(tags$strong(p("No Data")), style = "color: #FF2525; text-align: center")))
                      ),                  
                      
                      
                      ),
                      
               column(style='background-image: linear-gradient(#C7D0D8, #82C5F2); border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px', width=3,  
                        conditionalPanel("input.state != ''", div(style='padding-bottom:10px; display: block; margin-left: 120px; margin-right: 5px;', uiOutput("state_image")) ), 
                        conditionalPanel("input.state == ''", div(style=' padding-bottom:10px; display: block; margin-left: 90px; margin-right: 100px;', 
                                                                span(tags$strong(p("Please select State")), style = " line-height: 90px;color: #FF2525; font-size:20px; text-align: center")) ),
  
                        div(style="display: block; margin-left: 70px; margin-right: 40px;", htmlOutput("state_selector"), htmlOutput("district_selector"))),
               
               column(5, 
                      span(tags$strong(textOutput("T_state")), style = "color: #0B82D2; font-size:50px; text-align: center"), 
                      
                      wellPanel(style = "border: 1px #82C5F2;border-style: solid; background: #FFFFFF;  border-radius: 50px", span(tags$strong(textOutput("T_district")), 
                                                                    style = "color: #82C5F2; font-size:25px; text-align: center")),
                      
                      div(style='border: 10px #0B82D2;border-style: solid; border-radius: 2px; text-align: center; background: #0B82D2;font-size:20px; color: white', "Susceptibility Level" ), 
                      
                      div(style='padding:20px; border: 1px #0B82D2;border-radius: 2px;text-align: center; background: #FFFFFF; font-size:20px; color:#0B82D2; text-decoration-line: underline', 
                          span(tags$strong(textOutput("Flood_data"))))
                     ),
               
               column(2, 
                      div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:20px;color: white; text-align: center;", icon("shield-alt", class = "about-icon fa-pull-right"),icon("shield-alt", class = "about-icon fa-pull-left"), "STATUS"),
                      
                      div(style="background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; 
                           color: #0B82D2; text-align: center;",
                          conditionalPanel("output.Flood_data == 'LOWER SUSCEPTIBILITY'", div(style='text-align: center; margin-top:10px;', 
                                                                                              img(height = 150, width = 170, src = "https://cdn.pixabay.com/photo/2017/03/28/01/46/check-mark-2180770_640.png"), 
                                                                                              span(tags$strong(p("SAFE")), style = "line-height: 45px;color: #009300; text-align: center") ) ), 
                          conditionalPanel("output.Flood_data == 'HIGHER SUSCEPTIBILITY'", div(style='text-align: center; margin-top:10px;', 
                                                                                              img(height = 150, width = 170, src = "https://cdn.pixabay.com/photo/2017/03/28/01/42/attention-2180765_960_720.png"), 
                                                                                              span(tags$strong(p("RISKY")), style = "line-height: 45px;color: #CC2D2D; text-align: center")) ),
                          conditionalPanel("output.Flood_data == 'NO DATA'", div(style='margin-bottom:20px;padding-top:10px', img(
                            height = 150, width = 170, src = "https://cdn.pixabay.com/photo/2012/04/14/16/26/question-34499__340.png", span(tags$strong(p("NO DATA")), style = "line-height: 45px;color: #1E96DC; text-align: center")))) )
                      ),
              
               column(style = 'background-image: linear-gradient(#82C5F2, #0B82D2); border: 1px #82C5F2;border-style: solid; padding-top:10px;margin-top:20px;display: block; padding-bottom:10px;', width = 10,
                      conditionalPanel("input.district == ''", div(style='padding-top:10px;padding-bottom:10px;',  leafletOutput(outputId = "map2" , height="550"))), 
                      conditionalPanel("input.district != ''", div(style='padding-top:10px;padding-bottom:10px;',  leafletOutput(outputId = "map" , height="550")))
                      ),
               
               column(2,
                      div(style="font-size:19px;margin-top:20px;background:#0B82D2;border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px;
                          padding:20px;color: white; text-align: center;", "ANNOUNCEMENT"),
                      
                      div(style="font-size:19px;background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; 
                           color: #0B82D2; text-align: center;",
                          conditionalPanel("output.Flood_data == 'LOWER SUSCEPTIBILITY'", div(style='padding-left:10px;padding-right:10px;padding-bottom:150px;padding-top:150px;color: #009300; text-align: justify;',
                                                                                              p("Everything is safe!"), p("Rainfall intensity is not exceeding normal intensity and/or water level does not rise above normal level"), p("You can keep on doing your daily routines!") ) ), 
                          conditionalPanel("output.Flood_data == 'HIGHER SUSCEPTIBILITY'", div(style='font-size:15.5px;color: #CC2D2D;text-align: justify;padding-left:10px;padding-right:10px;padding-bottom:1px;padding-top:10px;',
                                                                                               p("Be alert!"), p("Rainfall intensity is exceeding normal intenstiy and/or water level is above normal level"), p("It is advised to be prepared with the below procedures:"),
                                                                                               p("1. Ensure you have an evacuation plan"), p("2. Determine what to move up, out or away such as jewellery, medicines, improtant documents, etc."), p("3. Know your nearest temporary
                                                                                               evacuation center"), p("4. Know how to turn off all utilities, such as gas, electrictiy and water"), p("5. Maintain a disaster supply kit at home"), p("6. Ensure you have emergency supplies") ) ),
                          conditionalPanel("output.Flood_data == 'NO DATA'", div(style='padding-bottom:230px;padding-top:230px;',
                                                                                        span(tags$strong(p("NO DATA")), style = "line-height: 45px;color: #0B82D2; text-align: center"))) )
               )
               
               ) ),
    
    tabPanel("Graph and Details",
             fluidRow( 
               setBackgroundColor(
                 color = c("#F4F4F4", "#C7D0D8"),
                 gradient = "radial",
                 direction = "top",
                 shinydashboard = FALSE),
               
               column(2, 
                      div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:20px; margin-bottom: 50px; color: white; text-align: center;",icon("calendar", class = "about-icon fa-pull-right"),icon("calendar", class = "about-icon fa-pull-left"), 
                          format(Sys.time(), "%a, %e %b %Y") ),
                      
                      div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 100px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:10px; margin-bottom: 10px; color: white; text-align: center;", 
                          conditionalPanel("input.district2 != ''", div(style='text-align: center',icon("cloud-rain", class = "about-icon fa-pull-right"),icon("cloud-rain", class = "about-icon fa-pull-left"), 
                                                                       "Rainfall:", textOutput("rain2", inline = TRUE), "(mm)" ) ), 
                          conditionalPanel("input.district2 == ''", div(style='margin-top:10px',icon("cloud-rain", class = "about-icon fa-pull-right"),icon("cloud-rain", class = "about-icon fa-pull-left"), 
                                                                       span(tags$strong(p("No Data")), style = "color: #FF2525; text-align: center"))) ),
                      
                      div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 100px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:10px; margin-bottom: 10px; color: white; text-align: center;", 
                          conditionalPanel("input.district2 != ''", div(style='text-align: center',icon("ruler-vertical", class = "about-icon fa-pull-right"),icon("ruler-vertical", class = "about-icon fa-pull-left"), 
                                                                       "Water Level:", textOutput("level2", inline = TRUE), "(m)" ) ), 
                          conditionalPanel("input.district2 == ''", div(style='margin-top:10px',icon("ruler-vertical", class = "about-icon fa-pull-right"),icon("ruler-vertical", class = "about-icon fa-pull-left"), 
                                                                       span(tags$strong(p("No Data")), style = "color: #FF2525; text-align: center"))) ),
               ),
               
             column(style=' background-image: linear-gradient(#C7D0D8, #82C5F2); border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px', width=3, 
                    conditionalPanel("input.state2 != ''", div(style='padding-bottom:10px; display: block; margin-left: 120px; margin-right: 5px;', uiOutput("state_image2")) ), 
                    conditionalPanel("input.state2 == ''", div(style=' padding-bottom:10px; display: block; margin-left: 90px; margin-right: 100px;', 
                                                               span(tags$strong(p("Please select State")), style = " line-height: 90px;color: #FF2525; font-size:20px; text-align: center"))  ),
                    
                    div(style="display: block; margin-left: 70px; margin-right: 40px;", htmlOutput("state_selector2"), htmlOutput("district_selector2")) 
                ),
             
             column(5, 
                    span(tags$strong(textOutput("T_state2")), style = "color: #0B82D2; font-size:50px; text-align: center"),
                    
                    wellPanel(style = "border: 1px #82C5F2;border-style: solid; background: #FFFFFF;  border-radius: 50px", span(tags$strong(textOutput("T_district2")), style = "color: #82C5F2; font-size:25px; text-align: center")),
                    
                    div(style='border: 1px #0B82D2;border-style: solid; border-radius: 2px; text-align: center; background-image: linear-gradient(#82C5F2, #0B82D2);font-size:20px; color: white; padding-top:5px;padding-bottom:5px;',
                        "Graph Information"), 
                    
                    conditionalPanel("input.radio == 1", 
                           div(style='text-align:center;color:#0B82D2;background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 2px;border: 1px #82C5F2;border-style: solid; 
                           padding-right:5px;padding-left:20px;padding-bottom:5px;padding-top:5px;font-size:20px;', span(p("This graph (scatterplot) depicts the relationship between rainfall intensity and water level of actual data and predicted data."))) ), 
                    conditionalPanel("input.radio == 2",
                          div(style='text-align: center;background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 2px;border: 1px #82C5F2;border-style: solid; color: #0B82D2; padding:20px;font-size:20px;', 
                              span(p("This graph (line chart) depicts the intensity of rainfall per month."))) ),
                    conditionalPanel("input.radio == 3",
                           div(style='text-align: center;background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 2px;border: 1px #82C5F2;border-style: solid; color: #0B82D2; padding:20px;font-size:20px;', 
                               span(p("This graph (line chart) depicts the level of water rise and fall per month."))) ),
                    ),
             
             column(2, 
                    div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; 
                          padding:20px;color: white; text-align: center;", icon("water", class = "about-icon fa-pull-right"),icon("water", class = "about-icon fa-pull-left"), "WATER LEVEL WARNING"),
                    
                    div(style="margin-bottom:15px; background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; 
                           color: #0B82D2;padding-top:10px; padding-left:10px; padding-bottom:10px;", span(tags$strong(textOutput("nlegends")), style = "color: #009300; text-align: center"), span(tags$strong(textOutput("alegends")), style = "color: #CC2D2D; text-align: center"),),
                    
                    div(style="background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; 
                          padding:5px;color: white; text-align: center;", icon("book", class = "about-icon fa-pull-right"),icon("book", class = "about-icon fa-pull-left"),"TYPE OF GRAPH"),
                    
                    div(style="background-image: linear-gradient(#FFFFFF, #F4F4F4);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; margin-left: 10px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; color: #0B82D2; padding-left:10px;",
                        
                        radioButtons("radio", "", choices = list("Flood Susceptibility" = 1, 
                                                                 "Monthly Rainfall Intensity" = 2,
                                                                 "Monthly Water Level" = 3)))
             ),
             
             
             column(style = 'background-image: linear-gradient(#82C5F2, #3DA7ED); border: 1px #82C5F2;border-style: solid; padding-top:10px;margin-top:25px;display: block; 
                      padding-bottom:15px;', width = 10, offset=1, 
                    conditionalPanel("input.radio == 1", div(style='border: 1px black;border-style: solid;display: block; margin-left: auto; margin-right: auto;', plotOutput(outputId = "Flood_graph", width = "100%", height="600")) ), 
                    conditionalPanel("input.radio == 2", div(style='border: 1px black;border-style: solid;display: block; margin-left: auto; margin-right: auto;', plotOutput(outputId = "Rain_graph", width = "100%", height="600"))  ) ,
                    conditionalPanel("input.radio == 3", div(style='border: 1px black;border-style: solid;display: block; margin-left: auto; margin-right: auto;', plotOutput(outputId = "Level_graph", width = "100%", height="600"))  ) ),
             
             column(1,
                    conditionalPanel("input.radio == 1", div(style="background-image: linear-gradient(#3DA7ED, #C7D0D8);border-radius: 10px; margin-left: 5px; margin-right: 10px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:10px; margin-top: 25px;", span(tags$strong(p("LEGENDS")), style = " color: white; text-align: center;"), span(p("• Predicted Data"), style = "color:red;text-align: center;"), span(p("• Actual Data"), style = "color:blue;text-align: center;"))),
                    )
                   )
           ),
    
    tabPanel("About",
             
             column(2, 
                    div(style="background: white;border-radius: 10px; margin-left: 10px; margin-right: 5px; border: 1px #82C5F2;border-style: solid; padding-top:10px; margin-top:3px; 
                          padding:20px; color: #0B82D2; text-align: center;", img(height = 240, width = 170, src = "https://upload.wikimedia.org/wikipedia/commons/7/7c/UTP_LOGO.png"))
                    ),
             
             column(3, 
                    div(style="font-size:25px;margin-top:11px;background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px;border: 1px #82C5F2;border-style: solid; 
                          padding:5px; padding-top:10px;color: white; text-align: center; color: white;", span(tags$strong(p("AUTHOR'S DETAILS"))) ),
                    
                    div(style="font-size:20px;padding-left: 20px; color: darkblue; background-image: linear-gradient(#C7D0D8, #82C5F2);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; 
                        border: 1px #82C5F2;border-style: solid; padding-top:48px; padding-bottom:48px;", span(tags$strong(p("Name: Zainul Irfan bin Zainuddin"))), span(tags$strong(p("ID: 18000800"))), span(tags$strong(p("Course: Information Technology (IT)"))) ) 
                    ),
             
             column(5, 
                    div(style="font-size:25px;margin-top:11px;background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px;border: 1px #82C5F2;border-style: solid; 
                          padding:5px; padding-top:10px;color: white; text-align: center; color: white;", span(tags$strong(p("PROJECT PURPOSE"))) ),
                    
                    div(style="text-align:justify;font-size:18px;padding-left: 20px; padding-right: 20px;color: darkblue; background: white;border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; 
                        border: 1px #82C5F2;border-style: solid; padding-top:20px; padding-bottom:20px;", span(tags$strong(p("This system is developed in partial fulfilment of the requirements for Final Year Project (FYP) II.")), style = "text-align: center"), 
                        span(p("FYP II (TEB3024) is a course in Universiti Teknologi PETRONAS (UTP) that requires students to do project and/or development work in their respective discipline. The purpose of this course is for students to enhance skills in the 
                        process of applying knowledge, expanding thoughts, solving problems independently, and presenting findings.")))                    
                    ),
             
             column(2, 
                    div(style="font-size:25px;margin-top:11px;background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px;border: 1px #82C5F2;border-style: solid; 
                          padding:5px; padding-top:10px;color: white; text-align: center; color: white;", span(tags$strong(p("EXTERNAL LINKS"))) ),
                    
                    div(style="text-align: center; font-size:20px;padding-left: 20px; padding-right: 20px;color: darkblue; background-image: linear-gradient(#C7D0D8, #82C5F2);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; 
                        border: 1px #82C5F2;border-style: solid; padding-top:27px; padding-bottom:27px;", span(tags$strong(p("Dissertation"))), p("'placeholder for link'"), span(tags$strong(p("Viva Slide"))), p("'placeholder for link'") ) 
             ),
             
             column(width = 10, offset=1,  
                    div(style="font-size:25px;margin-top:30px;background-image: linear-gradient(#82C5F2, #0B82D2);border-radius: 10px; border-bottom-left-radius:0px; border-bottom-right-radius:0px;border: 1px #82C5F2;border-style: solid; 
                          padding:5px; padding-top:10px;color: white; text-align: center; color: white;", span(tags$strong(p("SYSTEM PURPOSE"))) ),
                    
                    div(style="text-align:justify;font-size:22px;padding-left: 25px; padding-right: 25px;color: darkblue; background-image: linear-gradient(#82C5F2, #3DA7ED);border-radius: 10px; border-top-left-radius:0px; border-top-right-radius:0px; 
                        border: 1px #82C5F2;border-style: solid; padding-top:25px; padding-bottom:25px;", span(tags$strong(p("A Web-Based Flood Susceptibility Prediction System through Machine Learning using Logistic Regression Algorithm for  
                        Various Area in Malaysia" )), style = "text-align: center"), span(p("Flood Susceptibility Predictor is developed in order to improve the existing disaster early warning systems, thus increasing users' preparedness for future flood occurrences.
                                                                                            The system utilizes the technology of supervised machine learning to operate since the prediction model for this system is taught with a collection of input data that are
                                                                                            associated with the correct output. The system develop the prediction model by using logistic regression as the algorithm prediction model in order to have a Boolean
                                                                                            outcome, higher susceptibility or lower susceptibility.")), tags$br(), span(tags$strong(p("Objectives:"))), span(p("The main objective for this system is to mitigate the damages that
                                                                                                                                                                                                    could have happened to potential flood victims. Other than that:")),
                        span(p("• To investigate the susceptibility for an area to flood and the method to monitor the susceptibility for flood.")), span(p("• To develop system that can predict the susceptibility for an area to flood based on the water level data and intensity of rainfall data ")),
                        span(p("• To study the feasibility of the proposed system."))) 
                    )
             )
    
    ))

server = shinyServer(function(input, output, session) {
  
  output$state_selector = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state", #name of input
                label = "State:", #label displayed in ui
                choices = c("Select State..."='', as.character(unique(stateData$State))),  # calls unique values from the State column in the previously created table
                )
  })
  
  output$state_selector2 = renderUI({
    selectInput(inputId = "state2",
                label = "State:",
                choices = c("Select State..."='', as.character(unique(stateData$State))),
                )
  })
  
  output$district_selector = renderUI({#creates district select box object called in ui
    
    data_available = stateData[stateData$State == input$state, "District"]
    #creates a reactive list of available districts based on the state selection made
    
    selectInput(inputId = "district",
                label = "District:",
                choices = c("Select District..."='', unique(data_available)), #calls list of available districts
                )
  })

  output$district_selector2 = renderUI({
    
    data_available = stateData[stateData$State == input$state2, "District"]
    
    selectInput(inputId = "district2",
                label = "District:",
                choices = c("Select District..."='', unique(data_available)),
                )
  })
  
  #Display Normal water level 
  output$nlegends <- renderText ({
    if (input$district2 == "Gombak (Batu 10)"){
      paste("Normal: < 76.40 (m)")
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
      paste("Normal: < 38.30 (m)")  
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
      paste("Normal: ± 4.50 (m)") 
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
      paste("Normal: ± 14.00 (m)") 
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
      paste("Normal: ± 16.50 (m)") 
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
      paste("Normal: ± 13.30 (m)") 
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
      paste("Normal: < 86.00 (m)")
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
      paste("Normal: ± 60.00 (m)")
    }
    else if (input$district2 == "Kuantan (Cherating)"){
      paste("Normal: < 1.50 (m)")
    }
  })
  
  #Display Alert water level 
  output$alegends <- renderText ({
    if (input$district2 == "Gombak (Batu 10)"){
      paste("Alert: ≥ 76.40 (m)")
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
      paste("Alert: ≥ 38.30 (m)")
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
      paste("Alert: ≥ 6.80 (m)")
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
      paste("Alert: ≥ 19.50 (m)")
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
      paste("Alert: ≥ 17.70 (m)")
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
      paste("Alert: ≥ 16.00 (m)")
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
      paste("Alert: ≥ 86.00 (m)")
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
      paste("Alert: ≥ 64.00 (m)")
    }
    else if (input$district2 == "Kuantan (Cherating)"){
      paste("Alert: ≥ 1.50 (m)")
    }
    else{
      paste("No Data")
    }
  })
  
  #Display State
  output$T_state <- renderText ({
    if (input$state != ""){
      toupper(paste(input$state))
    }
    else if (input$state == ""){
      paste("No State selected")
    }
  })
  
  output$T_state2 <- renderText ({
    if (input$state2 != ""){
      toupper(paste(input$state2))
    }
    else if (input$state2 == ""){
      paste("No State selected")
    }
  })
  
  #Display Strict
  output$T_district<- renderText ({
    if (input$district != ""){
      paste(input$district)
    }
    else if (input$district == ""){
      paste("No District selected")
    }
  })
  
  output$T_district2 <- renderText ({
    if (input$district2 != ""){
      paste(input$district2)
    }
    else if (input$district2 == ""){
      paste("No District selected")
    }
  })
  
  #Display latest water level
  levelInput <- reactive({
    if (input$district == "Gombak (Batu 10)"){
      wlevel <- tail(train_SGG$`Water Level`, n=1)
    }
    else if (input$district == "Hulu Selangor (Tanjung Malim)"){
      wlevel <- tail(train_SGH$`Water Level`, n=1)
    }
    else if (input$district == "Kuala Selangor (Rantau Panjang)"){
      wlevel <- tail(train_SGK$`Water Level`, n=1)
    }
    else if (input$district == "Dungun (Kuala Jengai)"){
      wlevel <- tail(train_TGD$`Water Level`, n=1)
    }
    else if (input$district == "Hulu Terengganu (Kuala Ping)"){
      wlevel <- tail(train_TGH$`Water Level`, n=1)
    }
    else if (input$district == "Setiu (Kg Besut)"){
      wlevel <- tail(train_TGS$`Water Level`, n=1)
    }
    else if (input$district == "Bentong (Kuala Marong)"){
      wlevel <- tail(train_PHGB$`Water Level`, n=1)
    }
    else if (input$district == "Jerantut (Kuala Tahan)"){
      wlevel <- tail(train_PHGJ$`Water Level`, n=1)
    }
    else if (input$district == "Kuantan (Cherating)"){
      wlevel <- tail(train_PHGK$`Water Level`, n=1)
    }
    return(wlevel)
  })
  
  
  output$level <- renderText ({
    levelInput()
  })
  
  levelInput2 <- reactive({
    if (input$district2 == "Gombak (Batu 10)"){
      wlevel2 <- tail(train_SGG$`Water Level`, n=1)
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
      wlevel2 <- tail(train_SGH$`Water Level`, n=1)
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
      wlevel2 <- tail(train_SGK$`Water Level`, n=1)
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
      wlevel2 <- tail(train_TGD$`Water Level`, n=1)
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
      wlevel2 <- tail(train_TGH$`Water Level`, n=1)
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
      wlevel2 <- tail(train_TGS$`Water Level`, n=1)
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
      wlevel2 <- tail(train_PHGB$`Water Level`, n=1)
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
      wlevel2 <- tail(train_PHGJ$`Water Level`, n=1)
    }
    else if (input$district2 == "Kuantan (Cherating)"){
      wlevel2 <- tail(train_PHGK$`Water Level`, n=1)
    }
    return(wlevel2)
  })
  
  
  output$level2 <- renderText ({
    levelInput2()
  })
  
  #Display latest rainfall intensity
  rainInput <- reactive({
    if (input$district == "Gombak (Batu 10)"){
      drop <- tail(train_SGG$Rainfall, n=1)
    }
    else if (input$district == "Hulu Selangor (Tanjung Malim)"){
      drop <- tail(train_SGH$Rainfall, n=1)
    }
    else if (input$district == "Kuala Selangor (Rantau Panjang)"){
      drop <- tail(train_SGK$Rainfall, n=1)
    }
    else if (input$district == "Dungun (Kuala Jengai)"){
      drop <- tail(train_TGD$Rainfall, n=1)
    }
    else if (input$district == "Hulu Terengganu (Kuala Ping)"){
      drop <- tail(train_TGH$Rainfall, n=1)
    }
    else if (input$district == "Setiu (Kg Besut)"){
      drop <- tail(train_TGS$Rainfall, n=1)
    }
    else if (input$district == "Bentong (Kuala Marong)"){
      drop <- tail(train_PHGB$Rainfall, n=1)
    }
    else if (input$district == "Jerantut (Kuala Tahan)"){
      drop <- tail(train_PHGJ$Rainfall, n=1)
    }
    else if (input$district == "Kuantan (Cherating)"){
      drop <- tail(train_PHGK$Rainfall, n=1)
    }
    return(drop)
  })
  
  
  output$rain <- renderText ({
    rainInput()
  })
  
  rainInput2 <- reactive({
    if (input$district2 == "Gombak (Batu 10)"){
      drop2 <- tail(train_SGG$Rainfall, n=1)
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
      drop2 <- tail(train_SGH$Rainfall, n=1)
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
      drop2 <- tail(train_SGK$Rainfall, n=1)
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
      drop2 <- tail(train_TGD$Rainfall, n=1)
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
      drop2 <- tail(train_TGH$Rainfall, n=1)
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
      drop2 <- tail(train_TGS$Rainfall, n=1)
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
      drop2 <- tail(train_PHGB$Rainfall, n=1)
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
      drop2 <- tail(train_PHGJ$Rainfall, n=1)
    }
    else if (input$district2 == "Kuantan (Cherating)"){
      drop2 <- tail(train_PHGK$Rainfall, n=1)
    }
    return(drop2)
  })
  
  
  output$rain2 <- renderText ({
    rainInput2()
  })
  
  #Display susceptibility status
  datasetInput <- reactive({
    if (input$district == "Gombak (Batu 10)"){
      sus <- toupper(tail(sus_SGG, n=1))
    }
    else if (input$district == "Hulu Selangor (Tanjung Malim)"){
      sus <- toupper(tail(sus_SGH, n=1))
    }
    else if (input$district == "Kuala Selangor (Rantau Panjang)"){
      sus <- toupper(tail(sus_SGK, n=1))
    }
    else if (input$district == "Dungun (Kuala Jengai)"){
      sus <- toupper(tail(sus_TGD, n=1))
    }
    else if (input$district == "Hulu Terengganu (Kuala Ping)"){
      sus <- toupper(tail(sus_TGH, n=1))
    }
    else if (input$district == "Setiu (Kg Besut)"){
      sus <- toupper(tail(sus_TGS, n=1))
    }
    else if (input$district == "Bentong (Kuala Marong)"){
      sus <- toupper(tail(sus_PHGB, n=1))
    }
    else if (input$district == "Jerantut (Kuala Tahan)"){
      sus <- toupper(tail(sus_PHGJ, n=1))
    }
    else if (input$district == "Kuantan (Cherating)"){
      sus <- toupper(tail(sus_PHGK, n=1))
    }
    else {
      sus <- paste("NO DATA")
    }
    return(sus)
  })
  
  
  output$Flood_data <- renderText ({
    datasetInput()
  })

  #Display flood susceptibility scatterplot
  graphInput <- reactive({
    if (input$district2 == "Gombak (Batu 10)"){
      gra <- plot(train_SGG$`Water Level`,train_SGG$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall Intensity (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_SGG$`Water Level`,test_SGG$Rainfall, pch=19, col = "red")
                  #legend(77, 55, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
      gra <- plot(train_SGH$`Water Level`,train_SGH$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall Intensity (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_SGH$`Water Level`,test_SGH$Rainfall, pch=19, col = "red")
             #legend(39.9, 10, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
      gra <- plot(train_SGK$`Water Level`,train_SGK$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall Intensity (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_SGK$`Water Level`,test_SGK$Rainfall, pch=19, col = "red")
             #legend(6, 47, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
      gra <- plot(train_TGD$`Water Level`,train_TGD$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall Intensity (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_TGD$`Water Level`,test_TGD$Rainfall, pch=19, col = "red")
             #legend(20, 170, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
      gra <- plot(train_TGH$`Water Level`,train_TGH$Rainfall, 
             xlab="Water Level", ylab="Rainfall", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_TGH$`Water Level`,test_TGH$Rainfall, pch=19, col = "red")
             #legend(19, 100, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
      gra <- plot(train_TGS$`Water Level`,train_TGS$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_TGS$`Water Level`,test_TGS$Rainfall, pch=19, col = "red")
             #legend(16.7, 170, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
      gra <- plot(train_PHGB$`Water Level`,train_PHGB$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_PHGB$`Water Level`,test_PHGB$Rainfall, pch=19, col = "red")
             #legend(86.4, 41, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
      gra <- plot(train_PHGJ$`Water Level`,train_PHGJ$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_PHGJ$`Water Level`,test_PHGJ$Rainfall, pch=19, col = "red")
             #legend(64, 75, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else if (input$district2 == "Kuantan (Cherating)"){
      gra <- plot(train_PHGK$`Water Level`,train_PHGK$Rainfall, 
             xlab="Water Level (m)", ylab="Rainfall (mm)", pch=19, col = "blue", cex.lab=1.2, cex.axis=1.2)
             points( test_PHGK$`Water Level`,test_PHGK$Rainfall, pch=19, col = "red")
             #legend(2.3, 150, legend=c("Predicted Data", "Actual Data"), col=c("red", "blue"), pch=19:19, cex=1.2)
    }
    else {
      gra <- plot(0,type='n',axes=FALSE, xlab="No data", ylab ="No data", cex.lab=1.4, cex.axis=1.4)
    }
    return(gra)
  })
  

  output$Flood_graph <- renderPlot ({
    graphInput()
  })
  
  #Display line chart of rainfall intensity
  output$Rain_graph <- renderPlot({
    if (input$district2 == "Gombak (Batu 10)"){
        train_SGG$Date <- as.Date( train_SGG$Date, '%d/%m/%Y')
        ggplot( data = train_SGG, aes(Date, Rainfall)) + geom_line(color="#0B82D2") + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
        train_SGH$Date <- as.Date( train_SGH$Date, '%d/%m/%Y')
        ggplot( data = train_SGH, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
        train_SGK$Date <- as.Date( train_SGK$Date, '%d/%m/%Y')
        ggplot( data = train_SGK, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
        train_TGD$Date <- as.Date( train_TGD$Date, '%d/%m/%Y')
        ggplot( data = train_TGD, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
        train_TGH$Date <- as.Date( train_TGH$Date, '%d/%m/%Y')
        ggplot( data = train_TGH, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
        train_TGS$Date <- as.Date( train_TGS$Date, '%d/%m/%Y')
        ggplot( data = train_TGS, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
        train_PHGB$Date <- as.Date( train_PHGB$Date, '%d/%m/%Y')
        ggplot( data = train_PHGB, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
        train_PHGJ$Date <- as.Date( train_PHGJ$Date, '%d/%m/%Y')
        ggplot( data = train_PHGJ, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Kuantan (Cherating)"){
        train_PHGK$Date <- as.Date( train_PHGK$Date, '%d/%m/%Y')
        ggplot( data = train_PHGK, aes(Date, Rainfall)) + geom_line(color='#0B82D2') + 
        xlab("Date (month)") + ylab("Rainfall Intensity (mm)") + theme(text = element_text(size = 15)) 
    }
    else {
        plot(0,type='n',axes=FALSE, xlab="No data", ylab ="No data", cex.lab=1.4, cex.axis=1.4)
    }
  })
  
  #Display line chart of water level
  output$Level_graph <- renderPlot({
    if (input$district2 == "Gombak (Batu 10)"){
        train_SGG$Date <- as.Date( train_SGG$Date, '%d/%m/%Y')
        ggplot( data = train_SGG, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 76.40, col="#CC2D2D") +
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Hulu Selangor (Tanjung Malim)"){
        train_SGH$Date <- as.Date( train_SGH$Date, '%d/%m/%Y')
        ggplot( data = train_SGH, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 38.30, col="#CC2D2D") +
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Kuala Selangor (Rantau Panjang)"){
        train_SGK$Date <- as.Date( train_SGK$Date, '%d/%m/%Y')
        ggplot( data = train_SGK, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 4.50, col="#009300") + geom_hline(yintercept = 6.80, col="#CC2D2D") + 
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Dungun (Kuala Jengai)"){
        train_TGD$Date <- as.Date( train_TGD$Date, '%d/%m/%Y')
        ggplot( data = train_TGD, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 14, col="#009300") + geom_hline(yintercept = 19.50, col="#CC2D2D") + 
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Hulu Terengganu (Kuala Ping)"){
        train_TGH$Date <- as.Date( train_TGH$Date, '%d/%m/%Y')
        ggplot( data = train_TGH, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 16.50, col="#009300") + geom_hline(yintercept = 17.70, col="#CC2D2D") + 
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Setiu (Kg Besut)"){
        train_TGS$Date <- as.Date( train_TGS$Date, '%d/%m/%Y')
        ggplot( data = train_TGS, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 13.30, col="#009300") + geom_hline(yintercept = 16, col="#CC2D2D") + 
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Bentong (Kuala Marong)"){
        train_PHGB$Date <- as.Date( train_PHGB$Date, '%d/%m/%Y')
        ggplot( data = train_PHGB, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 86, col="#CC2D2D") + 
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Jerantut (Kuala Tahan)"){
        train_PHGJ$Date <- as.Date( train_PHGJ$Date, '%d/%m/%Y')
        ggplot( data = train_PHGJ, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 60, col="#009300") + geom_hline(yintercept = 64, col="#CC2D2D") +
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else if (input$district2 == "Kuantan (Cherating)"){
        train_PHGK$Date <- as.Date( train_PHGK$Date, '%d/%m/%Y')
        ggplot( data = train_PHGK, aes(Date, `Water Level`)) + geom_line(color='#0B82D2') + 
        geom_hline(yintercept = 1.5, col="#CC2D2D") +
        xlab("Date (month)") + ylab("Water Level (m)") + theme(text = element_text(size = 15)) 
    }
    else {
        plot(0,type='n',axes=FALSE, xlab="No data", ylab ="No data", cex.lab=1.4, cex.axis=1.4)
      
    }
  })
  
  #Display Flag Image
  output$state_image <- renderUI ({
    {
      if (input$state == "Selangor"){
        img(height = 100, width = 200, src = "https://upload.wikimedia.org/wikipedia/commons/0/0c/Flag_of_Selangor.svg")
      }
      else if (input$state == "Terengganu"){
        img(height = 100, width = 200, src = "https://upload.wikimedia.org/wikipedia/commons/6/6b/Flag_of_Terengganu.svg")
      } 
      else if (input$state == "Pahang"){
        img(height = 100, width = 200, src = "https://upload.wikimedia.org/wikipedia/commons/a/aa/Flag_of_Pahang.svg")
      }
    }
  })
  
  output$state_image2 <- renderUI ({
    {
      if (input$state2 == "Selangor"){
        img(height = 100, width = 200, src = "https://upload.wikimedia.org/wikipedia/commons/0/0c/Flag_of_Selangor.svg")
      }
      else if (input$state2 == "Terengganu"){
        img(height = 100, width = 200, src = "https://upload.wikimedia.org/wikipedia/commons/6/6b/Flag_of_Terengganu.svg")
      } 
      else if (input$state2 == "Pahang"){
        img(height = 100, width = 200, src = "https://upload.wikimedia.org/wikipedia/commons/a/aa/Flag_of_Pahang.svg")
      }
    }
  })
  
  #Display map
  output$map <- renderLeaflet({
            {
            if (input$district == "Gombak (Batu 10)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=101.72652531282041, lat=3.267121326446296, popup="Gombak (Batu 10)")
            }
            else if (input$district == "Hulu Selangor (Tanjung Malim)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=101.52000878375452, lat=3.679467136307345, popup="Hulu Selangor (Tanjung Malim)")
              }
            else if (input$district == "Kuala Selangor (Rantau Panjang)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=101.44307541372505, lat=3.402748444841658, popup="Kuala Selangor (Rantau Panjang)")
            }
            else if (input$district == "Dungun (Kuala Jengai)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=103.08770469113253, lat=4.734533210349149, popup="Dungun (Kuala Jengai)")
            }     
            else if (input$district == "Hulu Terengganu (Kuala Ping)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=102.91791753676395, lat=5.162931499215839, popup="Hulu Terengganu (Kuala Ping)")
            } 
            else if (input$district == "Setiu (Kg Besut)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=102.68609144069414, lat=5.487030831830899, popup="Setiu (Kg Besut)")
            } 
            else if (input$district == "Bentong (Kuala Marong)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=101.91430914912544, lat=3.512821040293908, popup="Bentong (Kuala Marong)")
              }   
            else if (input$district == "Jerantut (Kuala Tahan)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=102.40378175586775, lat=4.383270436349302, popup="Jerantut (Kuala Tahan)")
              }        
            else if (input$district == "Kuantan (Cherating)"){
              leaflet() %>%
              addTiles() %>%
              addMarkers(lng=103.39366330962727, lat=4.130165468169508, popup="Kuantan (Cherating)")
            }
            else {
              leaflet() %>%
              addTiles() %>%
              setView(101.9758, 4.2105, zoom = 6) %>%
              addMarkers(lng=101.9758, lat=4.2105, popup="Malaysia")
              }
              
             }
               })
  
  output$map2 <- renderLeaflet({
    {
      if (input$state == "Selangor"){
        leaflet() %>%
        addTiles() %>%
        setView(101.5183, 3.0738, zoom = 9) %>%
        addMarkers(lng=101.5183, lat=3.0738, popup="Selangor")
      }
      else if (input$state == "Terengganu"){
        leaflet() %>%
        addTiles() %>%
        setView(103.1324, 5.3117, zoom = 9) %>%
        addMarkers(lng=103.1324, lat=5.3117, popup="Terengganu")
      }
      else if (input$state == "Pahang"){
        leaflet() %>%
        addTiles() %>%
        setView(103.3256, 3.8126, zoom = 9) %>%
        addMarkers(lng=103.3256, lat=3.8126, popup="Pahang")
      }
      else {
        leaflet() %>%
        addTiles() %>%
        setView(101.9758, 4.2105, zoom = 6) %>%
        addMarkers(lng=101.9758, lat=4.2105, popup="Malaysia")
      }
    }
  })
  
})#close the shinyServer

shinyApp(ui = ui, server = server) #launch shinyApp