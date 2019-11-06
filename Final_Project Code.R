# first part ---------------------------------------------------------------------------------------------
# PCA, principle component analysis ----------------------------------------------------------------------
rm(list = ls())
library(ggplot2)
whole_data <- read.csv("clean_final_data.csv", header = TRUE, sep = ',')
# year of 2018
data_princeton <- whole_data[whole_data$University == 'Princeton University',]
data_columbia <- whole_data[whole_data$University == 'Columbia University in the city of New York',]
data_colorado <- whole_data[whole_data$University == 'University of Colorado Boulder',]
data_northeastern <-whole_data[whole_data$University == 'Northeastern University',]
data_iowa <- whole_data[whole_data$University == 'Iowa State University',]

# define the mathmetical function-----------------------------------------------------------------------------
contribution_estimator <- function(arg1, arg2){
  ans = abs(arg1 - arg2) / 100
  return(ans)
}
# ----------------------------------------------------------------------------------------------
# compute the mathmetical function reasult we built, for each university over decades
university_list <- list(data_princeton,data_columbia,data_colorado,data_northeastern,data_iowa)
# create matrixs to store each university's result
matrix_1 <- matrix(data = NA, nrow = 8, ncol = 9)
matrix_2 <- matrix(data = NA, nrow = 8, ncol = 9)
matrix_3 <- matrix(data = NA, nrow = 8, ncol = 9)
matrix_4 <- matrix(data = NA, nrow = 8, ncol = 9)
matrix_5 <- matrix(data = NA, nrow = 8, ncol = 9)
loop_num = 1
for (university in university_list) {
  # loop for university
  temp <- matrix(data = NA, nrow = 8, ncol = 9)
  for (col in 3:10) {
    # loop for factor
    for (row in 1:9) {
      # loop for year
      factor_rank = university[row,col]
      general_rank = university[row, 11]
      ans = contribution_estimator(factor_rank, general_rank)
      temp[col-2,row] <- ans
    }
  }
  print(paste("This is ",loop_num,"th loop"))
  if (loop_num == 1){
    matrix_1 = temp
  } else if(loop_num == 2){
    matrix_2 = temp
  } else if(loop_num == 3){
    matrix_3 = temp
  } else if(loop_num == 4){
    matrix_4 = temp
  } else if(loop_num == 5){
    matrix_5 = temp
  }
  loop_num = loop_num + 1
}
# place to store each university's result
# View(matrix_1)
university_result_list <- list(a = matrix_1, b = matrix_2, c = matrix_3, d = matrix_4, e=matrix_5)
# use [[]] to choose specific matrix result
# View(university_result_list[[1]]) ----------------------------------------------------------------------------------------------
# compute the sum result 
factor_sum_result_for_five_schools <- matrix(data = NA, nrow = 5, ncol = 8)
# 5 == number of school, 8 == number of factor
for (i in 1:5){
  # loop for school
  for (j in 1:8){
    # loop for factor
    factor_sum_result_for_five_schools[i,j] <- sum(university_result_list[[i]][j,])
  }
}
# -----------------------------------------------------------------------------------------------------------
# graph for the factor and general rank curve
graph_viewer <- function(i){
  year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
  f1 = university_list[[i]][,3]
  f2 = university_list[[i]][,4]
  f3 = university_list[[i]][,5]
  f4 = university_list[[i]][,6]
  f5 = university_list[[i]][,7]
  f6 = university_list[[i]][,8]
  f7 = university_list[[i]][,9]
  f8 = university_list[[i]][,10]
  generalRank = university_list[[i]][,11]
  plot(year, f1, col="red", ylim=c(0,100), xlab = 'year', ylab = 'ranks', type = 'l')
  lines(year, f2, col="green")
  lines(year, f3, col="blue")
  lines(year, f4, col="black")
  lines(year, f5, col="yellow")
  lines(year, f6, col="#D2B48C")
  lines(year, f7, col="#FF7F50")
  lines(year, f8, col="#B8860B")
  lines(year, generalRank, col='orange', lwd=3)# the most thick one is general rank
}
# for single school, factor&general rank----------------------------------------------------
# input is the # of university, 1 is princeton, 2 is columbia...
graph_viewer(1)
graph_viewer(2)
# ...
# for single school, function value curve----------------------------------------------------
function_value_viewer <- function(i){
  school = c('f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8')
  sum_result = data.frame(x = school, y = factor_sum_result_for_five_schools[i,])
  ggplot(data = sum_result, mapping = aes(x = x, y = y)) + geom_bar(stat = 'identity')
}
# input is the # of university, 1 is princeton, 2 is columbia---------------------------------------------------
function_value_viewer(1)
# to view the sum value of each factors for the second university use:
function_value_viewer(2)
# ....

# second part ------------------------------------------------------------------------------------------
# correlation plot -------------------------------------------------------------------------------------
rm(list = ls())
whole_data <- read.csv("final_whole_data.csv", header = TRUE , sep = ",")
GR <- whole_data$General.Uni.Ranking
f1 <- whole_data$Admission.rate
f2 <- whole_data$Average.SAT.equivalent.score.of.students.admitted
f3 <- whole_data$Avg.Cost.of.Attendance.academic.year.instituion.
f4 <- whole_data$in.state.tuition.and.fee
f5 <- whole_data$out.state.tuition.and.fee
f6 <- whole_data$Completion.rate.for.first.time..full.time.students.at.four.year.institutions..150..of.expected.time.to.completion.
f7 <- whole_data$Number.of.undergraduate.student
f8 <- whole_data$Number.of.graduate.student
plot(f1, GR, main = "corelation plot", xlab = "admission rate", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f2, GR, main = "corelation plot", xlab = "avg of SAT", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f3, GR, main = "corelation plot", xlab = "avg cost", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f4, GR, main = "corelation plot", xlab = "in-state fee", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f5, GR, main = "corelation plot", xlab = "out-state fee", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f6, GR, main = "corelation plot", xlab = "completion rate", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f7, GR, main = "corelation plot", xlab = "num of undergraduate", ylab="US News Rank", pch = 18, frame = FALSE)
plot(f8, GR, main = "corelation plot", xlab = "num of graduate", ylab="US News Rank", pch = 18, frame = FALSE)
# Third part -------------------------------------------------------------------------------------------
# Location visualization -------------------------------------------------------------------------------
rm(list = ls())
# histogram graph
dataframe3<-read.csv("University task3_v1.csv")
summary(dataframe3)
ggplot(data = dataframe3) +
  geom_bar(mapping = aes(x = State)) +
  xlab('State')
# boxplot graph
whole_data <- read.csv("final_whole_data.csv", header = TRUE , sep = ",")
state <- whole_data$State
GR <- whole_data$General.Uni.Ranking
plot(state, GR, main = "corelation plot", xlab = "num of graduate", ylab="US News Rank", pch = 18, frame = FALSE)
# Fourth Part -------------------------------------------------------------------------------------------
# boxplot -----------------------------------------------------------------------------------------------
rm(list = ls())
dataframe2<-read.csv("University task1_v1_alpha.csv")
ALL_2016 = dataframe2[dataframe2$Year == 2016,]
ALL_2008 = dataframe2[dataframe2$Year == 2008,]
AR_2016 = ALL_2016[7]
AR_2008 = ALL_2008[7]
NOUGS_2016 = ALL_2016[13]
NOUGS_2008 = ALL_2008[13]

data = c(Year2008 = AR_2008, Year2016 = AR_2016)
boxplot(data,xlab="For Years 2008 and 2016",ylab="Admission Rate")

data1 = c(Year2008 = NOUGS_2008, Year2016 = NOUGS_2016)
boxplot(data1,xlab="For Years 2008 and 2016",ylab="Number of Undergrade Students")

# Fifth Part --------------------------------------------------------------------------------------------
# line graph --------------------------------------------------------------------------------------------
rm(list = ls())
years <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
dataframe2<-read.csv("University task1_v1_alpha.csv")

#Mean Admission rate for Northeasten for years 2008 to 2016.
Mean_NEU_Adm_R<-mean(dataframe2$Admission.rate[274:282],na.rm=TRUE)
NEU_Admission_Rate <- c(dataframe2$Admission.rate[274:282]) #10 years data combined into a vector
plot(years,NEU_Admission_Rate, type="o",xlab="Years",ylab="NEU Admission Rate")
abline(h = Mean_NEU_Adm_R, col="red")

#Number of Graduate students of NEU
Mean_NEU_NOGS<-mean(dataframe2$Number.of.graduate.student[274:282],na.rm=TRUE)
NEU_NOGS <- c(dataframe2$Number.of.graduate.student[274:282])
plot(years,NEU_NOGS, type="o",xlab="Years",ylab="No. of Graduate Students")
abline(h = Mean_NEU_NOGS, col="red")

#Number of Undergrad students of NEU
Mean_NEU_NOUGS<-mean(dataframe2$Number.of.undergraduate.student[274:282],na.rm=TRUE)
NEU_NOUGS <- c(dataframe2$Number.of.undergraduate.student[274:282])
plot(years,NEU_NOUGS, type="o",xlab="Years",ylab="No. of UnderGraduate Students")
abline(h = Mean_NEU_NOUGS, col="red")

#Completion Rate for NEU
Mean_NEU_CRFT<-mean(dataframe2$Completion.rate.for.first.time..full.time.students.at.four.year.institutions..150..of.expected.time.to.completion.[274:282],na.rm=TRUE)
NEU_CRFT <- c(dataframe2$Completion.rate.for.first.time..full.time.students.at.four.year.institutions..150..of.expected.time.to.completion.[274:282])
plot(years,NEU_CRFT, type="o",xlab="Years",ylab="Completion Rate")
abline(h = Mean_NEU_CRFT, col="red")

#Average cost of attendence for NEU
Mean_NEU_ACOA<-mean(dataframe2$Avg.Cost.of.Attendance.academic.year.instituion.[274:282],na.rm=TRUE)
NEU_ACOA <- c(dataframe2$Avg.Cost.of.Attendance.academic.year.instituion.[274:282])
plot(years,NEU_ACOA,  type="o",xlab="Years",ylab="Average Cost of Attendence")
abline(h = Mean_NEU_ACOA, col="red")

#Average SAT SCORE of attendence for NEU
Mean_NEU_ASESOSA<-mean(dataframe2$Average.SAT.equivalent.score.of.students.admitted[274:282],na.rm=TRUE)
NEU_ASESOSA <- c(dataframe2$Average.SAT.equivalent.score.of.students.admitted[274:282])
plot(years,NEU_ASESOSA,  type="o",xlab="Years",ylab="Average SAT Score")
abline(h = Mean_NEU_ASESOSA, col="red")

#Average In state tuition fee for NEU
Mean_NEU_ISTF<-mean(dataframe2$in.state.tuition.and.fee[274:282],na.rm=TRUE)
NEU_ISTF <- c(dataframe2$in.state.tuition.and.fee[274:282])
plot(years,NEU_ISTF,type="o",xlab="Years",ylab="In State Tuition Fee")
abline(h = Mean_NEU_ISTF, col="red")

#Average Out state tuition fee for NEU
Mean_NEU_OSTF<-mean(dataframe2$out.state.tuition.and.fee[274:282],na.rm=TRUE)
NEU_OSTF <- c(dataframe2$out.state.tuition.and.fee[274:282])
plot(years,NEU_OSTF,  type="o",xlab="Years",ylab="Out-State Tuition Fee")
abline(h = Mean_NEU_OSTF, col="red")

###
#Princeton
### Plotting Yearwise Data

#Mean Admission rate for Princeton for years 2008 to 2016.
Mean_PRN_Adm_R<-mean(dataframe2$Admission.rate[319:327],na.rm=TRUE)
PRN_Admission_Rate <- c(dataframe2$Admission.rate[319:327])
plot(years,PRN_Admission_Rate, type="o",xlab="Years",ylab="Princeton Admission Rate")
abline(h = Mean_PRN_Adm_R, col="red")

#Number of Graduate students of Princeton
Mean_PRN_NOGS<-mean(dataframe2$Number.of.graduate.student[319:327],na.rm=TRUE)
PRN_NOGS <- c(dataframe2$Number.of.graduate.student[319:327])
plot(years,PRN_NOGS, type="o",xlab="Years",ylab="No. of Graduate Students")
abline(h = Mean_PRN_NOGS, col="red")

#Number of Undergrad students of Princeton
Mean_PRN_NOUGS<-mean(dataframe2$Number.of.undergraduate.student[319:327],na.rm=TRUE)
PRN_NOUGS <- c(dataframe2$Number.of.undergraduate.student[319:327])
plot(years,PRN_NOUGS, type="o",xlab="Years",ylab="No. of UnderGraduate Students")
abline(h = Mean_PRN_NOUGS, col="red")

#Completion Rate for Princeton
Mean_PRN_CRFT<-mean(dataframe2$Completion.rate.for.first.time..full.time.students.at.four.year.institutions..150..of.expected.time.to.completion.[319:327],na.rm=TRUE)
PRN_CRFT <- c(dataframe2$Completion.rate.for.first.time..full.time.students.at.four.year.institutions..150..of.expected.time.to.completion.[319:327])
plot(years,PRN_CRFT, type="o",xlab="Years",ylab="Completion Rate")
abline(h = Mean_PRN_CRFT, col="red")

#Average cost of attendence for Princeton
Mean_PRN_ACOA<-mean(dataframe2$Avg.Cost.of.Attendance.academic.year.instituion.[319:327],na.rm=TRUE)
PRN_ACOA <- c(dataframe2$Avg.Cost.of.Attendance.academic.year.instituion.[319:327])
plot(years,PRN_ACOA,  type="o",xlab="Years",ylab="Average Cost of Attendence")
abline(h = Mean_PRN_ACOA, col="red")

#Average SAT SCORE of attendence for Princeton
Mean_PRN_ASESOSA<-mean(dataframe2$Average.SAT.equivalent.score.of.students.admitted[319:327],na.rm=TRUE)
PRN_ASESOSA <- c(dataframe2$Average.SAT.equivalent.score.of.students.admitted[319:327])
plot(years,PRN_ASESOSA,  type="o",xlab="Years",ylab="Average SAT Score")
abline(h = Mean_PRN_ASESOSA, col="red")

#Average In state tuition fee for Princeton
Mean_PRN_ISTF<-mean(dataframe2$in.state.tuition.and.fee[319:327],na.rm=TRUE)
PRN_ISTF <- c(dataframe2$in.state.tuition.and.fee[319:327])
plot(years,PRN_ISTF,type="o",xlab="Years",ylab="In State Tuition Fee")
abline(h = Mean_PRN_ISTF, col="red")

#Average Out state tuition fee for Princeton
Mean_PRN_OSTF<-mean(dataframe2$out.state.tuition.and.fee[319:327],na.rm=TRUE)
PRN_OSTF <- c(dataframe2$out.state.tuition.and.fee[319:327])
plot(years,PRN_OSTF,  type="o",xlab="Years",ylab="Out-State Tuition Fee")
abline(h = Mean_PRN_OSTF, col="red")

# Sixth Part --------------------------------------------------------------------------------------------
# hypothesis Test----------------------------------------------------------------------------------------
rm(list = ls())
whole_data <- read.csv("final_whole_data.csv", header = TRUE , sep = ",")
MASample <- whole_data[whole_data$State == 'MA',]

# one sample z test
# get z value
z.test <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / (sqrt(var/(n))) 
  return(z)
}
# hypothesis test for admission rate-----------------------------------
z_score = z.test(MASample$Admission.rate, whole_data$Admission.rate)
# performing the t test
t.test(MASample$Admission.rate, mu = mean(whole_data$Admission.rate))
# hypothesis test for num of undergraduate students--------------------
z_score = z.test(MASample$Number.of.undergraduate.student, whole_data$Number.of.undergraduate.student)
t.test(MASample$Number.of.undergraduate.student, mu = mean(whole_data$Number.of.undergraduate.student))

