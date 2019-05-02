library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

# hyperparameter for plots
cex_factor = 1.6


# Load and clean the data selecting for the desired fields
raw_data <- read.csv("./student-mat2.txt")
df <- dplyr::select(raw_data, school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3,is_pass)

df_females = filter(df,sex=="F")
df_males = filter(df,sex=="M")
num_females = nrow(df_females)
num_males = nrow(df_males)

# Compute frequency distributions for COMPAS scores for both the groups
female_score_freq = 1:20
male_score_freq = 1:20
female_score_pdf = 1:20
male_score_pdf = 1:20
for (i in seq(1,20,by=2)){
  female_score_freq[i] = nrow(filter(df,sex=="F", G3==i))
  female_score_pdf[i] = female_score_freq[i]/num_females
  male_score_freq[i] = nrow(filter(df,sex=="M", G3==i))
  male_score_pdf[i] = male_score_freq[i]/num_males
}


# function to compute PPV or NPV for a group given a threshold
predictiveValue <- function(threshold, positive=T, r="F"){
  if (positive){
    # all members with score >= threshold are marked will recidivate
    total_members = 0
    total_recid = 0
    if(threshold>10){
      # no one is marked will recidivate => PPV = 1
      return(1)
    }
    for(i in seq(threshold,20,by=1)){
      total_members = total_members + nrow(filter(df,sex==r,G3==i))
      total_recid = total_recid + nrow(filter(df,sex==r,G3==i, is_pass==1))
    }
    if(total_members==0){
      return(1)
    }
    else{
      return(total_recid/total_members)
    }
  }
  else{
    # all members with score <= threshold are marked will not recidivate
    total_members = 0
    total_not_recid = 0
    if(threshold < 1){
      # no one is marked will not recidivate
      return(1)
    }
    for(i in seq(1,threshold,by=1)){
      total_members = total_members + nrow(filter(df,sex==r,G3==i))
      total_not_recid = total_not_recid + nrow(filter(df,sex==r,G3==i, is_pass==0))
    }
    if(total_members==0){
      return(1)
    }
    else{
      return(total_not_recid/total_members)
    }
  }
}



# Try all pairs of thresholds to equalize PPV and NPV simultaneously
female_ppv = 1:21
male_ppv = 1:21
female_npv = 1:21
male_npv = 1:21
for(t in seq(1,21)){
  female_ppv[t] = predictiveValue(t,positive=T,r="F")
  male_ppv[t] = predictiveValue(t,positive=T,r="M")
  female_npv[t] = predictiveValue(t-1,positive=F,r="F")
  male_npv[t] = predictiveValue(t-1,positive=F,r="M")
}
cat("Female PPV: ", female_ppv,"\n")
cat("Male PPV: ", male_ppv,"\n")
cat("Female NPV: ", female_npv,"\n")
cat("Male NPV: ", male_npv,"\n")
###


(female_score_pdf[3]+female_score_pdf[4])
print(male_score_pdf[5])

# Pick colors to be used in the barplots
col1 = "lightblue1"#"darkturquoise"
col2 = "dimgray"
col3 = "salmon"

pdf('female_thresholds.pdf')
barplot(female_score_pdf,beside=F,names.arg=1:20, ylab="", xlab="G3", ylim=c(0,0.5),
        main=paste("Thresholds for Female scores","\nPPV = 0.682, NPV = 0.699, ","Deferrals = 20%"),
        col=c(col1,col1,col2,col2,col3,col3,col3,col3,col3,col3),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
      legend = c("0", "Defer","1"),
      fill = c(col1, col2, col3))
abline(h=0, lwd=2)
abline(v=2.4, lwd=3)
abline(v=5, lwd=3)
dev.off()

pdf('male_thresholds.pdf')
barplot(male_score_pdf,beside=F,names.arg=1:20, ylab="", xlab="G3", ylim=c(0,0.5),
        main=paste("Thresholds for Male scores","\nPPV = 0.677, NPV = 0.684, ","Deferrals = 9%"),
        col=c(col1,col1,col1,col1,col2,col3,col3,col3,col3,col3),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
       legend = c("0", "Defer","1"),
       fill = c(col1, col2, col3))
abline(h=0, lwd=2)
abline(v=4.8, lwd=3)
abline(v=6.2, lwd=3)
dev.off()