# Code demonstrating how one could equalize PPV and NPV across African-American and
# Caucasian defendants by using two thresholds per group.

library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

# hyperparameter for plots
cex_factor = 1.7


# Load and clean the data selecting for the desired fields
raw_data <- read.csv("~/team-mangalyaan/datasets/compas-scores-two-years.csv")
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
                    filter(days_b_screening_arrest <= 30) %>%
                    filter(days_b_screening_arrest >= -30) %>%
                    filter(is_recid != -1) %>%
                    filter(c_charge_degree != "O") %>%
                    filter(score_text != 'N/A')

df_female = filter(df,sex=="F" | sex=="Female")
df_male = filter(df,sex=="Male" | sex=="M")
num_female = nrow(df_female)
num_male = nrow(df_male)

# Compute frequency distributions for COMPAS scores for both the groups
female_score_freq = 1:10
male_score_freq = 1:10
female_score_pdf = 1:10
male_score_pdf = 1:10
for (i in seq(1,10,by=1)){
  female_score_freq[i] = nrow(filter(df,sex=="F"| sex=="Female", decile_score==i))
  female_score_pdf[i] = female_score_freq[i]/num_female
  male_score_freq[i] = nrow(filter(df,sex=="Male" | sex=="M", decile_score==i))
  male_score_pdf[i] = male_score_freq[i]/num_male
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
    for(i in seq(threshold,10,by=1)){
      total_members = total_members + nrow(filter(df,sex==r,decile_score==i))
      total_recid = total_recid + nrow(filter(df,sex==r,decile_score==i, is_recid==1))
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
    if(threshold <1){
      # no one is marked will not recidivate
      return(1)
    }
    for(i in seq(1,threshold,by=1)){
      total_members = total_members + nrow(filter(df,sex==r,decile_score==i))
      total_not_recid = total_not_recid + nrow(filter(df,sex==r,decile_score==i, is_recid==0))
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
female_ppv = 1:11
male_ppv = 1:11
female_npv = 1:11
male_npv = 1:11
for(t in seq(1,11)){
  female_ppv[t] = predictiveValue(t,positive=T,r="F")
  male_ppv[t] = predictiveValue(t,positive=T,r="Male")
  female_npv[t] = predictiveValue(t-1,positive=F,r="F")
  male_npv[t] = predictiveValue(t-1,positive=F,r="Male")
}
cat("female_ppv: ", female_ppv,"\n")
cat("male_ppv: ", male_ppv,"\n")
cat("female_ppv: ", female_ppv,"\n")
cat("male_ppv: ", male_ppv,"\n")
###


(female_score_pdf[3]+female_score_pdf[4])
print(male_score_pdf[5])

# Pick colors to be used in the barplots
col1 = "lightblue1"#"darkturquoise"
col2 = "dimgray"
col3 = "salmon"

pdf('female_thresholds.pdf')
barplot(female_score_pdf,beside=F,names.arg=1:10, ylab="", xlab="Decile Score", ylim=c(0,0.15),
        main=paste("Thresholds for female scores","\nPPV = 0.682, NPV = 0.699, ","Deferrals = 20%"),
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
barplot(male_score_pdf,beside=F,names.arg=1:10, ylab="", xlab="Decile Score", ylim=c(0,0.3),
        main=paste("Thresholds for male scores","\nPPV = 0.677, NPV = 0.684, ","Deferrals = 9%"),
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

