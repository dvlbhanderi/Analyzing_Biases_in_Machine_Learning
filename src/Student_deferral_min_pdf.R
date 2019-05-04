library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)


# hyperparameter for plots
cex_factor = 1.7

# Load and clean the data selecting for the desired fields
raw_data <- read.csv("./student-mat2.txt")
df <- dplyr::select(raw_data, school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3,is_pass)

df_females = filter(df,sex=="F")
df_males = filter(df,sex=="M")
num_females = nrow(df_females)
num_males = nrow(df_males)

# Compute frequency distributions for COMPAS scores for both the groups
female_score_freq = 1:10
male_score_freq = 1:10
female_score_pdf = 1:10
male_score_pdf = 1:10
for (i in seq(1,10,by=1)){
  female_score_freq[i] = nrow(filter(df,sex=="F", G3==i))
  male_score_pdf[i] = female_score_freq[i]/num_females
  female_score_freq[i] = nrow(filter(df,sex=="M", G3==i))
  male_score_pdf[i] = white_score_freq[i]/num_whites
}

score_pdfs = rbind(female_score_pdf,male_score_pdf)


# MIN PDF target
min_pdf = pmin(female_score_pdf,male_score_pdf)
female_deferrals = 1:10
male_deferrals = 1:10
for(i in seq(1,10,by=1)){
  if(female_score_pdf[i] < male_score_pdf[i]){
    female_deferrals[i] = 0
    male_deferrals[i] = nrow(filter(df,sex=="M", G3==i)) * (1 - female_score_pdf[i]/male_score_pdf[i])

  }
  if(female_score_pdf[i] > male_score_pdf[i]){
    female_deferrals[i] = nrow(filter(df,sex=="F", G3==i)) *(1 - male_score_pdf[i]/female_score_pdf[i])
    male_deferrals[i] = 0
  }
}

female_nondeferrals = female_score_freq - female_deferrals
male_nondeferrals = male_score_freq - male_deferrals



deferrals = rbind(female_deferrals,male_deferrals)

deferrals_frac = rbind(female_deferrals/female_score_freq, male_deferrals/male_score_freq)


total_female_deferral_fraction = Reduce(f="+",x = female_deferrals,accumulate=F)/num_females
cat("Total female deferral fraction: ",total_female_deferral_fraction,"\n")

total_male_deferral_fraction = Reduce(f="+",x = male_deferrals,accumulate=F)/num_males
cat("Total male deferral fraction: ",total_male_deferral_fraction,"\n")

# Pick colors to be used in the barplots
col_female = "gray38"
col_female_deferral = "gray70"
col_male = "orange2"
col_male_deferral = "moccasin"


# Generate barplot to display the deferrals under the min-PDF method
dat = cbind(rbind(min_pdf,female_score_pdf-min_pdf,0,0), rbind(0,0,min_pdf,male_score_pdf-min_pdf))
dat = dat[,c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20)]

pdf('min_method.pdf')
barplot(dat, space=c(0.5,0.), col=c(col_female,col_female_deferral,col_male,col_male_deferral), ylim = c(0,0.3),
        ylab="", xlab="Decile Score",names.arg=seq(1,10.5,by=0.5),
        main="Equalizing AP using the minimum method.\n Total deferral rate in each group = 24.5%",
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
barplot(dat,add=T, space=c(0.5,0.),angle=c(0,45,0,45),density=c(0,20,0,20), col=c("male","male","female","female"),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
        legend=c("female non deferrals","female deferrals",
                      "male non deferrals","male deferrals"),
        fill = c(col_female,col_female_deferral,col_male,col_male_deferral))
legend("topright",
        legend=c("Female non deferrals","Female deferrals",
                      "Male non deferrals","Male deferrals"),
        density=c(0,20,0,20), fill=c("male","male","female","female"))
abline(h=0,lwd=2)
dev.off()