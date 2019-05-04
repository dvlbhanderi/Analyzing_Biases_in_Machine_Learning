# Code demonstrating how one could equalize the AP of Female and
# Male defendants by the min-PDF method.

library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)


# hyperparameter for plots
cex_factor = 1.7

# Load and clean the data selecting for the desired fields
raw_data <- read.csv("~/team-mangalyaan/datasets/compas-scores-two-years.csv")
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
                    days_b_screening_arrest, decile_score, is_recid, c_jail_in, c_jail_out) %>%
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
  female_score_freq[i] = nrow(filter(df,race=="African-American", decile_score==i))
  female_score_pdf[i] = female_score_freq[i]/num_female
  male_score_freq[i] = nrow(filter(df,race=="Caucasian", decile_score==i))
  male_score_pdf[i] = male_score_freq[i]/num_male
}

score_pdfs = rbind(female_score_pdf,male_score_pdf)


# MIN PDF target
min_pdf = pmin(female_score_pdf,male_score_pdf)
female_deferrals = 1:10
male_deferrals = 1:10
for(i in seq(1,10,by=1)){
  if(female_score_pdf[i] < male_score_pdf[i]){
    female_deferrals[i] = 0
    male_deferrals[i] = nrow(filter(df,race=="Caucasian", decile_score==i)) *(1 - female_score_pdf[i]/male_score_pdf[i])

  }
  if(female_score_pdf[i] > male_score_pdf[i]){
    female_deferrals[i] = nrow(filter(df,race=="African-American", decile_score==i)) *(1 - male_score_pdf[i]/female_score_pdf[i])
    male_deferrals[i] = 0
  }
}

female_nondeferrals = female_score_freq - female_deferrals
male_nondeferrals = male_score_freq - male_deferrals



deferrals = rbind(female_deferrals,male_deferrals)

deferrals_frac = rbind(female_deferrals/female_score_freq, male_deferrals/male_score_freq)


total_female_deferral_fraction = Reduce(f="+",x = female_deferrals,accumulate=F)/num_female
cat("Total female deferral fraction: ",total_female_deferral_fraction,"\n")

total_male_deferral_fraction = Reduce(f="+",x = male_deferrals,accumulate=F)/num_male
cat("Male deferrals",Reduce(f="+",x = male_deferrals,accumulate=F))
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
barplot(dat,add=T, space=c(0.5,0.),angle=c(0,45,0,45),density=c(0,20,0,20), col=c("white","white","black","black"),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
        legend=c("African-American non deferrals","African-American deferrals",
                      "Caucasian non deferrals","Caucasian deferrals"),
        fill = c(col_female,col_female_deferral,col_male,col_male_deferral))
legend("topright",
        legend=c("African-American non deferrals","African-American deferrals",
                      "Caucasian non deferrals","Caucasian deferrals"),
        density=c(0,20,0,20), fill=c("white","white","black","black"))
abline(h=0,lwd=2)
dev.off()
