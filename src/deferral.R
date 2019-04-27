
library(grid)
library(gridExtra) 
library(dplyr)
library(ggplot2)



# hyperparameter for plots
cex_factor = 1.6

# Load and clean the data selecting for the desired fields
raw_data <- read.csv("./compas-scores-two-years.csv")
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
                    filter(days_b_screening_arrest <= 30) %>%
                    filter(days_b_screening_arrest >= -30) %>%
                    filter(is_recid != -1) %>%
                    filter(c_charge_degree != "O") %>%
                    filter(score_text != 'N/A')

df_females = filter(df,sex=="F" | sex=="Female")
df_males = filter(df,sex=="Male" | sex=="M")
num_females = nrow(df_females)
num_males = nrow(df_males)

# Compute frequency distributions for COMPAS scores for both the groups
female_score_freq = 1:10
male_score_freq = 1:10
female_score_pdf = 1:10
male_score_pdf = 1:10
for (i in seq(1,10,by=1)){
  female_score_freq[i] = nrow(filter(df,sex=="F" | sex=="Female", decile_score==i))
  female_score_pdf[i] = female_score_freq[i]/num_females
  male_score_freq[i] = nrow(filter(df,sex=="Male" | sex=="M", decile_score==i))
  male_score_pdf[i] = male_score_freq[i]/num_males
}


# Defer on African-Americans so as to convert their AP into that of Caucasians
ratio_pdfs = female_score_pdf/male_score_pdf
delta = 1 - min(ratio_pdfs)
q_females = 1 - (1-delta)/ratio_pdfs
female_deferrals = female_score_freq*q_females
female_deferrals_frac = female_score_pdf*q_females
female_nondeferrals_frac = female_score_pdf*(1-q_females)

# Defer on Caucasians so as to convert their AP into that of African-Americans
ratio_pdfs = male_score_pdf/female_score_pdf
delta = 1 - min(ratio_pdfs)
q_males = 1 - (1-delta)/ratio_pdfs
male_deferrals = male_score_freq*q_males
male_deferrals_frac = male_score_pdf*q_males
male_nondeferrals_frac = male_score_pdf*(1-q_males)

# Pick colors to be used in the barplots
col_female = "gray38"
col_female_deferral = "gray70"
col_male = "orange2"
col_male_deferral = "moccasin"

# Generate barplot to display the deferrals of African-Americans to convert
# their AP into that of Caucasians
dat1 = rbind(female_nondeferrals_frac,female_deferrals_frac)
png('female_to_male_equalize.png')
barplot(dat1, beside=F,main=paste("Converting female AP \ninto male AP.",
                                    "Deferral rate = 38%"),xlab="Decile Score", ylab="",
        ylim=c(0,0.15), names.arg=1:10, space=c(0.5,0.5),
        col=c(col_female,col_female_deferral),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
barplot(dat1,space=c(0.5,0.5),add=T,angle=c(0,45), density=c(0,20),
        cex.axis= cex_factor)
title(ylab="Probability Density", line=2.85, cex.lab=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        fill = c(col_female,col_female_deferral), cex=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        density = c(0,20), fill=c("white","white"),cex=cex_factor)
abline(h=0)
dev.off()

# Printing some statistics
total_female_deferrals = Reduce(f="+",x = female_deferrals,accumulate=F)
cat("Total females", num_females,"\n")
cat("Total female deferrals: ",total_female_deferrals,"\n")
cat("Fraction of female deferrals: ", total_female_deferrals/num_females,"\n")


# Generate barplot to display the deferrals of Caucasians to convert
# their AP into that of African-Americans
dat2 = rbind(male_nondeferrals_frac,male_deferrals_frac)
png('male_to_female_equalize.png')
barplot(dat2, beside=F,main=paste("Converting male AP into \nfemale AP.",
                                    "Deferral rate = 15%"),xlab="Decile Score",
        ylab="", ylim=c(0,0.3), names.arg=1:10, space=c(0.5,0.5),
        col=c(col_male,col_male_deferral),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
barplot(dat2,space=c(0.5,0.5),add=T,angle=c(0,45), density=c(0,20), col=c("male","black"),
        cex.axis=cex_factor)
title(ylab="Probability Density", line=2.85, cex.lab=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        fill = c(col_male,col_male_deferral), cex=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        density = c(0,20),fill=c("male","black"), cex=cex_factor)
abline(h=0)
dev.off()

# Printing some statistics
total_male_deferrals = Reduce(f="+",x = male_deferrals,accumulate=F)
cat("Total males", num_males,"\n")
cat("Total male deferrals: ",total_male_deferrals,"\n")
cat("Fraction of male deferrals: ",total_male_deferrals/num_males,"\n")
