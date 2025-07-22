# Load packages -----------------------------------------------------------
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("lme4")
# install.packages("cowplot")
# install.packages("ggpubr")

library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(lme4)
library(cowplot)
library(rstan)
library(ggpubr)

rm(list=ls())
theme_set(theme_bw())
setwd("C:/Users/LSP005/Nextcloud/Professionnel/Projets/2023 - Gender-neutral/2025 - Gender-neutral 2/github")

# Load Experiment 2 Data --------------------------------------------------

filename_exp2 = "results_exp_clean_rej.xlsx"#"results_exp_clean_rej.xlsx"

load_exp2_sheet <- function(sheet, condition_label, list_label, subject_prefix){
  df <- read_excel(filename_exp2, sheet = sheet, na = "---", col_names = TRUE, skip = 2)
  df <- df[,c(-1:-3,-6:-7)]
  names(df) <- c("item", "gender", names(df)[-1:-2])
  df[df == "e"] <- NA
  df[df == "cut"] <- NA
  df[,-1:-2] <- sapply(df[,-1:-2], as.numeric)
  df <- melt(df, c("item", "gender"), value.name = "RT")
  names(df)[3] <- "subject"
  df$condition <- condition_label
  df$list <- list_label
  levels(df$subject) <- paste(subject_prefix, levels(df$subject))
  return(df)
}

SL1 <- load_exp2_sheet(1, "MASCULIN", "L1", "L1")
SL2 <- load_exp2_sheet(2, "MASCULIN", "L2", "L2")
IL1 <- load_exp2_sheet(3, "FEMININ", "L1", "L1")
IL2 <- load_exp2_sheet(4, "FEMININ", "L2", "L2")

# gender -> gender of continuation
# condition -> M/F generic
data_exp2 <- rbind(SL1, SL2, IL1, IL2)
data_exp2$gender <- factor(data_exp2$gender)
data_exp2$condition <- factor(data_exp2$condition)
data_exp2$list <- factor(data_exp2$list)
data_exp2$iscorrect <- !is.na(data_exp2$RT)

# exclude some items
data_exp2 <- data_exp2 %>% 
 filter(item!=32 & item!=25 & item!=43 & item!=44 & item!=9 & item!=17 & item!=22)

agg_data_exp2 <- data_exp2 %>%
  #filter(condition=='MASCULIN') %>%
  group_by(subject, condition, gender) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE),
            mean_PC = mean(iscorrect, na.rm = TRUE),
            .groups = 'drop')
agg_data_exp2$mean_RT_z <- scale(agg_data_exp2$mean_RT)

# Frequentist Statistics --------------------------------------------------

anova_exp2 <- aov(mean_RT ~ condition * gender + Error(subject/gender), data = agg_data_exp2)
summary(anova_exp2)

mixed_model_exp2 <- lmer(mean_RT ~ condition * gender + (1 | subject), data = agg_data_exp2)
summary(mixed_model_exp2)

# Prepare data for STAN ---------------------------------------------------

# agg_data_exp2_L: mean RT data in list format
agg_data_exp2_L <- list( subject = as.numeric(agg_data_exp2$subject),
                         gender = as.numeric(agg_data_exp2$gender=="M"),
                         condition = as.numeric(agg_data_exp2$condition=="MASCULIN"),
                         mean_RT_z = (agg_data_exp2$mean_RT-mean(agg_data_exp2$mean_RT))/sd(agg_data_exp2$mean_RT),
                         mean_RT = agg_data_exp2$mean_RT,
                         mean_PC = agg_data_exp2$mean_PC,
                         N = length(agg_data_exp2$mean_RT))
agg_data_exp2_L$Nsubject = max(agg_data_exp2_L$subject)

# aggpaired_data_exp2_L: mean RT data in list format with only the difference between gender
aggpaired_data_exp2_L = list();
Ndat = length(agg_data_exp2_L$subject)
aggpaired_data_exp2_L$subject = agg_data_exp2_L$subject[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$condition = agg_data_exp2_L$condition[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_RT_diff = diff(agg_data_exp2_L$mean_RT)[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_RT_diff_z = (aggpaired_data_exp2_L$mean_RT_diff-mean(aggpaired_data_exp2_L$mean_RT_diff))/sd(aggpaired_data_exp2_L$mean_RT_diff)
aggpaired_data_exp2_L$N = length(aggpaired_data_exp2_L$subject)
aggpaired_data_exp2_L$mean_RT_F = agg_data_exp2_L$mean_RT[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_RT_M = agg_data_exp2_L$mean_RT[seq(2, Ndat, 2)]
aggpaired_data_exp2_L$mean_PC_F = agg_data_exp2_L$mean_PC[seq(1, Ndat, 2)]
aggpaired_data_exp2_L$mean_PC_M = agg_data_exp2_L$mean_PC[seq(2, Ndat, 2)]

# agg_data_exp2_L: all RT data in list format
RTdata_exp2_L <- list( subject = as.numeric(data_exp2$subject),
                       item = as.numeric(data_exp2$item),
                       gender = as.numeric(data_exp2$gender=="F"),#gender = as.numeric(data_exp2$gender=="M"),
                       condition = as.numeric(data_exp2$condition=="FEMININ"),#condition = as.numeric(data_exp2$condition=="MASCULIN"),
                       iscorrect = data_exp2$iscorrect,
                       RT = data_exp2$RT,
                       RT_z = (data_exp2$RT-mean(data_exp2$RT, na.rm = TRUE))/sd(data_exp2$RT, na.rm = TRUE))
RTdata_exp2_L$Nsubject = max(RTdata_exp2_L$subject)
RTdata_exp2_L$Nitem = max(RTdata_exp2_L$item)
RTdata_exp2_L$N = length(RTdata_exp2_L$RT_z)
CRdata_exp2_L = RTdata_exp2_L

# remove errors
RTdata_exp2_L$subject = RTdata_exp2_L$subject[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$item = RTdata_exp2_L$item[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$gender = RTdata_exp2_L$gender[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$condition = RTdata_exp2_L$condition[!is.na(RTdata_exp2_L$RT_z)]
RTdata_exp2_L$RT_z = RTdata_exp2_L$RT_z[!is.na(RTdata_exp2_L$RT_z)]
#RTdata_exp2_L$RT = RTdata_exp2_L$RT[!is.na(RTdata_exp2_L$RT)]
#RTdata_exp2_L$RT_z = (RTdata_exp2_L$RT_z - mean(RTdata_exp2_L$RT_z))/sd(RTdata_exp2_L$RT_z)
RTdata_exp2_L$N = length(RTdata_exp2_L$RT_z)

# Plotting ----------------------------------------------------------------

pairedplot <- function(mean_RT_F,mean_RT_M,condition){
  if (min(condition)==max(condition))
  {xvalue = 1
  y = c(mean_RT_F,mean_RT_M)
  d <- data.frame(y=y, 
                  x = rep(c(-1,+1), each=length(mean_RT_F)),
                  id = factor(rep(1:length(mean_RT_F),2)))
  d$xj <- jitter(d$x, amount=.2)
  d$Gender = factor(d$x, labels=c("F","M"))
  pal = c("forestgreen",  "orange")
  ggplot(data=d, aes(y=y)) +
    geom_boxplot(aes(x=xvalue+x, group = Gender, color = Gender), width=0.2, position=position_dodge(1.1), outlier.shape = NA) +
    geom_line(aes(x=xvalue+0.5*xj, group = id), color = "gray" ,linewidth = 0.5)+
    geom_point(aes(x=xvalue+0.5*xj, color = Gender), size = 0.7) + scale_color_manual(values = pal) + scale_fill_manual(values = pal)  +
    theme_bw()}
  else
  {xvalue0 = 1
  xvalue1 = 5
  y = c(mean_RT_F[condition==0],mean_RT_M[condition==0])
  d0 <- data.frame(y=y, 
                   x = rep(c(-1,+1), each=length(mean_RT_F[condition==0])),
                   id = factor(rep(1:length(mean_RT_F[condition==0]),2)))
  d0$xj <- jitter(d0$x, amount=.2)
  d0$Gender = factor(d0$x, labels=c("F","M"))
  
  y = c(mean_RT_F[condition==1],mean_RT_M[condition==1])
  d1 <- data.frame(y=y, 
                   x = rep(c(-1,+1), each=length(mean_RT_F[condition==1])),
                   id = factor(rep(1:length(mean_RT_F[condition==1]),2)))
  d1$xj <- jitter(d1$x, amount=.2)
  d1$Gender = factor(d1$x, labels=c("F","M"))
  
  pal = c("forestgreen",  "orange")
  ggplot(data=d0, aes(y=y)) +
    geom_boxplot(aes(x=xvalue0+x, group=Gender, color = Gender), width=0.2, position=position_dodge(1.1), outlier.shape = NA) +
    geom_line(aes(x=xvalue0+0.5*xj, group=id), color = "gray",linewidth = 0.5)+
    geom_point(aes(x=xvalue0+0.5*xj, color = Gender), size = 0.7) + scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
    geom_boxplot(data=d1, aes(x=xvalue1+x, group=Gender, color = Gender), width=0.2, position=position_dodge(1.1), outlier.shape = NA) +
    geom_line(data=d1, aes(x=xvalue1+0.5*xj, group=id), color = "gray",linewidth = 0.5)+
    geom_point(data=d1, aes(x=xvalue1+0.5*xj, color = Gender), size = 0.7) + scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
    theme_bw()}
}


# Plotting ---------------------------------------------------------------

plot2 = pairedplot(aggpaired_data_exp2_L$mean_RT_F,aggpaired_data_exp2_L$mean_RT_M,aggpaired_data_exp2_L$condition)
plot2 <- plot2 + ylim(500,4000) + theme(legend.position="none") + ylab("Reaction Time (ms)")
plot2 <- plot2 + xlab("") + scale_x_continuous(breaks=c(1,5), labels=c("Feminine hybrid nouns","Masculine hybrid nouns"))

plot4 = pairedplot(100*aggpaired_data_exp2_L$mean_PC_F,100*aggpaired_data_exp2_L$mean_PC_M,aggpaired_data_exp2_L$condition)
plot4 <- plot4 + ylim(0,100) + theme(legend.position="right") + ylab("Percent correct (%)")
plot4 <- plot4 + xlab("Condition") + scale_x_continuous(breaks=c(1,5), labels=c("Feminine hybrid nouns","Masculine hybrid nouns"))

plot_grid(plot2, plot4, nrow=2, axis = "r", align = "v")

# Some results -----------------------------------------------------------

# feminine hybrid nouns, feminine continuation
mean(data_exp2$RT[data_exp2$condition=="FEMININ" & data_exp2$gender=="F"],na.rm = TRUE)
sd(data_exp2$RT[data_exp2$condition=="FEMININ" & data_exp2$gender=="F"],na.rm = TRUE)
mean(data_exp2$iscorrect[data_exp2$condition=="FEMININ" & data_exp2$gender=="F"],na.rm = TRUE)

# feminine hybrid nouns, masculine continuation
mean(data_exp2$RT[data_exp2$condition=="FEMININ" & data_exp2$gender=="M"],na.rm = TRUE)
sd(data_exp2$RT[data_exp2$condition=="FEMININ" & data_exp2$gender=="M"],na.rm = TRUE)
mean(data_exp2$iscorrect[data_exp2$condition=="FEMININ" & data_exp2$gender=="M"],na.rm = TRUE)

# masculine hybrid nouns, feminine continuation
mean(data_exp2$RT[data_exp2$condition=="MASCULIN" & data_exp2$gender=="F"],na.rm = TRUE)
sd(data_exp2$RT[data_exp2$condition=="MASCULIN" & data_exp2$gender=="F"],na.rm = TRUE)
mean(data_exp2$iscorrect[data_exp2$condition=="MASCULIN" & data_exp2$gender=="F"],na.rm = TRUE)

# masculine hybrid nouns, masculine continuation
mean(data_exp2$RT[data_exp2$condition=="MASCULIN" & data_exp2$gender=="M"],na.rm = TRUE)
sd(data_exp2$RT[data_exp2$condition=="MASCULIN" & data_exp2$gender=="M"],na.rm = TRUE)
mean(data_exp2$iscorrect[data_exp2$condition=="MASCULIN" & data_exp2$gender=="M"],na.rm = TRUE)

# masculine hybrid nouns, feminine-masculine continuation
mean(data_exp2$RT[data_exp2$condition=="MASCULIN" & data_exp2$gender=="F"],na.rm = TRUE) - mean(data_exp2$RT[data_exp2$condition=="MASCULIN" & data_exp2$gender=="M"],na.rm = TRUE)

# # hierarchical model with random effect of subject and item on intercept (applied on RT) ------------------------------------------

m2.2h <- stan_model(file = 'm2.2h.stan')
lm2.2h_exp2 <- sampling(m2.2h,
                        data = RTdata_exp2_L,
                        chains = 7,             # number of Markov chains
                        warmup = 3000,          # number of warmup iterations per chain
                        iter = 7000,            # total number of iterations per chain
                        refresh = 1000)
parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"sigma"
#parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition","sigma","gammaz_0i","gammaz_0s")
parameter_names = rev(c(expression(beta[0]),expression(beta[gender]),expression(beta[condition]),expression(beta[gender%*%condition])))#,"sigma"
print(lm2.2h_exp2, pars = parameters)
#plot(lm2.2h_exp2, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m2.2h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot7 <- stan_plot(lm2.2h_exp2, pars = parameters, show_density = TRUE, show_outer_line = TRUE, ci_level= 0.95, outer_level= 0.99, fill_color='cornflowerblue', outline_color='black', est_color='darkblue') + 
  #ggtitle("reaction-time model") + 
  ggtitle("") + 
  xlim(-1,1) + xlab("weights") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot7 = plot7 + scale_y_continuous(breaks=1:length(parameters),labels=(parameter_names))#c("beta_0","beta_gender","beta_condition","beta_gendercondition",""))#
plot7

m3.2h <- stan_model(file = 'm3.2h.stan')
fit.m3.2h <- sampling(m3.2h,
                      data = CRdata_exp2_L,
                      chains = 7,             # number of Markov chains
                      warmup = 3000,          # number of warmup iterations per chain
                      iter = 7000,            # total number of iterations per chain
                      refresh = 1000)
parameters = c("beta_0","beta_gender","beta_condition","beta_gendercondition")#,"plapse","sigma"
parameter_names = rev(c(" ",expression(beta[0]),expression(beta[gender]),expression(beta[condition]),expression(beta[gender%*%condition])))#,"sigma"
print(fit.m3.2h, pars = parameters)
#plot(fit.m3.2h, show_density = TRUE, show_outer_line = FALSE, ci_level= 0.95,outer_level= 0.99, pars = parameters) + ggtitle("m3.2h") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot8 <- stan_plot(fit.m3.2h, pars = parameters, show_density = TRUE, show_outer_line = TRUE, ci_level= 0.95, outer_level= 0.99, fill_color='cornflowerblue', outline_color='black', est_color='darkblue') + 
  #ggtitle("percent-correct model") + #ggtitle("percent-correct model") + 
  #ggtitle("") + 
  xlim(-4,4) + xlab("weights") #+ coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))#+xlim(-3,1)
plot8 = plot8 + scale_y_continuous(labels=(parameter_names))#c("beta_0","beta_gender","beta_condition","beta_gendercondition",""))#

plot_grid(plot7,plot8, ncol=2, labels=c("A. Reaction-time model","B. Percent-correct model"), axis = "r", align = "hv")
