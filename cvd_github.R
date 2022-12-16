

#This script covers data visualization code for Figures 2B, and Figures 3A, 3B, 3C. It also covers code to compute input data for Figure 2C, Figure 2D, and Figure 3D. Other figures (Figure 1A, 1B, 1C, 1D and Figure 2A) were done in Excel. 


library(tidyverse)

library(ggplot2)

library(FactoMineR)

df <- read.table("file.txt", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE)

str(df)

#'data.frame':   453 obs. of  38 variables:
# $ id                 : chr  "1" "2" "3" "4" ...
# $ evaluation         : chr  "psych" "med" "med" "psych" ...
# $ cvd                : int  1 1 0 0 1 0 1 0 0 1 ...
# $ cvd.symptom        : int  7 1 0 0 2 0 2 0 0 2 ...
# $ stress             : int  1 1 1 1 1 1 1 1 1 1 ...
# $ stress.symptom     : int  16 3 6 11 7 4 6 9 10 4 ...
# $ id.2               : chr  "1" "2" "3" "4" ...
# $ birth.year         : int  1951 1997 1955 1986 1983 NA 1954 1984 1989 NA ...
# $ eval.year          : int  2010 2010 2010 2010 2010 2010 2010 2010 2011 2011 ...
# $ age.at.eval        : int  59 13 55 24 27 NA 56 26 22 NA ...
# $ sex                : chr  "M" "M" "F" "F" ...
# $ home.country       : chr  "Pakistan" "Bangladesh" "Guinea" "Honduras" ...
# $ un.subregions      : chr  "central.southern.asia" "central.southern.asia" "sub.saharan.africa" "latin.america.caribbean" ...
# $ id.1               : chr  "1" "2" "3" "4" ...
# $ breath             : int  0 0 0 0 0 0 1 0 0 1 ...
# $ chest              : int  0 0 0 0 0 0 0 0 0 1 ...
# $ pain               : int  1 1 0 0 1 0 0 0 0 0 ...
# $ weakness           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ lightheadedness    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ palpitations       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ swelling           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ stroke             : int  1 0 0 0 0 0 0 0 0 0 ...
# $ hypertension       : int  1 0 0 0 0 0 0 0 0 0 ...
# $ diabetes           : int  1 0 0 0 0 0 0 0 0 0 ...
# $ cholesterol        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ irritability       : int  1 1 1 1 0 0 0 0 0 0 ...
# $ headache           : int  1 0 0 0 1 0 0 1 0 0 ...
# $ fatigue.stress     : int  1 0 0 1 0 0 1 1 1 0 ...
# $ tension            : int  0 0 0 0 0 0 0 0 0 0 ...
# $ appetite           : int  1 0 1 1 1 1 1 1 1 1 ...
# $ teeth              : int  0 0 0 0 0 0 0 0 0 0 ...
# $ libido             : int  0 0 0 1 0 0 0 0 1 0 ...
# $ restlessness       : int  1 1 1 1 1 1 1 1 1 1 ...
# $ stomach.ache       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ depressed          : int  1 0 1 1 1 1 1 1 1 1 ...
# $ sleep              : int  1 1 1 1 1 1 1 1 1 1 ...
# $ stand.cvd.points   : int  4 1 0 0 1 0 1 0 0 2 ...
# $ stand.stress.points: int  7 3 5 7 5 4 5 6 6 4 ...




data_b <- df[,c(2,3,5,8:9,11,13,15:38)]

data_c <- data_b %>% select(cvd, stress, evaluation)

data_d <- data.frame(cbind(data_c, data_b[,4:29]))


data_d$cvd <- as.factor(data_d$cvd)

data_d$stress <- as.factor(data_d$stress)



#fit a linear regression model to analyze the relationship between CVD status (factor:(1,0)) with all other variables:demographic variables (e.g., age, UN.subregion) and technical variables (e.g., evaluation type, evaluation year), stress status, and all symptoms. 

lm <- lm(cvd ~ . , data = data_d)

summary(lm)









##data wrangling to prepare input for Figure 2B



df$cvd <- as.factor(df$cvd)

df$stress <- as.factor(df$stress)




data_spread <- df %>% spread(key=cvd, value=stand.cvd.points) 

names(data_spread) <- c("id", "evaluation", "cvd.symptom","stress", "stress.symptom", "id.2", "birth.year", "eval.year", "age.at.eval", "sex", "home.country", "un.subregions", "id.1", "breath", "chest", "pain", "weakness", "lightheadedness", "palpitations", "swelling", "stroke", "hypertension", "diabetes", "cholesterol", "irritability", "headache", "fatigue.stress", "tension", "appetite", "teeth", "libido", "restlessness", "stomach.ache", "depressed", "sleep", "stand.stress.points","no_cvd", "cvd")

data_spread_2 <- data_spread %>% spread(key=stress, value=stand.stress.points)

names(data_spread_2) <- c("id", "evaluation", "cvd.symptom", "stress.symptom", "id.2", "birth.year", "eval.year", "age.at.eval", "sex", "home.country", "un.subregions", "id.1", "breath", "chest", "pain", "weakness", "lightheadedness", "palpitations", "swelling", "stroke", "hypertension", "diabetes", "cholesterol", "irritability", "headache", "fatigue.stress", "tension", "appetite", "teeth", "libido", "restlessness", "stomach.ache", "depressed", "sleep", "no_cvd", "cvd","no_stress", "stress")




#Data wrangling continued: group by cvd and stress

data_no_both <- data_spread_2 %>% filter(no_cvd == 0 & no_stress == 0)

data_yes_both <- data_spread_2 %>% filter(cvd > 0 & stress > 0)

data_yes_cvd_no_stress <- data_spread_2 %>% filter(cvd >0 & no_stress == 0)

data_no_cvd_yes_stress <- data_spread_2 %>% filter(no_cvd == 0 & stress > 0)





data_gather_no_both <- data_no_both %>% gather(key="condition", value ="symptoms", 35:38)

data_gather_yes_both <- data_yes_both %>% gather(key="condition", value ="symptoms", 35:38)

data_gather_yes_cvd_no_stress <- data_yes_cvd_no_stress %>% gather(key="condition", value ="symptoms", 35:38)

data_gather_no_cvd_yes_stress <- data_no_cvd_yes_stress %>% gather(key="condition", value ="symptoms", 35:38)





#rename the strings in the 'condition' column to reflect the different groups that we want to plot

data_gather_yes_both_updated <- data_gather_yes_both %>% mutate(across('condition' , str_replace, 'cvd', 'cvd_in_CVD+')) %>% mutate(across('condition', str_replace, 'stress', 'stress_both'))


data_gather_no_cvd_yes_stress_updated <- data_gather_no_cvd_yes_stress %>% mutate(across('condition', str_replace, 'stress', 'stress_in_CVD-'))


#combine both data frames

updated <- rbind(data_gather_yes_both_updated, data_gather_no_cvd_yes_stress_updated)


#Figure 2B density plot
ggplot(updated, aes(symptoms, fill = condition), binwidth = 0.5) + geom_density(alpha = 0.2) 















##get symptom frequencies for Figure 2C



#get symptom frequencies for cvd and non-cvd separately


#for CVD+

cvd <- data_d %>% filter(cvd == 1)


frequencies_cvd <- apply(cvd[,8:29], 2, table)


freq_df_cvd <- data.frame(frequencies_cvd)


write.table(freq_df_cvd, 'frequencies_cvd.txt', quote=FALSE, col.names=TRUE, sep='\t', row.names=TRUE)







#for CVD-

noncvd <- data_d %>% filter(cvd == 0)


frequencies_noncvd <- apply(noncvd[,8:29], 2, table)


freq_df_noncvd <- data.frame(frequencies_noncvd)


write.table(freq_df_noncvd, 'frequencies_noncvd.txt', quote=FALSE, col.names=TRUE, sep='\t', row.names=TRUE)




## Odds ratio and fisher's exact test for Figure 2D

fisher.test(matrix(c(83,48,132,190),nrow=2))
fisher.test(matrix(c(91,13,124,225),nrow=2))
fisher.test(matrix(c(170,136,45,102),nrow=2))
fisher.test(matrix(c(20,10,195,228),nrow=2))
fisher.test(matrix(c(106,77,109,161),nrow=2))
fisher.test(matrix(c(14,9,201,229),nrow=2))
fisher.test(matrix(c(194,185,21,53),nrow=2))
fisher.test(matrix(c(27,4,188,234),nrow=2))
fisher.test(matrix(c(183,182,32,56),nrow=2))
fisher.test(matrix(c(191,159,24,79),nrow=2))











##Figures 3A, B, C, and D



data_subset <- df[,c(2,3,5,8:9,11,13,15:38)]

data_subset_b <- data_subset %>% select(cvd, stress, evaluation)

data_subset_c <- data.frame(cbind(data_subset_b, data_subset[,4:29]))

data_subset_c <- data_subset_c %>% drop_na()


data_subset_c$cvd <- as.factor(data_subset_c$cvd)

data_subset_c$stress <- as.factor(data_subset_c$stress)



data_subset_d <- data_subset_c[,c(1, 3:29)]

data_subset_d_updated <- data_subset_d %>% mutate(across('sex' , str_replace, 'MTF', '3')) %>% mutate(across('sex', str_replace, 'F', '2')) %>% mutate(across('sex', str_replace, 'M', '1')) %>% mutate(across('un.subregions', str_replace, 'central.southern.asia', '1')) %>% mutate(across('un.subregions', str_replace, 'sub.saharan.africa', '2')) %>% mutate(across('un.subregions', str_replace, 'latin.america.caribbean', '3')) %>% mutate(across('un.subregions', str_replace, 'europe.north.america', '4')) %>% mutate(across('un.subregions', str_replace, 'eastern.southeastern.asia', '5')) %>% mutate(across('un.subregions', str_replace, 'north.africa.western.asia', '6')) %>% mutate(across('evaluation', str_replace, 'psych_gyn', '5')) %>% mutate(across('evaluation', str_replace, 'psych_med', '4')) %>% mutate(across('evaluation', str_replace, 'gyn', '3')) %>% mutate(across('evaluation', str_replace, 'psych', '1')) %>% mutate(across('evaluation', str_replace, 'med', '2')) 



data_subset_d_updated$sex <- as.integer(data_subset_d_updated$sex)

data_subset_d_updated$evaluation <- as.integer(data_subset_d_updated$evaluation)

data_subset_d_updated$un.subregions <- as.integer(data_subset_d_updated$un.subregions)







#keep only the symptoms list

str(data_subset_d_updated)



data_subset_e <- data_subset_d_updated[,c(1, 5, 7:28)]

data_subset_f <- data_subset_e[,c(1, 3:24)]





#get sum of CVD and sum of stress symptoms

data_subset_f $cvd.symptoms <- rowSums(data_subset_f[,2:12])

data_subset_f $stress.symptoms <- rowSums(data_subset_f[,13:23])


data_subset_f_updated <- data_subset_f[,c(1,24,25)]


#Figure 3A

pca <- PCA(data_subset_f_updated, quali.sup = 1, graph = FALSE)

plotellipses(pca,1, label='none')






#Figure 3B

pca <- PCA(data_subset_f, quali.sup = 1, graph = FALSE)

plotellipses(pca,1, label='none')



#Figure 3C
pca <- PCA(data_subset_f, quali.sup = 1, graph = TRUE)




#getting PCA data for Figures 3B and 3C

correlations <- data.frame(pca$var$cor)


write.table(correlations,'corelations',quote=FALSE, col.names=TRUE, row.names=TRUE, sep='\t')





#input for Figure 3D

contributions <- data.frame(pca$var$contrib)


write.table(contributions,'contributions',quote=FALSE, col.names=TRUE, row.names=TRUE, sep='\t')














