## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------
require(dplyr)
require(skimr)
require(haven)
Data_online<-read.csv("Study1-Online.csv")
Data_field<-read.csv("Study1-Field.csv")


## -----------------------------------------------------------------------------------------------------------------------------
Data_online <- Data_online %>% filter(!is.na(Q54))
Data_field <- Data_field %>% filter(!is.na(Q54))


## -----------------------------------------------------------------------------------------------------------------------------
Data_online<-Data_online %>% filter(Q6==1)
Data_field <- Data_field %>% filter(Q6==1)


## -----------------------------------------------------------------------------------------------------------------------------
Age<-c(Data_online$Q5,Data_field$Q5)
mean(Age)
sd(Age)
min(Age)
max(Age)


## -----------------------------------------------------------------------------------------------------------------------------
Gender<-c(Data_online$Q4,Data_field$Q4)
table(Gender)
prop.table(table(Gender))


## -----------------------------------------------------------------------------------------------------------------------------
Student<-c(Data_online$Q8,Data_field$Q8)
table(Student)
prop.table(table(Student))
Romantic<-c(Data_online$Q7,Data_field$Q7)
table(Romantic)
prop.table(table(Romantic))


## -----------------------------------------------------------------------------------------------------------------------------
Data_analysis1<- Data_online %>% select(Q4,Q23:Q37,Q39,Q40,Q41,Q42,Q43,Q44,FL_21_DO_Scenario5_attractiveandnotdominantfemale,FL_21_DO_Scenario7_unattractiveanddominantfemale,FL_21_DO_Scenario6_attractiveanddominantfemale,FL_21_DO_Scenario8_unattractiveandnonedominantfemale,FL_17_DO_Scenario_attractiveandnon_dominantmale,FL_17_DO_Scenario3_dominantandunattractivemale,FL_17_DO_Scenario1_attractiveanddominantmale,FL_17_DO_Scenario4_NotDominantandUnattractivemale, Q93:Q98)
names(Data_analysis1)<-c("Gender","Suspicious","Betrayed","Worried","Distrustful","Jealousy","Rejected","Hurt","Anxious","Angry", "Threatened","Sad","Upset","Attractiveness_check1","Attractiveness_check2","Assertiveness","Self_confidence","Extroverted","Influential","Socially_competent","Good_judge_of_character","Attractive_not_dominant_woman","Unattractive_dominant_woman","Attractive_dominant_woman","Unattractive_not_dominant_woman","Attractive_not_dominant_man","Unattractive_dominant_man","Attractive_dominant_man","Unattractive_not_dominant_man","Mate_value1","Mate_value2", "Mate_value3","Mate_value4", "Mate_value5", "Mate_value6")
Data_analysis1$Sample<-rep("Online",nrow(Data_analysis1))
# For some reason qualtrics exports.The label here but not elsewhere Q34.
Data_analysis2<- Data_field %>% select(Q4,Q23:Q34, Q36:Q37,Q39,Q40,Q41,Q42,Q43,Q44,Q74,Q77,Q83,Q80,Q68,Q71,Q89,Q86, Q93:Q98)
names(Data_analysis2)<-c("Gender","Suspicious","Betrayed","Worried","Distrustful","Jealousy","Rejected","Hurt","Anxious","Angry", "Threatened","Sad","Upset","Attractiveness_check1","Attractiveness_check2","Assertiveness","Self_confidence","Extroverted","Influential","Socially_competent","Good_judge_of_character","Attractive_not_dominant_woman","Unattractive_dominant_woman","Attractive_dominant_woman","Unattractive_not_dominant_woman","Attractive_not_dominant_man","Unattractive_dominant_man","Attractive_dominant_man","Unattractive_not_dominant_man", "Mate_value1","Mate_value2", "Mate_value3","Mate_value4", "Mate_value5", "Mate_value6")
Data_analysis2$Sample<-rep("Field",nrow(Data_analysis2))
Data_analysis<-rbind(Data_analysis1,Data_analysis2)



## -----------------------------------------------------------------------------------------------------------------------------
Data_analysis<-Data_analysis %>% mutate(Attractive_condition=case_when(Attractive_not_dominant_woman == 1 | Attractive_dominant_woman == 1 | Attractive_not_dominant_man == 1 | Attractive_dominant_man == 1 ~ "Attractive", TRUE ~ "Not Attractive"), Dominant_condition=case_when(Attractive_dominant_woman == 1 | Unattractive_dominant_woman == 1 | Attractive_dominant_man == 1 | Unattractive_dominant_man == 1 ~ "Dominant", TRUE ~ "Not Dominant"), ID=1:nrow(Data_analysis))
write.csv(Data_analysis, "Dataset1.csv", row.names = F)
write_sav(Data_analysis, "Dataset1.sav")


## -----------------------------------------------------------------------------------------------------------------------------
require(psych)
alpha<- Data_analysis %>% dplyr::select(Mate_value1:Mate_value6)
require(psych)
psych::alpha(as.data.frame(alpha))


## -----------------------------------------------------------------------------------------------------------------------------
Data_analysis<-Data_analysis %>% mutate(Mate_value= Mate_value1+Mate_value2+Mate_value3+Mate_value4+Mate_value5+Mate_value6)


## -----------------------------------------------------------------------------------------------------------------------------
t.test(Mate_value~Gender, data=Data_analysis)


## -----------------------------------------------------------------------------------------------------------------------------
require(tidyr)
summary_mate_value <- Data_analysis %>% # "Start with the data set
  group_by(Gender) %>% # Then group by IV
  summarize(N = length(na.omit(Mate_value)),
            Mean_mate_value = mean(Mate_value, na.rm=T),
            SD_mate_value = sd(Mate_value,na.rm = T),
            SE_mate_value= SD_mate_value/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_mate_value



## -----------------------------------------------------------------------------------------------------------------------------
t.test(Mate_value~Attractive_condition, data=Data_analysis)
t.test(Mate_value~Dominant_condition, data=Data_analysis)


## -----------------------------------------------------------------------------------------------------------------------------
require(dplyr)
Data_analysis_men<- Data_analysis %>% filter(Gender==1)
Data_analysis_women<- Data_analysis %>% filter(Gender==2)



## -----------------------------------------------------------------------------------------------------------------------------
require(ez)
require(papaja)
Ez_ANOVA1<-ezANOVA(Data_analysis_men, dv=Attractiveness_check1, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA1
Ez_ANOVA2<-ezANOVA(Data_analysis_women, dv=Attractiveness_check1, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA2
Ez_ANOVA3<-ezANOVA(Data_analysis_men, dv=Attractiveness_check2, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA3
Ez_ANOVA4<-ezANOVA(Data_analysis_women, dv=Attractiveness_check2, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA4


## -----------------------------------------------------------------------------------------------------------------------------
require(apa)
anova_apa(Ez_ANOVA1, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA2, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA3, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA4, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
summary_attractive_men <- Data_analysis_men  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Attractiveness_check1), # Then summarize each group, these are complete so we can use same N
            Mean_att1 = mean(Attractiveness_check1),
            SD_att1 = sd(Attractiveness_check1),
            SE_att1 = SD_att1/sqrt(N), # Then summarize each group, these are complete so we can use same N
            Mean_att2 = mean(Attractiveness_check2),
            SD_att2 = sd(Attractiveness_check2),
            SE_att2 = SD_att2/sqrt(N)) 
summary_attractive_men

summary_attractive_women <- Data_analysis_women  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Attractiveness_check1), # Then summarize each group, these are complete so we can use same N
            Mean_att1 = mean(Attractiveness_check1),
            SD_att1 = sd(Attractiveness_check1),
            SE_att1 = SD_att1/sqrt(N), # Then summarize each group, these are complete so we can use same N
            Mean_att2 = mean(Attractiveness_check2),
            SD_att2 = sd(Attractiveness_check2),
            SE_att2 = SD_att2/sqrt(N)) 
summary_attractive_women



## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_men <- manova(cbind(Assertiveness, Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition, data = Data_analysis_men)
summary(Manovamodel_men)
summary(Manovamodel_men, test="Roy")
summary(Manovamodel_men, test="Wilks")
summary(Manovamodel_men, test="Hotelling-Lawley")
summary.aov(Manovamodel_men)


## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_men_attr <- manova(cbind(Assertiveness, Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition*Attractive_condition, data = Data_analysis_men)
summary(Manovamodel_men_attr)
summary(Manovamodel_men_attr, test="Roy")
summary(Manovamodel_men_attr, test="Wilks")
summary(Manovamodel_men_attr, test="Hotelling-Lawley")
summary.aov(Manovamodel_men_attr)
Manovamodel_men_attr$coefficients # check signs.


## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_women <- manova(cbind(Assertiveness, Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition, data = Data_analysis_women)
summary(Manovamodel_women)
summary(Manovamodel_women, test="Roy")
summary(Manovamodel_women, test="Wilks")
summary(Manovamodel_women, test="Hotelling-Lawley")
summary.aov(Manovamodel_women)


## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_women_attr <- manova(cbind(Assertiveness, Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition*Attractive_condition, data = Data_analysis_women)
summary(Manovamodel_women_attr)
summary(Manovamodel_women_attr, test="Roy")
summary(Manovamodel_women_attr, test="Wilks")
summary(Manovamodel_women_attr, test="Hotelling-Lawley")
summary.aov(Manovamodel_women_attr)
Manovamodel_women_attr$coefficients # check signs.


## -----------------------------------------------------------------------------------------------------------------------------
Ez_ANOVA_jealousy<-ezANOVA(Data_analysis, dv=Jealousy, wid=ID, between=.(Attractive_condition,Dominant_condition,Gender), detailed=TRUE, type=3)
Ez_ANOVA_jealousy
anova_apa(Ez_ANOVA_jealousy, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
require(dplyr)
Data_analysis_covar<- Data_analysis %>% filter(!is.na(Mate_value))
Ez_ANOVA_jealousy_covar<-ezANOVA(Data_analysis_covar, dv=Jealousy, wid=ID, between=.(Attractive_condition,Dominant_condition,Gender), between_covariates = Mate_value, detailed=TRUE, type=3)
Ez_ANOVA_jealousy_covar
anova_apa(Ez_ANOVA_jealousy_covar, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
Ez_ANOVA_jealousy_men<-ezANOVA(Data_analysis_men, dv=Jealousy, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type=3)
Ez_ANOVA_jealousy_men
Ez_ANOVA_jealousy_women<-ezANOVA(Data_analysis_women, dv=Jealousy, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type=3)
Ez_ANOVA_jealousy_women
anova_apa(Ez_ANOVA_jealousy_men, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA_jealousy_women, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
summary_jealousy_attractiveness_men <- Data_analysis_men  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_attractiveness_men

summary_jealousy_attractiveness_women <- Data_analysis_women  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_attractiveness_women


## -----------------------------------------------------------------------------------------------------------------------------
summary_jealousy_dominant_men <- Data_analysis_men  %>% # "Start with the data set
  group_by(Dominant_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_dominant_men

summary_jealousy_dominant_women <- Data_analysis_women  %>% # "Start with the data set
  group_by(Dominant_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_dominant_women


## -----------------------------------------------------------------------------------------------------------------------------
require(ggplot2)
require(ggthemes)
require(scales)
graph_data_men <- describeBy(Data_analysis_men$Jealousy,list(as.factor(Data_analysis_men$Attractive_condition),as.factor(Data_analysis_men$Dominant_condition)), mat=TRUE,digits=2)

graph_data_women <- describeBy(Data_analysis_women$Jealousy,list(as.factor(Data_analysis_women$Attractive_condition),as.factor(Data_analysis_women$Dominant_condition)), mat=TRUE,digits=2)

names(graph_data_men)[names(graph_data_men) == 'group1'] = 'Attractiveness'
names(graph_data_men)[names(graph_data_men) == 'group2'] = 'Dominance'  
names(graph_data_women)[names(graph_data_women) == 'group1'] = 'Attractiveness'
names(graph_data_women)[names(graph_data_women) == 'group2'] = 'Dominance'  

limits = aes(ymax = mean + sd, ymin=mean - sd)
dodge = position_dodge(width=0.9)
graph_men_bar<-ggplot(graph_data_men, aes(x = Attractiveness, y = mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_men_bar
# Reference set
reference_men_mean<-c(3.15,3,3.39,2.44)
reference_men_SD<-c(1.16,1.25,.85,.98)
Attractiveness<-c("Attractive","Attractive","Not Attractive", "Not Attractive")
Dominance<-c("Dominant","Not Dominant","Dominant", "Not Dominant")
reference_men<-cbind.data.frame(reference_men_mean,reference_men_SD,Attractiveness,Dominance)
limits = aes(ymax = reference_men_mean + reference_men_SD, ymin=reference_men_mean - reference_men_SD)
dodge = position_dodge(width=0.9)

graph_men_reference<-ggplot(reference_men, aes(x = Attractiveness, y = reference_men_mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+ 
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_men_reference
require(cowplot)
plot_grid(graph_men_bar, graph_men_reference, labels = c("A","B"), hjust = -8, label_size = 8,  ncol = 1, align = 'v')
ggsave("barplot_men.png")
ggsave("barplot_men.pdf")


## -----------------------------------------------------------------------------------------------------------------------------
require(ggplot2)
require(ggthemes)
require(scales)
graph_data_women <- describeBy(Data_analysis_women$Jealousy,list(as.factor(Data_analysis_women$Attractive_condition),as.factor(Data_analysis_women$Dominant_condition)), mat=TRUE,digits=2)

graph_data_women <- describeBy(Data_analysis_women$Jealousy,list(as.factor(Data_analysis_women$Attractive_condition),as.factor(Data_analysis_women$Dominant_condition)), mat=TRUE,digits=2)

names(graph_data_women)[names(graph_data_women) == 'group1'] = 'Attractiveness'
names(graph_data_women)[names(graph_data_women) == 'group2'] = 'Dominance'  
names(graph_data_women)[names(graph_data_women) == 'group1'] = 'Attractiveness'
names(graph_data_women)[names(graph_data_women) == 'group2'] = 'Dominance'  

limits = aes(ymax = mean + sd, ymin=mean - sd)
dodge = position_dodge(width=0.9)
graph_women_bar<-ggplot(graph_data_women, aes(x = Attractiveness, y = mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_women_bar
# Reference set
reference_women_mean<-c(3.62,3.48,2.47,2.83)
reference_women_SD<-c(1.12,1.12,1.07,1.34)
Attractiveness<-c("Attractive","Attractive","Not Attractive", "Not Attractive")
Dominance<-c("Dominant","Not Dominant","Dominant", "Not Dominant")
reference_women<-cbind.data.frame(reference_women_mean,reference_women_SD,Attractiveness,Dominance)
limits = aes(ymax = reference_women_mean + reference_women_SD, ymin=reference_women_mean - reference_women_SD)
dodge = position_dodge(width=0.9)

graph_women_reference<-ggplot(reference_women, aes(x = Attractiveness, y = reference_women_mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+ 
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_women_reference
require(cowplot)
plot_grid(graph_women_bar, graph_women_reference, labels = c("A","B"), hjust = -8, label_size = 8,  ncol = 1, align = 'v')
ggsave("barplot_women.png")
ggsave("barplot_women.pdf")


## -----------------------------------------------------------------------------------------------------------------------------

Data_analysis$Gender<-as.factor(Data_analysis$Gender)
levels(Data_analysis$Gender) <- c("Men", "Women")
dodge <- position_dodge(width = 0.7)
violinplot<-ggplot(Data_analysis, aes(x = Attractive_condition, y = Jealousy, fill=Dominant_condition)) + geom_violin(position=dodge) + geom_boxplot(position=dodge,width = 0.1, outlier.colour=NA) + xlab("Attractiveness condition")+ ylab("Jealousy") + labs(fill="Dominance condition") + theme(legend.position = "top") + facet_grid(~Gender) + scale_fill_grey(start=.4) + theme_tufte(12)
violinplot
ggsave("violinplot.png", width= 200, height=200, units="mm")


## -----------------------------------------------------------------------------------------------------------------------------
histogram_men<-ggplot(Data_analysis_men, aes(x=Jealousy)) 
histogram_men <- histogram_men + geom_histogram(binwidth=.5, colour="black", fill="white") + facet_grid(Attractive_condition~Dominant_condition) + theme_tufte(7.5)

histogram_women<-ggplot(Data_analysis_women, aes(x=Jealousy)) 
histogram_women <- histogram_women + geom_histogram(binwidth=.5, colour="black", fill="white") + facet_grid(Attractive_condition~Dominant_condition) + theme_tufte(7.5)

plot_grid(histogram_men, histogram_women, labels = c("Men (N=114)","Women (N=225)"), label_size = 7,  ncol = 1, align = 'v')
ggsave("histogram.png")
ggsave("histogram.pdf")


## -----------------------------------------------------------------------------------------------------------------------------
require(coin)
require(BayesFactor)
Data_analysis$Attractive_condition<-as.factor(Data_analysis$Attractive_condition)
Data_analysis$Dominant_condition<-as.factor(Data_analysis$Dominant_condition)
Data_analysis$Gender<-as.factor(Data_analysis$Gender)
bf_jealousy <- anovaBF(Jealousy ~ Attractive_condition*Dominant_condition*Gender, data=as.data.frame(Data_analysis))
bf_jealousy
1/bf_jealousy 
independence_test(Jealousy ~ Attractive_condition*Dominant_condition*Gender, data=Data_analysis)


## -----------------------------------------------------------------------------------------------------------------------------
require(coin)
require(BayesFactor)
Data_analysis_men$Attractive_condition<-as.factor(Data_analysis_men$Attractive_condition)
Data_analysis_men$Dominant_condition<-as.factor(Data_analysis_men$Dominant_condition)
bf_jealousy_men <- anovaBF(Jealousy ~ Attractive_condition*Dominant_condition, data=as.data.frame(Data_analysis_men))
bf_jealousy_men
1/bf_jealousy_men 
independence_test(Jealousy ~ Attractive_condition*Dominant_condition, data=Data_analysis_men)
Data_analysis_women$Attractive_condition<-as.factor(Data_analysis_women$Attractive_condition)
Data_analysis_women$Dominant_condition<-as.factor(Data_analysis_women$Dominant_condition)
bf_jealousy_women <- anovaBF(Jealousy ~ Attractive_condition*Dominant_condition, data=as.data.frame(Data_analysis_women))
bf_jealousy_women
1/bf_jealousy_women # 
independence_test(Jealousy ~ Attractive_condition*Dominant_condition, data=Data_analysis_women)


## -----------------------------------------------------------------------------------------------------------------------------
ezBoot1<-ezBoot(Data_analysis_men, dv=Jealousy, wid=ID, between=Attractive_condition*Dominant_condition, resample_within = F, iterations = 1e3)
ezBoot2<-ezBoot(Data_analysis_women, dv=Jealousy, wid=ID, between=Attractive_condition*Dominant_condition, resample_within = F, iterations = 1e3)


## -----------------------------------------------------------------------------------------------------------------------------
p<-ezPlot2(pred=ezBoot1, x= Attractive_condition,split=Dominant_condition)
print(p)
p2<-ezPlot2(pred=ezBoot2, x= Attractive_condition,split=Dominant_condition)
print(p2)


## -----------------------------------------------------------------------------------------------------------------------------
Ez_ANOVA_jealousy_men_check<-ezANOVA(Data_analysis_men, dv=Jealousy, wid=ID, between=.(Attractive_condition,Dominant_condition,Sample), detailed=TRUE, type=3)
Ez_ANOVA_jealousy_men_check
Ez_ANOVA_jealousy_women_check<-ezANOVA(Data_analysis_women, dv=Jealousy, wid=ID, between=.(Attractive_condition,Dominant_condition,Sample), detailed=TRUE, type=3)
Ez_ANOVA_jealousy_women_check
anova_apa(Ez_ANOVA_jealousy_men_check, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA_jealousy_women_check, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
require(dplyr)
require(skimr)
require(haven)
Data_online<-read.csv("Study2-Online.csv")
Data_field<-read.csv("Study2-Field.csv")


## -----------------------------------------------------------------------------------------------------------------------------
Data_online <- Data_online %>% filter(!is.na(Q98))
Data_field <- Data_field %>% filter(!is.na(Q98))


## -----------------------------------------------------------------------------------------------------------------------------
Data_online<-Data_online %>% filter(Q6==1)
Data_field <- Data_field %>% filter(Q6==1)


## -----------------------------------------------------------------------------------------------------------------------------
Age<-c(Data_online$Q5,Data_field$Q5)
mean(Age)
sd(Age)
min(Age)
max(Age)


## -----------------------------------------------------------------------------------------------------------------------------
Gender<-c(Data_online$Q4,Data_field$Q4)
table(Gender)
prop.table(table(Gender))


## -----------------------------------------------------------------------------------------------------------------------------
Student<-c(Data_online$Q8,Data_field$Q8)
table(Student)
prop.table(table(Student))
Romantic<-c(Data_online$Q7,Data_field$Q7)
table(Romantic)
prop.table(table(Romantic))


## -----------------------------------------------------------------------------------------------------------------------------
Data_analysis1<- Data_online %>% select(Q4,Q23:Q37,Q39,Q40,Q41,Q42,Q43,Q44,FL_21_DO_Scenario5_attractiveandnotdominantfemale,FL_21_DO_Scenario7_unattractiveanddominantfemale,FL_21_DO_Scenario6_attractiveanddominantfemale,FL_21_DO_Scenario8_unattractiveandnotdominantfemale,FL_17_DO_Scenario_attractiveandnon_dominantmale,FL_17_DO_Scenario3_dominantandunattractivemale,FL_17_DO_Scenario1_attractiveanddominantmale,FL_17_DO_Scenario4_NotDominantandUnattractivemale, Q93:Q98)
names(Data_analysis1)<-c("Gender","Suspicious","Betrayed","Worried","Distrustful","Jealousy","Rejected","Hurt","Anxious","Angry", "Threatened","Sad","Upset","Attractiveness_check1","Attractiveness_check2","Assertiveness","Self_confidence","Extroverted","Influential","Socially_competent","Good_judge_of_character","Attractive_not_dominant_woman","Unattractive_dominant_woman","Attractive_dominant_woman","Unattractive_not_dominant_woman","Attractive_not_dominant_man","Unattractive_dominant_man","Attractive_dominant_man","Unattractive_not_dominant_man","Mate_value1","Mate_value2", "Mate_value3","Mate_value4", "Mate_value5", "Mate_value6")
Data_analysis1$Sample<-rep("Online",nrow(Data_analysis1))
# For some reason qualtrics exports.The label here but not elsewhere Q34.
Data_analysis2<- Data_field %>% select(Q4,Q23:Q34, Q36:Q37,Q39,Q40,Q41,Q42,Q43,Q44,FL_21_DO_Scenario5_attractiveandnotdominantfemale,FL_21_DO_Scenario7_unattractiveanddominantfemale,FL_21_DO_Scenario6_attractiveanddominantfemale,FL_21_DO_Scenario8_unattractiveandnotdominantfemale,FL_17_DO_Scenario_attractiveandnon_dominantmale,FL_17_DO_Scenario3_dominantandunattractivemale,FL_17_DO_Scenario1_attractiveanddominantmale,FL_17_DO_Scenario4_NotDominantandUnattractivemale, Q93:Q98)
names(Data_analysis2)<-c("Gender","Suspicious","Betrayed","Worried","Distrustful","Jealousy","Rejected","Hurt","Anxious","Angry", "Threatened","Sad","Upset","Attractiveness_check1","Attractiveness_check2","Assertiveness","Self_confidence","Extroverted","Influential","Socially_competent","Good_judge_of_character","Attractive_not_dominant_woman","Unattractive_dominant_woman","Attractive_dominant_woman","Unattractive_not_dominant_woman","Attractive_not_dominant_man","Unattractive_dominant_man","Attractive_dominant_man","Unattractive_not_dominant_man", "Mate_value1","Mate_value2", "Mate_value3","Mate_value4", "Mate_value5", "Mate_value6")
Data_analysis2$Sample<-rep("Field",nrow(Data_analysis2))
Data_analysis<-rbind(Data_analysis1,Data_analysis2)


## -----------------------------------------------------------------------------------------------------------------------------
Data_analysis<-Data_analysis %>% mutate(Attractive_condition=case_when(Attractive_not_dominant_woman == 1 | Attractive_dominant_woman == 1 | Attractive_not_dominant_man == 1 | Attractive_dominant_man == 1 ~ "Attractive", TRUE ~ "Not Attractive"), Dominant_condition=case_when(Attractive_dominant_woman == 1 | Unattractive_dominant_woman == 1 | Attractive_dominant_man == 1 | Unattractive_dominant_man == 1 ~ "Dominant", TRUE ~ "Not Dominant"), ID=1:nrow(Data_analysis))
write.csv(Data_analysis, "Dataset2.csv", row.names = F)
write_sav(Data_analysis, "Dataset2.sav")


## -----------------------------------------------------------------------------------------------------------------------------
require(psych)
alpha<- Data_analysis %>% dplyr::select(Mate_value1:Mate_value6)
require(psych)
psych::alpha(as.data.frame(alpha))


## -----------------------------------------------------------------------------------------------------------------------------
Data_analysis<-Data_analysis %>% mutate(Mate_value= Mate_value1+Mate_value2+Mate_value3+Mate_value4+Mate_value5+Mate_value6)


## -----------------------------------------------------------------------------------------------------------------------------
t.test(Mate_value~Gender, data=Data_analysis)


## -----------------------------------------------------------------------------------------------------------------------------
describeBy(Data_analysis$Mate_value, Data_analysis$Gender)


## -----------------------------------------------------------------------------------------------------------------------------
library(lsr)
cohensD(Data_analysis$Mate_value~ Data_analysis$Gender)

## -----------------------------------------------------------------------------------------------------------------------------
library(compute.es)
des(d=.2240383, n.1=178, n.2=277)


## -----------------------------------------------------------------------------------------------------------------------------
t.test(Mate_value~Attractive_condition, data=Data_analysis)
t.test(Mate_value~Dominant_condition, data=Data_analysis)


## -----------------------------------------------------------------------------------------------------------------------------
require(dplyr)
Data_analysis_men<- Data_analysis %>% filter(Gender==1)
Data_analysis_women<- Data_analysis %>% filter(Gender==2)



## -----------------------------------------------------------------------------------------------------------------------------
require(ez)
require(papaja)
Ez_ANOVA1<-ezANOVA(Data_analysis_men, dv=Attractiveness_check1, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA1
Ez_ANOVA2<-ezANOVA(Data_analysis_women, dv=Attractiveness_check1, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA2
Ez_ANOVA3<-ezANOVA(Data_analysis_men, dv=Attractiveness_check2, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA3
Ez_ANOVA4<-ezANOVA(Data_analysis_women, dv=Attractiveness_check2, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type = 3)
Ez_ANOVA4


## -----------------------------------------------------------------------------------------------------------------------------
require(apa)
anova_apa(Ez_ANOVA1, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA2, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA3, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA4, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
summary_attractive_men <- Data_analysis_men  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Attractiveness_check1), # Then summarize each group, these are complete so we can use same N
            Mean_att1 = mean(Attractiveness_check1),
            SD_att1 = sd(Attractiveness_check1),
            SE_att1 = SD_att1/sqrt(N), # Then summarize each group, these are complete so we can use same N
            Mean_att2 = mean(Attractiveness_check2),
            SD_att2 = sd(Attractiveness_check2),
            SE_att2 = SD_att2/sqrt(N)) 
summary_attractive_men

summary_attractive_women <- Data_analysis_women  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Attractiveness_check1), # Then summarize each group, these are complete so we can use same N
            Mean_att1 = mean(Attractiveness_check1),
            SD_att1 = sd(Attractiveness_check1),
            SE_att1 = SD_att1/sqrt(N), # Then summarize each group, these are complete so we can use same N
            Mean_att2 = mean(Attractiveness_check2),
            SD_att2 = sd(Attractiveness_check2),
            SE_att2 = SD_att2/sqrt(N)) 
summary_attractive_women



## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_men <- manova(cbind(Assertiveness,Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition, data = Data_analysis_men)
summary(Manovamodel_men)
summary(Manovamodel_men, test="Roy")
summary(Manovamodel_men, test="Wilks")
summary(Manovamodel_men, test="Hotelling-Lawley")
summary.aov(Manovamodel_men)


## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_men_attr <- manova(cbind(Assertiveness,Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition*Attractive_condition, data = Data_analysis_men)
summary(Manovamodel_men_attr)
summary(Manovamodel_men_attr, test="Roy")
summary(Manovamodel_men_attr, test="Wilks")
summary(Manovamodel_men_attr, test="Hotelling-Lawley")
summary.aov(Manovamodel_men_attr)
Manovamodel_men_attr$coefficients # check signs.


## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_women <- manova(cbind(Assertiveness,Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition, data = Data_analysis_women)
summary(Manovamodel_women)
summary(Manovamodel_women, test="Roy")
summary(Manovamodel_women, test="Wilks")
summary(Manovamodel_women, test="Hotelling-Lawley")
summary.aov(Manovamodel_women)


## -----------------------------------------------------------------------------------------------------------------------------
Manovamodel_women_attr <- manova(cbind(Assertiveness,Self_confidence,Extroverted,Influential,Socially_competent,Good_judge_of_character) ~ Dominant_condition*Attractive_condition, data = Data_analysis_women)
summary(Manovamodel_women_attr)
summary(Manovamodel_women_attr, test="Roy")
summary(Manovamodel_women_attr, test="Wilks")
summary(Manovamodel_women_attr, test="Hotelling-Lawley")
summary.aov(Manovamodel_women_attr)
Manovamodel_women_attr$coefficients # check signs.


## -----------------------------------------------------------------------------------------------------------------------------
Ez_ANOVA_jealousy<-ezANOVA(Data_analysis, dv=Jealousy, wid=ID, between=.(Attractive_condition,Dominant_condition,Gender), detailed=TRUE, type=3)
Ez_ANOVA_jealousy
anova_apa(Ez_ANOVA_jealousy, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
require(dplyr)
Data_analysis_covar<- Data_analysis %>% filter(!is.na(Mate_value))
Ez_ANOVA_jealousy_covar<-ezANOVA(Data_analysis_covar, dv=Jealousy, wid=ID, between=.(Attractive_condition,Dominant_condition,Gender), between_covariates = Mate_value, detailed=TRUE, type=3)
Ez_ANOVA_jealousy_covar
anova_apa(Ez_ANOVA_jealousy_covar, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
Ez_ANOVA_jealousy_men<-ezANOVA(Data_analysis_men, dv=Jealousy, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type=3)
Ez_ANOVA_jealousy_men
Ez_ANOVA_jealousy_women<-ezANOVA(Data_analysis_women, dv=Jealousy, wid=ID, between=Attractive_condition*Dominant_condition, detailed=TRUE, type=3)
Ez_ANOVA_jealousy_women
anova_apa(Ez_ANOVA_jealousy_men, es="ges", format = "rmarkdown")
anova_apa(Ez_ANOVA_jealousy_women, es="ges", format = "rmarkdown")


## -----------------------------------------------------------------------------------------------------------------------------
summary_jealousy_attractiveness_men <- Data_analysis_men  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_attractiveness_men

summary_jealousy_attractiveness_women <- Data_analysis_women  %>% # "Start with the data set
  group_by(Attractive_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_attractiveness_women


## -----------------------------------------------------------------------------------------------------------------------------
summary_jealousy_dominant_men <- Data_analysis_men  %>% # "Start with the data set
  group_by(Dominant_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_dominant_men

summary_jealousy_dominant_women <- Data_analysis_women  %>% # "Start with the data set
  group_by(Dominant_condition) %>% # Then group by IV
  summarize(N = length(Jealousy), # Then summarize each group, these are complete so we can use same N
            Mean_jeal = mean(Jealousy),
            SD_jeal = sd(Jealousy),
            SE_jeal = SD_jeal/sqrt(N)) # Then summarize each group, these are complete so we can use same N
summary_jealousy_dominant_women


## -----------------------------------------------------------------------------------------------------------------------------
require(ggplot2)
require(ggthemes)
require(scales)
graph_data_men <- describeBy(Data_analysis_men$Jealousy,list(as.factor(Data_analysis_men$Attractive_condition),as.factor(Data_analysis_men$Dominant_condition)), mat=TRUE,digits=2)

graph_data_women <- describeBy(Data_analysis_women$Jealousy,list(as.factor(Data_analysis_women$Attractive_condition),as.factor(Data_analysis_women$Dominant_condition)), mat=TRUE,digits=2)

names(graph_data_men)[names(graph_data_men) == 'group1'] = 'Attractiveness'
names(graph_data_men)[names(graph_data_men) == 'group2'] = 'Dominance'  
names(graph_data_women)[names(graph_data_women) == 'group1'] = 'Attractiveness'
names(graph_data_women)[names(graph_data_women) == 'group2'] = 'Dominance'  

limits = aes(ymax = mean + sd, ymin=mean - sd)
dodge = position_dodge(width=0.9)
graph_men_bar<-ggplot(graph_data_men, aes(x = Attractiveness, y = mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_men_bar
# Reference set
reference_men_mean<-c(3.15,3,3.39,2.44)
reference_men_SD<-c(1.16,1.25,.85,.98)
Attractiveness<-c("Attractive","Attractive","Not Attractive", "Not Attractive")
Dominance<-c("Dominant","Not Dominant","Dominant", "Not Dominant")
reference_men<-cbind.data.frame(reference_men_mean,reference_men_SD,Attractiveness,Dominance)
limits = aes(ymax = reference_men_mean + reference_men_SD, ymin=reference_men_mean - reference_men_SD)
dodge = position_dodge(width=0.9)

graph_men_reference<-ggplot(reference_men, aes(x = Attractiveness, y = reference_men_mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+ 
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_men_reference
require(cowplot)
plot_grid(graph_men_bar, graph_men_reference, labels = c("A","B"), hjust = -8, label_size = 8,  ncol = 1, align = 'v')
ggsave("barplot_men_2.png")
ggsave("barplot_men_2.pdf")


## -----------------------------------------------------------------------------------------------------------------------------
require(ggplot2)
require(ggthemes)
require(scales)
graph_data_women <- describeBy(Data_analysis_women$Jealousy,list(as.factor(Data_analysis_women$Attractive_condition),as.factor(Data_analysis_women$Dominant_condition)), mat=TRUE,digits=2)

graph_data_women <- describeBy(Data_analysis_women$Jealousy,list(as.factor(Data_analysis_women$Attractive_condition),as.factor(Data_analysis_women$Dominant_condition)), mat=TRUE,digits=2)

names(graph_data_women)[names(graph_data_women) == 'group1'] = 'Attractiveness'
names(graph_data_women)[names(graph_data_women) == 'group2'] = 'Dominance'  
names(graph_data_women)[names(graph_data_women) == 'group1'] = 'Attractiveness'
names(graph_data_women)[names(graph_data_women) == 'group2'] = 'Dominance'  

limits = aes(ymax = mean + sd, ymin=mean - sd)
dodge = position_dodge(width=0.9)
graph_women_bar<-ggplot(graph_data_women, aes(x = Attractiveness, y = mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_women_bar
# Reference set
reference_women_mean<-c(3.62,3.48,2.47,2.83)
reference_women_SD<-c(1.12,1.12,1.07,1.34)
Attractiveness<-c("Attractive","Attractive","Not Attractive", "Not Attractive")
Dominance<-c("Dominant","Not Dominant","Dominant", "Not Dominant")
reference_women<-cbind.data.frame(reference_women_mean,reference_women_SD,Attractiveness,Dominance)
limits = aes(ymax = reference_women_mean + reference_women_SD, ymin=reference_women_mean - reference_women_SD)
dodge = position_dodge(width=0.9)

graph_women_reference<-ggplot(reference_women, aes(x = Attractiveness, y = reference_women_mean, fill = Dominance))+
  geom_bar(stat='identity', position=dodge)+ 
  geom_errorbar(limits, position=dodge, width=0.25) + xlab("Attractiveness condition")+ ylab("Mean Jealousy") + labs(fill="Dominance condition") + theme_tufte(10) + scale_fill_grey(start=.4) + coord_cartesian(ylim=c(1,5))
graph_women_reference
require(cowplot)
plot_grid(graph_women_bar, graph_women_reference, labels = c("A","B"), hjust = -8, label_size = 8,  ncol = 1, align = 'v')
ggsave("barplot_women_2.png")
ggsave("barplot_women_2.pdf")


## -----------------------------------------------------------------------------------------------------------------------------

Data_analysis$Gender<-as.factor(Data_analysis$Gender)
levels(Data_analysis$Gender) <- c("Men", "Women")
dodge <- position_dodge(width = 0.7)
violinplot<-ggplot(Data_analysis, aes(x = Attractive_condition, y = Jealousy, fill=Dominant_condition)) + geom_violin(position=dodge) + geom_boxplot(position=dodge,width = 0.1, outlier.colour=NA) + xlab("Attractiveness condition")+ ylab("Jealousy") + labs(fill="Dominance condition") + theme(legend.position = "top") + facet_grid(~Gender) + scale_fill_grey(start=.4) + theme_tufte(12)
violinplot
ggsave("violinplot_2.png", width= 200, height=200, units="mm")


## -----------------------------------------------------------------------------------------------------------------------------
histogram_men<-ggplot(Data_analysis_men, aes(x=Jealousy)) 
histogram_men <- histogram_men + geom_histogram(binwidth=.5, colour="black", fill="white") + facet_grid(Attractive_condition~Dominant_condition) + theme_tufte(7.5)

histogram_women<-ggplot(Data_analysis_women, aes(x=Jealousy)) 
histogram_women <- histogram_women + geom_histogram(binwidth=.5, colour="black", fill="white") + facet_grid(Attractive_condition~Dominant_condition) + theme_tufte(7.5)

plot_grid(histogram_men, histogram_women, labels = c("Men (N=178)","Women (N=278)"), label_size = 7,  ncol = 1, align = 'v')
ggsave("histogram_2.png")
ggsave("histogram_2.pdf")


## -----------------------------------------------------------------------------------------------------------------------------
require(coin)
require(BayesFactor)
Data_analysis$Attractive_condition<-as.factor(Data_analysis$Attractive_condition)
Data_analysis$Dominant_condition<-as.factor(Data_analysis$Dominant_condition)
Data_analysis$Gender<-as.factor(Data_analysis$Gender)
bf_jealousy <- anovaBF(Jealousy ~ Attractive_condition*Dominant_condition*Gender, data=as.data.frame(Data_analysis))
bf_jealousy
1/bf_jealousy 
independence_test(Jealousy ~ Attractive_condition*Dominant_condition*Gender, data=Data_analysis)


## -----------------------------------------------------------------------------------------------------------------------------
require(coin)
require(BayesFactor)
Data_analysis_men$Attractive_condition<-as.factor(Data_analysis_men$Attractive_condition)
Data_analysis_men$Dominant_condition<-as.factor(Data_analysis_men$Dominant_condition)
bf_jealousy_men <- anovaBF(Jealousy ~ Attractive_condition*Dominant_condition, data=as.data.frame(Data_analysis_men))
bf_jealousy_men
1/bf_jealousy_men 
independence_test(Jealousy ~ Attractive_condition*Dominant_condition, data=Data_analysis_men)
Data_analysis_women$Attractive_condition<-as.factor(Data_analysis_women$Attractive_condition)
Data_analysis_women$Dominant_condition<-as.factor(Data_analysis_women$Dominant_condition)
bf_jealousy_women <- anovaBF(Jealousy ~ Attractive_condition*Dominant_condition, data=as.data.frame(Data_analysis_women))
bf_jealousy_women
1/bf_jealousy_women # 
independence_test(Jealousy ~ Attractive_condition*Dominant_condition, data=Data_analysis_women)

