rm(list = ls())
# EDIT - Set directory
# Data file should be saved in a folder called "Data"
# Figures will be output to a folder called "Figs"
# Tables will be output to a folder called "Tabs"

library(statar)
library(doStata)
library(tidyverse)
library(stargazer)
library(scales)
require(cowplot)
require(xtable)
#### Read-in ####
df_original <- read.csv("C:/Users/spatt/Desktop/replicationworkshop/Public_access_datafile_APSR_Lee_2021.csv",stringsAsFactors = F)

### Pre-processing, df ####
df<-df_original%>%mutate(issue = ifelse(issue=="GMO","GMO Ban",issue),
                         issue = ifelse(issue=="RC","Rent Control",issue),
                         issue = ifelse(issue=="NEP","Needle Exchange",issue),
                         issue = factor(issue,levels = c("Needle Exchange","GMO Ban","Rent Control")),
                         preference_bin = ifelse(preference>0.5,1,0),
                         policy_bin = ifelse(preference>0.5,1,0),
                         preference_3 = ifelse(preference>0.5,1,0),
                         preference_3 = ifelse(preference==0.5,0.5,preference_3),
                         preference_3 = ifelse(preference<0.5,0,preference_3),
                         accuracy_3 = ifelse(accuracy>0.5,1,0),
                         accuracy_bin = ifelse(accuracy>0.5,1,0),
                         accuracy_3 = ifelse(accuracy==0.5,0.5,accuracy_3),
                         accuracy_3 = ifelse(accuracy<0.5,0,accuracy_3),
                         gender=  ifelse(female==1,"Women","Men"),
                         age_bin=  cut(age, breaks= quantile(age,probs = seq(0,1,1/2),na.rm = T),
                                       labels=c("Younger","Older")),
                         exp_bin=  cut(gov_exp, breaks= quantile(gov_exp,probs = seq(0,1,1/2),na.rm = T),
                                       labels=c("Less experience","More experience")),
                         polarization = republican_alignment -democrat_alignment,
                         moderate = ifelse(ideo5=="Moderate, middle of the road",1,0),
                         conservative = ifelse(ideo5%in%c("Somewhat conservative","Very conservative"),1,0),
                         republican = ifelse(pid_3=="Republican",1,NA),
                         republican = ifelse(!is.na(pid_indep)&pid_indep=="Republican Party",1,republican),
                         republican = ifelse(pid_3=="Democrat",0,republican),
                         republican = ifelse(!is.na(pid_indep)&pid_indep=="Democratic Party",0,republican),
                         republican_alignment <- ifelse(partisan_diss %in% c("Democrats are more likely to support rent control","Democrats are more likely to support a GMO ban","Republicans are more likely to support the use of needle exchanges"),1,0),
                         democrat_alignment <- ifelse(partisan_diss %in% c("Republicans are more likely to support rent control","Republicans are more likely to support a GMO ban","Democrats are more likely to support the use of needle exchanges"),1,0),
                         party_bin = factor(ifelse(republican==1,"Republican","Democrat")),
                         policymaker = 1,
                         survey = "CP18")

df$familiarity <- as.numeric(df$familiarity)
df$familiarity_3 <- ifelse(df$familiarity==1,1,NA)
df$familiarity_3 <- ifelse(df$familiarity==0.67,0.5,df$familiarity_3)
df$familiarity_3 <- ifelse(df$familiarity==0.33,0,df$familiarity_3)
df$familiarity_3 <- ifelse(df$familiarity==0,0,df$familiarity_3)

df$familiarity_bin <- ifelse(df$familiarity_3==0,0,1)

a <- "Least\nCongruent"
b <- "Somewhat\nCongruent"
c <- "Most\nCongruent"

df$preference_3_level <- ifelse(df$preference > 0.5, c,"")
df$preference_3_level <- ifelse(df$preference == 0.5,b,df$preference_3_level)
df$preference_3_level <- ifelse(df$preference < 0.5, a,df$preference_3_level)
df$preference_3_level <- factor(df$preference_3_level, levels = c(a,b,c))

a <- "Least\nAccurate"
b <- "Somewhat\nAccurate"
c <- "Most\nAccurate"
df$accuracy_3_level <- ifelse(df$accuracy > 0.5, c,"")
df$accuracy_3_level <- ifelse(df$accuracy == 0.5,b,df$accuracy_3_level)
df$accuracy_3_level <- ifelse(df$accuracy < 0.5, a,df$accuracy_3_level)
df$accuracy_3_level <- factor(df$accuracy_3_level, levels = c(a,b,c))

df$accuracy_posterior <- df$accuracy + df$accuracy_response
df$accuracy_response_bin = ifelse(df$accuracy_posterior>0.5&df$accuracy<0.51,1,NA)
df$accuracy_response_bin = ifelse(df$accuracy_posterior>0.5&df$accuracy>0.5,0,df$accuracy_response_bin)
df$accuracy_response_bin = ifelse(df$accuracy_posterior<0.51&df$accuracy<0.51,0,df$accuracy_response_bin)
df$accuracy_response_bin = ifelse(df$accuracy_posterior<0.51&df$accuracy>0.51,0,df$accuracy_response_bin)

df$preference_posterior <- df$preference + df$preference_response
df$preference_response_bin = ifelse(df$preference_posterior>0.5&df$preference<0.51,1,NA)
df$preference_response_bin = ifelse(df$preference_posterior>0.5&df$preference>0.51,0,df$preference_response_bin)
df$preference_response_bin = ifelse(df$preference_posterior<0.51&df$preference<0.51,0,df$preference_response_bin)
df$preference_response_bin = ifelse(df$preference_posterior<0.51&df$preference>0.51,0,df$preference_response_bin)

df$bias_num <- ifelse(df$bias == "Unbiased",1,0)

df[,"decision_factors_decision_factors_experts"] = ifelse(df[,"decision_factors_decision_factors_experts"]=="Extremely important","1",df[,"decision_factors_decision_factors_experts"])
df[,"decision_factors_decision_factors_experts"] = ifelse(df[,"decision_factors_decision_factors_experts"]=="Very important","0.75",df[,"decision_factors_decision_factors_experts"])
df[,"decision_factors_decision_factors_experts"] = ifelse(df[,"decision_factors_decision_factors_experts"]=="Moderately important","0.5",df[,"decision_factors_decision_factors_experts"])
df[,"decision_factors_decision_factors_experts"] = ifelse(df[,"decision_factors_decision_factors_experts"]=="Slightly important","0.25",df[,"decision_factors_decision_factors_experts"])
df[,"decision_factors_decision_factors_experts"] = as.numeric(ifelse(df[,"decision_factors_decision_factors_experts"]=="Not at all important","0",df[,"decision_factors_decision_factors_experts"]))

df$deference_num=ifelse(df$decision_factors_decision_factors_experts>0.25,1,0)
df$deference =ifelse(df$deference_num == 1,"High deference",NA)
df$deference =ifelse(df$deference_num == 0,"Low deference",NA)



panel_control <- df[df$treated==0,]

panel_treated <- panel_control%>%
  mutate(accuracy = accuracy + accuracy_response,
         preference = preference + preference_response,
         treated=1)

panel_stacked <- rbind(panel_treated,panel_control)
panel_stacked$method = "Within-\nsubject"
df$method = "Across-\nsubject"
combined=rbind(df,panel_stacked)

##### fig_pref_control_by_party [Fig 1] ######
results <- df %>%
  filter(treated==0&policymaker==1)%>%
  mutate(party_bin = as.character(ifelse(republican == 1,"Rep. respondents","Dem. respondents")),
         party_bin = factor(party_bin,levels = c("Rep. respondents","Dem. respondents")))%>%
  select(policy_bin,party_bin,issue)%>%
  filter(!is.na(issue)&!is.na(policy_bin)&!is.na(party_bin))%>%
  group_by(party_bin,issue)%>%
  summarise(mean = mean(policy_bin, na.rm = TRUE),
            sd      = sd(policy_bin, na.rm = TRUE),
            n       = sum(!is.na(policy_bin)))

results$se = results$sd/sqrt(results$n)
results$ci = results$se*1.96

ggplot(data=results, aes(fill= party_bin, y=mean, x=issue)) +
  geom_bar(width = 0.5, position='dodge', stat='identity')+
  geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci), width=0.15, colour="gray48", position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c("firebrick","cornflowerblue"),guide=guide_legend(title=NULL))+
  #geom_text(aes(label=mean), position=position_dodge(width=0.9), vjust=5.5, size=4)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  xlab('')+ 
  ylab('')+
  ggtitle("")+
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = c("Support\nNeedle Exchange","Oppose\nGMO Ban","Oppose\nRent Control"))+
  theme(legend.position = "right") + 
  theme_minimal()+
  ggsave("Figs/fig_pref_control.png", unit = "in",width=7.5, height=5,dpi = 600)




##### fig_effects.png [Fig 2] ####
range <- c(-.09,.25)
reg1b <- lm(accuracy~ treated, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",])
reg2b <- lm(accuracy~ treated, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",])
reg3b <- lm(accuracy~ treated, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",])
reg1p <- lm(preference~ treated, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",])
reg2p <- lm(preference~ treated, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",])
reg3p <- lm(preference~ treated, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",])


fig_effect = function(reg_effect,range,ylabel,subtitle,maintitle){
  coeffs_effect = as.data.frame(summary(reg_effect)$coefficient)
  vcov_effect = vcov(reg_effect)
  results = as.data.frame(matrix(,1,3))
  colnames(results) = c("mean","se","ci")
  results[,"mean"] <- coeffs_effect["treated","Estimate"]
  results[,"se"] <- coeffs_effect["treated","Std. Error"]
  results$ci <- results$se * 1.96
  # results$colour = c("#006600","#003300")
  
  p <- ggplot(data=results, aes(y=mean,x="")) +
    geom_point(stat='identity',size=0.5)+
    geom_errorbar(aes(ymin = mean - ci,ymax = mean + ci),width = 0.05)+
    labs(y=ylabel, subtitle=subtitle,title=maintitle) +
    geom_hline(yintercept=0,lty=3)+
    theme_minimal()+
    
    scale_y_continuous(breaks = c(-0.05,0, 0.05,0.1,0.15,0.2),limits = range,
                       labels = c("","0%-pts","","10%-pts","","20%-pts"))+
    theme(legend.position = "none",axis.title.x = element_blank(),
          plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
          axis.title.y = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=9),
          axis.ticks.x = element_blank(),
          plot.title = element_text(face = 'bold',hjust = 0.5,size=12))
  return(p)}


beliefs = plot_grid(fig_effect(reg1b,range,"Δ Accuracy","Belief about Experts","Needle Exchange"),
                    fig_effect(reg2b,range,"Δ Accuracy","Belief about Experts","GMO Ban"),
                    fig_effect(reg3b,range,"Δ Accuracy","Belief about Experts","Rent Control"),ncol=1)

preference = plot_grid(fig_effect(reg1b,range,"Δ Congruence","Policy Preference","Needle Exchange"),
                       fig_effect(reg2b,range,"Δ Congruence","Policy Preference","GMO Ban"),
                       fig_effect(reg3b,range,"Δ Congruence","Policy Preference","Rent Control"),ncol=1)


p <- plot_grid(beliefs,preference,ncol=2,rel_widths  = c(1,1))


plot_grid(p) + ggsave("Figs/fig_effects.png", 
                      unit = "in",width=7.5, height=6.5)

#### fig_effects_by_party [Fig 3] #####
range <- c(-.09,.25)
reg1b <- lm(accuracy~ treated*republican, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",])
reg2b <- lm(accuracy~ treated*republican, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",])
reg3b <- lm(accuracy~ treated*republican, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",])
reg1p <- lm(preference~ treated*republican, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",])
reg2p <- lm(preference~ treated*republican, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",])
reg3p <- lm(preference~ treated*republican, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",])

fig_effects_by_party = function(reg,range,ylabel,subtitle,maintitle){
  coeffs = as.data.frame(summary(reg)$coefficient)
  vcov = vcov(reg)
  results = as.data.frame(matrix(,2,3))
  colnames(results) = c("mean","se","ci")
  rownames(results) = c("democrat","republican")
  results["democrat","mean"] <- coeffs["treated","Estimate"]
  results["democrat","se"] <- coeffs["treated","Std. Error"]
  results["republican","mean"] <- coeffs["treated","Estimate"] + coeffs["treated:republican","Estimate"]
  results["republican","se"] <- sqrt((vcov["treated:republican","treated:republican"] +  vcov["treated","treated"] + 2*(vcov["treated","treated:republican"])))
  results$ci <- results$se * 1.96
  results$party =c("democrat","republican")
  results$colour = c("cornflowerblue","firebrick")
  
  p <- ggplot(results, aes(y=mean,x =party,range = range, colour = party)) +
    geom_point(stat='identity')+
    geom_errorbar(aes(ymin = mean - ci,ymax = mean + ci),width = 0.05)+
    theme_minimal()+
    labs(y=ylabel, subtitle=subtitle,title=maintitle, x="") +
    geom_hline(yintercept=0,lty=3)+
    scale_y_continuous(breaks = c(-0.05,0, 0.05,0.1,0.15,0.2),limits = range,
                       labels = c("","0%-pts","","10%-pts","","20%-pts"))+
    scale_colour_manual(values = results$colour)+
    theme(legend.position = "none",axis.title.x = element_blank(),
          plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
          axis.title.y = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(face = 'bold',hjust = 0.5,size=12))
  return(p)}

beliefs = plot_grid(fig_effects_by_party(reg1b,range,"Δ Accuracy","Belief about Experts","Needle Exchange"),
                    fig_effects_by_party(reg2b,range,"Δ Accuracy","Belief about Experts","GMO Ban"),
                    fig_effects_by_party(reg3b,range,"Δ Accuracy","Belief about Experts","Rent Control"),ncol=1)

prefs = plot_grid(fig_effects_by_party(reg1p,range,"Δ Congruence","Policy Preference","Needle Exchange"),
                  fig_effects_by_party(reg2p,range,"Δ Congruence","Policy Preference","GMO Ban"),
                  fig_effects_by_party(reg3p,range,"Δ Congruence","Policy Preference","Rent Control"),ncol=1)

p <- plot_grid(beliefs,prefs,ncol=2,rel_widths  = c(1,1))

legend <- get_legend(fig_effects_by_party(reg2p,range,"","Policy Preferences","GMO Ban") + theme(legend.justification = "center",
                                                                                                 legend.text=element_text(size=11),legend.title = element_blank(),
                                                                                                 legend.background = element_rect(colour = 'white'),
                                                                                                 legend.position = "bottom")+
                       theme(legend.position = "bottom",legend.justification = 0.5)+
                       theme(legend.key.width=unit(2,"line")) +
                       theme(legend.text = element_text(margin = margin(r = 10, l =  10, unit = "pt"))))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1)) + ggsave("Figs/fig_effects_by_party.png", 
                                                         unit = "in",width=7.5, height=6.5)
dev.off()

#### fig_updating_by_prior [Fig 4] #####
fig_belief_updating_by_prior = function(reg,range,ylabel,subtitle,maintitle){
  coeffs = as.data.frame(summary(reg)$coefficient)
  vcov = vcov(reg)
  results = as.data.frame(matrix(,3,3))
  colnames(results) = c("mean","se","ci")
  results$accuracy_3_level = factor(levels(df$accuracy_3_level),levels = levels(df$accuracy_3_level))
  results[,"mean"] <- coeffs[,"Estimate"]
  results[,"se"] <- coeffs[,"Std. Error"]
  results$ci <- results$se * 1.96
  #  results$colour = c("cornflowerblue","firebrick")
  
  p <- ggplot(results, aes(y=mean,x=accuracy_3_level)) +
    geom_point(stat='identity',size=0.5)+
    geom_errorbar(aes(ymin = mean - ci,ymax = mean + ci),width = 0.05)+
    labs(y=ylabel, subtitle=subtitle,title=maintitle, x=  "Pre-treatment Accuracy") +
    theme(plot.title = element_text(face = 'plain',hjust = 0.5,size=12))+
    geom_hline(yintercept=0,lty=3)+
    theme_minimal()+
    theme(legend.position = "none",axis.title.x = element_blank(),
          plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
          axis.title.y = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face = 'bold',hjust = 0.5,size=12))+
    # scale_colour_manual(values = results$colour)+
    scale_y_continuous(labels = c("","0%-pts","","30%-pts","","60%-pts"),
                       limits = range,breaks = c(-0.15,0,0.15,0.3,0.45,0.6))
  return(p)}

fig_preference_updating_by_prior = function(reg,range,ylabel,subtitle,maintitle){
  coeffs = as.data.frame(summary(reg)$coefficient)
  vcov = vcov(reg)
  results = as.data.frame(matrix(,3,3))
  colnames(results) = c("mean","se","ci")
  results$preference_3_level = factor(levels(df$preference_3_level),levels = levels(df$preference_3_level))
  results[,"mean"] <- coeffs[,"Estimate"]
  results[,"se"] <- coeffs[,"Std. Error"]
  results$ci <- results$se * 1.96
  #  results$colour = c("cornflowerblue","firebrick")
  
  p <- ggplot(results, aes(y=mean,x=preference_3_level)) +
    geom_point(stat='identity',size=0.5)+
    geom_errorbar(aes(ymin = mean - ci,ymax = mean + ci),width = 0.05)+
    labs(y=ylabel, subtitle=subtitle,title=maintitle, x=  "Pre-treatment Congruence") +
    theme(plot.title = element_text(face = 'plain',hjust = 0.5,size=12))+
    geom_hline(yintercept=0,lty=3)+
    theme_minimal()+
    theme(legend.position = "none",axis.title.x = element_blank(),
          plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
          axis.title.y = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face = 'bold',hjust = 0.5,size=12))+
    scale_y_continuous(limits = range, breaks = c(-0.05,0,0.05,0.1,0.15,0.2),
                       labels = c("","0%-pts","","10%-pts","","20%-pts"))
  # scale_colour_manual(values = results$colour)+
  return(p)}

reg1b <- lm(accuracy_response ~ accuracy_3_level -1 , d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",])
reg2b <- lm(accuracy_response ~ accuracy_3_level -1, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",])
reg3b <- lm(accuracy_response ~ accuracy_3_level - 1, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",])
reg1p <- lm(preference_response ~ preference_3_level -1 , d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",])
reg2p <- lm(preference_response ~ preference_3_level -1, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",])
reg3p <- lm(preference_response ~ preference_3_level - 1, d=df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",])

range_b <- c(-.15,.6)
range_p <- c(-.05,.2)

beliefs = plot_grid(fig_belief_updating_by_prior(reg1b,range_b,"Δ Accuracy","Belief about Experts","Needle Exchange"),
                    fig_belief_updating_by_prior(reg2b,range_b,"Δ Accuracy","Belief about Experts","GMO Ban"),
                    fig_belief_updating_by_prior(reg3b,range_b,"Δ Accuracy","Belief about Experts","Rent Control"),ncol=1)

prefs = plot_grid(fig_preference_updating_by_prior(reg1p,range_p,"Δ Congruence","Policy Preference","Needle Exchange"),
                  fig_preference_updating_by_prior(reg2p,range_p,"Δ Congruence","Policy Preference","GMO Ban"),
                  fig_preference_updating_by_prior(reg3p,range_p,"Δ Congruence","Policy Preference","Rent Control"),ncol=1)

p <- plot_grid(beliefs,prefs,ncol=2,rel_widths  = c(1,1))

plot_grid(beliefs,prefs,ncol=2,rel_widths  = c(1,1))+ 
  ggsave("Figs/fig_updating_by_prior.png",unit = "in",width=7.5, height=6)
dev.off()


### DESCRIPTIVES BY POLICIES
## fig_bias ####
results <- df %>%
  select(bias_num,party_bin,issue)%>%
  filter(!is.na(issue)&!is.na(bias_num)&!is.na(party_bin))%>%
  mutate(party_bin = as.character(ifelse(party_bin == "Republican","Rep. respondents","Dem. respondents")),
         party_bin = factor(party_bin,levels = c("Rep. respondents","Dem. respondents")))%>%
  group_by(party_bin,issue)%>%
  summarise(mean = mean(bias_num, na.rm = TRUE),
            sd      = sd(bias_num, na.rm = TRUE),
            n       = sum(!is.na(bias_num)))

results$se = results$sd/sqrt(results$n)
results$ci = results$se*1.96
limits <- aes(ymax = results$mean + results$ci, ymin=results$mean - results$ci)

ggplot(data=results, aes(fill= party_bin, y=mean, x=issue)) +
  geom_bar(width = 0.5, position='dodge', stat='identity')+
  geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci), width=0.15, colour="gray48", position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c("firebrick","cornflowerblue"),guide=guide_legend(title=NULL))+
  #geom_text(aes(label=mean), position=position_dodge(width=0.9), vjust=5.5, size=4)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  xlab('')+ 
  ylab('')+
  ggtitle("")+
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = c("Experts on\nNeedle Exchange","Scientists on\nGMO Ban","Economists on\nRent Control"))+
  theme(legend.position = "right") + 
  theme_minimal()+
  ggsave("Figs/fig_bias.png", unit = "in",width=7.5, height=5,dpi = 600)


## fig_deference ####
results <- df %>%
  select(deference_num,party_bin,issue)%>%
  filter(!is.na(issue)&!is.na(deference_num)&!is.na(party_bin))%>%
  mutate(party_bin = as.character(ifelse(party_bin == "Republican","Rep. respondents","Dem. respondents")),
         party_bin = factor(party_bin,levels = c("Rep. respondents","Dem. respondents")))%>%
  group_by(party_bin,issue)%>%
  summarise(mean = mean(deference_num, na.rm = TRUE),
            sd      = sd(deference_num, na.rm = TRUE),
            n       = sum(!is.na(deference_num)))

results$se = results$sd/sqrt(results$n)

results$ci = results$se*1.96
limits <- aes(ymax = results$mean + results$ci, ymin=results$mean - results$ci)

ggplot(data=results, aes(fill= party_bin, y=mean, x=issue)) +
  geom_bar(width = 0.5, position='dodge', stat='identity')+
  geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci), width=0.15, colour="gray48", position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c("firebrick","cornflowerblue"),guide=guide_legend(title=NULL))+
  #geom_text(aes(label=mean), position=position_dodge(width=0.9), vjust=5.5, size=4)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  xlab('')+ 
  ylab('')+
  ggtitle("")+
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = c("Experts on\nNeedle Exchange","Scientists on\nGMO Ban","Economists on\nRent Control"))+
  theme(legend.position = "right") + 
  theme_minimal()+
  ggsave("Figs/fig_deference.png", unit = "in",width=7.5, height=5,dpi = 600)


### fig_polarity ####
results <- df %>%
  select(polarization,party_bin,issue)%>%
  filter(!is.na(issue)&!is.na(polarization)&!is.na(party_bin))%>%
  mutate(party_bin = as.character(ifelse(party_bin == "Republican","Rep. respondents","Dem. respondents")),
         party_bin = factor(party_bin,levels = c("Rep. respondents","Dem. respondents")))%>%
  group_by(party_bin,issue)%>%
  summarise(mean = mean(polarization, na.rm = TRUE),
            sd      = sd(polarization, na.rm = TRUE),
            n       = sum(!is.na(polarization)))

results$se = results$sd/sqrt(results$n)
results$ci = results$se*1.96
limits <- aes(ymax = results$mean + results$ci, ymin=results$mean - results$ci)

ggplot(data=results, aes(fill= party_bin, y=mean, x=issue)) +
  geom_bar(width = 0.5, position='dodge', stat='identity')+
  geom_errorbar(limits, width=0.3, colour="gray48", position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("firebrick","cornflowerblue"),guide=guide_legend(title=NULL))+
  #geom_text(aes(label=mean), position=position_dodge(width=0.9), vjust=5.5, size=4)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('')+ 
  ylab('')+
  ylab("")+
  theme_minimal()+
  #ggtitle("Relative Likelihood that Republican\n(vs. Democratic) Party Supports Policy")+
  scale_y_continuous(labels = percent)+ ggsave("Figs/fig_polarity.png", unit = "in",width=7.5, height=5)



###ALTERNATIVE VISUALIZATION OF MAIN FIGURES
##### fig_levels#######
policy <- "Needle Exchange"
beliefs_NEP <- df%>%
  # df %>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(party_bin)) %>%
  select(treated,accuracy)%>%
  mutate(condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=condition,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(0,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("Belief")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=12),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Needle Exchange"
preferences_NEP <-  df %>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(party_bin)) %>%
  select(party_bin,treated,preference)%>%
  mutate(
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=condition,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,
                position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("Least congruent","","","","","",
                                "Most congruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("Preference")+# ggtitle("")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


policy <- "GMO Ban"
beliefs_GMO <- df %>% 
  # df%>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(party_bin)) %>%
  select(party_bin,treated,accuracy)%>%
  mutate(
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=condition,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(0,1),
                     labels = c("","","","",""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Rent Control"
beliefs_RC <- df %>% 
  # df%>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(party_bin)) %>%
  select(party_bin,treated,accuracy)%>%
  mutate(
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=condition,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(0,1),
                     labels = c("","","","",""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+ xlab("")+ylab("")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


policy <- "GMO Ban"
preferences_GMO <-   df %>% 
  # df%>%
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(party_bin)) %>%
  select(party_bin,treated,preference)%>%
  mutate(
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=condition,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("","","","","","",
                                ""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+ xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Rent Control"
preferences_RC <-   df %>% 
  # df%>%
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(party_bin)) %>%
  select(party_bin,treated,preference)%>%
  mutate(
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=condition,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("","","","","","",
                                ""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+ xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


beliefs <- plot_grid(beliefs_NEP,beliefs_GMO,beliefs_RC,nrow=1,rel_widths  = c(1.4,1,1))
preferences <- plot_grid(preferences_NEP,preferences_GMO,preferences_RC,nrow=1,rel_widths  = c(1.4,1,1))

p <-plot_grid(beliefs,preferences,nrow=2,rel_heights  = c(1,0.91))
legend <- get_legend(beliefs_NEP + theme(legend.justification = "center",
                                         legend.text=element_text(size=10),
                                         legend.background = element_rect(colour = 'white'),
                                         legend.position = "bottom"))

plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels.png", unit = "in",width=7.5, height=6.5)
##### fig_levels_by_party  #######
policy <- "Needle Exchange"
beliefs_NEP <- df%>%
 # df %>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(party_bin)) %>%
  select(party_bin,treated,accuracy)%>%
  mutate(type = party_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
   theme_minimal()+

    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(0,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("Belief")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
    theme(legend.position = "none",
          axis.text.y =element_text(size=8),
          axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
  axis.text.x = element_text(size=8),
  axis.title.x = element_text(size=12),
  plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Needle Exchange"
preferences_NEP <-  df %>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(party_bin)) %>%
  select(party_bin,treated,preference)%>%
  mutate(type = party_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,
                position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("Least congruent","","","","","",
                                "Most congruent"))+
  scale_color_manual(name = NULL, 
                        values = c("Without expert evidence" = "#006600",
                                   "With expert evidence"  = "#660099"))+
  xlab("")+ylab("Preference")+# ggtitle("")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


policy <- "GMO Ban"
beliefs_GMO <- df %>% 
# df%>% 
   filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(party_bin)) %>%
  select(party_bin,treated,accuracy)%>%
  mutate(type = party_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(0,1),
                     labels = c("","","","",""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Rent Control"
beliefs_RC <- df %>% 
# df%>% 
   filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(party_bin)) %>%
  select(party_bin,treated,accuracy)%>%
  mutate(type = party_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(0,1),
                     labels = c("","","","",""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+ xlab("")+ylab("")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


policy <- "GMO Ban"
preferences_GMO <-   df %>% 
# df%>%
   filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(party_bin)) %>%
  select(party_bin,treated,preference)%>%
  mutate(type = party_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("","","","","","",
                                ""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+ xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Rent Control"
preferences_RC <-   df %>% 
# df%>%
   filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(party_bin)) %>%
  select(party_bin,treated,preference)%>%
  mutate(type = party_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("","","","","","",
                                ""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+ xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


beliefs <- plot_grid(beliefs_NEP,beliefs_GMO,beliefs_RC,nrow=1,rel_widths  = c(1.4,1,1))
preferences <- plot_grid(preferences_NEP,preferences_GMO,preferences_RC,nrow=1,rel_widths  = c(1.4,1,1))

p <-plot_grid(beliefs,preferences,nrow=2,rel_heights  = c(1,0.91))
legend <- get_legend(beliefs_NEP + theme(legend.justification = "center",
                                         legend.text=element_text(size=10),
                                         legend.background = element_rect(colour = 'white'),
                                         legend.position = "bottom"))

plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_party.png", unit = "in",width=7.5, height=6.5)
##### fig_levels_by_prior #######
policy <- "Needle Exchange"

beliefs_NEP <-   panel_stacked%>%
  # df %>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(accuracy_3_level)) %>%
  select(accuracy_3_level,treated,accuracy)%>%
  mutate(type = accuracy_3_level,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("Belief")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Needle Exchange"
preferences_NEP <-     panel_stacked%>% 
  #df %>%
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(preference_3_level)) %>%
  select(preference_3_level,treated,preference)%>%
  mutate(type = preference_3_level,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,
                position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("Least congruent","","","","","",
                                "Most congruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("Preference")+# ggtitle("")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


policy <- "GMO Ban"
beliefs_GMO <-   panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(accuracy_3_level)) %>%
  select(accuracy_3_level,treated,accuracy)%>%
  mutate(type = accuracy_3_level,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("","","","",""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


policy <- "GMO Ban"
preferences_GMO <-   panel_stacked%>%
  # df%>%
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(preference_3_level)) %>%
  select(preference_3_level,treated,preference)%>%
  mutate(type = preference_3_level,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("","","","","","",
                                ""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Rent Control"
beliefs_RC <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(accuracy)&!is.na(accuracy_3_level)) %>%
  select(accuracy_3_level,treated,accuracy)%>%
  mutate(type = accuracy_3_level,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("","","","",""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle(policy)+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

policy <- "Rent Control"
preferences_RC <-   
  panel_stacked %>% 
  filter(survey=="CP18"&issue == policy&policymaker==1&!is.na(preference)&!is.na(preference_3_level)) %>%
  select(preference_3_level,treated,preference)%>%
  mutate(type = preference_3_level,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),limits=c(-0.01,1),
                     labels = c("","","","","","",
                                ""))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


beliefs <- plot_grid(beliefs_NEP,beliefs_GMO,beliefs_RC,nrow=1,rel_widths  = c(1.4,1,1))
preferences <- plot_grid(preferences_NEP,preferences_GMO,preferences_RC,nrow=1,rel_widths  = c(1.4,1,1))

p <-plot_grid(beliefs,preferences,nrow=2,rel_heights  = c(1,0.91))
legend <- get_legend(beliefs_NEP + theme(legend.justification = "center",
                                         legend.text=element_text(size=10),
                                         legend.background = element_rect(colour = 'white'),
                                         legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_prior.png", unit = "in",width=7.5, height=6.5)




### SUBSAMPLE ANALYSES (within-subject)
#Deference, Impartiality, Age, Gender, Gov Exp, Level of Gov, Position, Familiarity
##### fig_levels_by_staffer #######
panel_stacked$position = ifelse(panel_stacked$Staffer==0,"Elected Official",NA)
panel_stacked$position = ifelse(panel_stacked$Staffer==1,"Staffer",panel_stacked$position)
panel_stacked$position = factor(panel_stacked$position,levels = c("Elected Official","Staffer"))
beliefs <-   panel_stacked%>%
  # df %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(position)) %>%
  select(position,treated,accuracy)%>%
  mutate(type = position,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-     panel_stacked%>% 
  #df %>%
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(position)) %>%
  select(position,treated,preference)%>%
  mutate(type = position,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,
                position = position_dodge(width=0.5))+
  theme_minimal()+ scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                                        limits=c(-0.01,1),
                                        labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
                       scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Preferences")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                         legend.text=element_text(size=10),
                                         legend.background = element_rect(colour = 'white'),
                                         legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_elected.png", unit = "in",width=7.5, height=6.5)

##### fig_levels_by_level #######
panel_stacked$state = ifelse(is.na(panel_stacked$Level),NA,"local")
panel_stacked$state = ifelse(panel_stacked$Level=="state","state",panel_stacked$state)
beliefs <-   panel_stacked%>%
  # df %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(state)) %>%
  select(state,treated,accuracy)%>%
  mutate(type = state,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


preferences <-     panel_stacked%>% 
  #df %>%
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(state)) %>%
  select(state,treated,preference)%>%
  mutate(type = state,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,
                position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
 scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Preferences")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_levels.png", unit = "in",width=7.5, height=6.5)


##### fig_levels_by_familiarity#######
beliefs <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(familiarity_bin)) %>%
  select(familiarity_bin,treated,accuracy)%>%
  mutate(type = factor(ifelse(familiarity_bin==1,"High familiarity","Low familiarity"),
                       levels = c("Low familiarity","High familiarity")),
         type = factor(type,
                       levels = c("Low familiarity","High familiarity")),
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-   
  panel_stacked %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(familiarity_bin)) %>%
  select(familiarity_bin,treated,preference)%>%
  mutate(type = factor(ifelse(familiarity_bin==1,"High familiarity","Low familiarity"),
                       levels = c("Low familiarity","High familiarity")),
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  ggtitle("Preferences")+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))



p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                         legend.text=element_text(size=10),
                                         legend.background = element_rect(colour = 'white'),
                                         legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_familiarity.png", unit = "in",width=7.5, height=6.5)



##### fig_levels_by_bias #######
beliefs <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(bias)) %>%
  select(bias,treated,accuracy)%>%
  mutate(type = bias,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-   
  panel_stacked %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(bias)) %>%
  select(bias,treated,preference)%>%
  mutate(type = bias,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  ggtitle("Preferences")+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))



p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_bias.png", unit = "in",width=7.5, height=6.5)






##### fig_levels_by_deference #######
beliefs <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(deference_num)) %>%
  select(deference_num,treated,accuracy)%>%
  mutate(type = factor(ifelse(deference_num==1,"High deference","Low deference"),
                       levels = c("Low deference","High deference")),
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-   
  panel_stacked %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(deference_num)) %>%
  select(deference_num,treated,preference)%>%
  mutate(type = factor(ifelse(deference_num==1,"High deference","Low deference"),
                       levels = c("Low deference","High deference")),
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  ggtitle("Preferences")+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))



p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_deference.png", unit = "in",width=7.5, height=6.5)






##### fig_levels_by_age_bin #######
beliefs <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(age_bin)) %>%
  select(age_bin,treated,accuracy)%>%
  mutate(type = age_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-   
  panel_stacked %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(age_bin)) %>%
  select(age_bin,treated,preference)%>%
  mutate(type = age_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  ggtitle("Preferences")+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))



p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_age_bin.png", unit = "in",width=7.5, height=6.5)







##### fig_levels_by_exp_bin #######
beliefs <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(exp_bin)) %>%
  select(exp_bin,treated,accuracy)%>%
  mutate(type = exp_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-   
  panel_stacked %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(exp_bin)) %>%
  select(exp_bin,treated,preference)%>%
  mutate(type = exp_bin,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  ggtitle("Preferences")+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))



p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_exp_bin.png", unit = "in",width=7.5, height=6.5)


##### fig_levels_by_gender #######
beliefs <-    panel_stacked%>%
  # df%>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)&!is.na(gender)) %>%
  select(gender,treated,accuracy)%>%
  mutate(type = gender,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        # plot.subtitle = element_text(face = "plain",size=9,hjust = 0.5),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-   
  panel_stacked %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(gender)) %>%
  select(gender,treated,preference)%>%
  mutate(type = gender,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  ggtitle("Preferences")+
  xlab("")+ylab("")+
  # labs(subtitle = "Preference Congruence With Consensus")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=10),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


beliefs
p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_gender.png", unit = "in",width=7.5, height=6.5)

### VALIDATION PRE-POST

##### fig_levels_by_method #######
beliefs <-  combined%>%
  # df %>% 
  filter(survey=="CP18"&policymaker==1&!is.na(accuracy)) %>%
  select(method,accuracy,treated)%>%
  mutate(
    type = method,
    condition = factor(ifelse(treated == 1,"With expert evidence",
                              "Without expert evidence"),
                       levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(accuracy),
                                          se = sd(accuracy)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits=c(-0.01,1),
                     labels = c("Least accurate \n(<20% experts)","","","","Most accurate \n(>80% experts)"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ggtitle("Beliefs")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))

preferences <-    combined%>% 
  #df %>%
  filter(survey=="CP18"&policymaker==1&!is.na(preference)&!is.na(method)) %>%
  select(method,treated,preference)%>%
  mutate(type = method,
         condition = factor(ifelse(treated == 1,"With expert evidence",
                                   "Without expert evidence"), levels = c("Without expert evidence","With expert evidence")))%>%
  group_by(condition,type) %>%  summarise(. ,count = n(),mean = mean(preference),
                                          se = sd(preference)/sqrt(count), ci   = se * 1.96,
                                          lower = mean - ci,upper = mean + ci)%>%
  as.data.frame(.)%>%ggplot() + 
  aes(x=type,y =mean, color=condition) + 
  geom_point(stat='identity',size=0.5,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = lower, ymax = upper),width=0.2,
                position = position_dodge(width=0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,6/6),
                     limits=c(-0.01,1),
                     labels = c("Least   \ncongruent","","","","","","Most   \ncongruent"))+
  scale_color_manual(name = NULL, 
                     values = c("Without expert evidence" = "#006600",
                                "With expert evidence"  = "#660099"))+
  xlab("")+ylab("")+ ggtitle("Preferences")+
  # labs(subtitle="Belief about Experts")+
  theme(legend.position = "none",
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=11,vjust = -10,face = 'bold'),
        #        axis.title.y = element_text(size=10,vjust = -10,face = 'bold'),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=12),
        #axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold',hjust = 0.5,size=11))


p <-plot_grid(beliefs,preferences,nrow=1,rel_widths = c(1.2,1))
legend <- get_legend(beliefs + theme(legend.justification = "center",
                                     legend.text=element_text(size=10),
                                     legend.background = element_rect(colour = 'white'),
                                     legend.position = "bottom"))


plot_grid(p,legend,nrow=2,rel_heights = c(7,1))+  ggsave("Figs/fig_levels_by_method.png", unit = "in",width=7.5, height=6.5)



#### TABLES
##### Cieling Effects Tables ####
df %>% 
  filter(!is.na(accuracy)&!is.na(republican))%>%select(accuracy,party_bin,issue,condition)%>%
  # mutate(party_bin = factor(ifelse(party_bin == "control","Without expert message",
  # "With expert message"), levels = c("Without expert message","With expert message")))%>%
  group_by(party_bin,issue,condition) %>%
  mutate(sum = n())%>%
  ungroup%>%
  group_by(issue,accuracy,party_bin,condition) %>%
  summarise(. ,count = n(),
            sum = first(sum),
            prop = count/sum)%>%arrange(desc(accuracy))%>%filter(accuracy==1)


df %>% 
  filter(!is.na(preference)&!is.na(republican))%>%select(preference,party_bin,issue,condition)%>%
  # mutate(party_bin = factor(ifelse(party_bin == "control","Without expert message",
  # "With expert message"), levels = c("Without expert message","With expert message")))%>%
  group_by(party_bin,issue,condition) %>%
  mutate(sum = n())%>%
  ungroup%>%
  group_by(issue,preference,party_bin,condition) %>%
  summarise(. ,count = n(),
            sum = first(sum),
            prop = count/sum)%>%arrange(desc(preference))%>%filter(preference==1)


panel_stacked %>% 
  filter(!is.na(accuracy))%>%select(accuracy,accuracy_3_level,issue,treated)%>%tab(treated)
  
  panel_stacked %>% 
  filter(!is.na(accuracy))%>%select(accuracy,accuracy_3_level,issue,treated)%>%
  # mutate(accuracy_3_level = factor(ifelse(accuracy_3_level == "control","Without expert message",
  # "With expert message"), levels = c("Without expert message","With expert message")))%>%
  group_by(accuracy_3_level,issue,treated) %>%
  mutate(sum = n())%>%
  ungroup%>%
  group_by(issue,accuracy_3_level,treated,accuracy) %>%
  summarise(. ,count = n(),
            sum = first(sum),
            prop = count/sum)%>%arrange(desc(accuracy))%>%
  filter(accuracy==1&!accuracy_3_level%in%c("Most\nAccurate"))%>%select(-accuracy)

panel_stacked %>% 
  filter(!is.na(preference))%>%select(preference,preference_3_level,issue,treated)%>%
  # mutate(preference_3_level = factor(ifelse(preference_3_level == "control","Without expert message",
  # "With expert message"), levels = c("Without expert message","With expert message")))%>%
  group_by(preference_3_level,issue,treated) %>%
  mutate(sum = n())%>%
  ungroup%>%
  group_by(issue,preference,preference_3_level,treated) %>%
  summarise(. ,count = n(),
            sum = first(sum),
            prop = count/sum)%>%arrange(desc(preference))%>%filter(preference==1)%>%
filter(preference==1&!preference_3_level%in%c("Most\nCongruent"))






## Tables for effects ####
reg_experiment = function(df,outcome,interact){
  if(outcome == "accuracy"&interact==F){return(lm(accuracy ~ treated,d=df))}
  else{if(outcome == "accuracy"&interact==T){return(lm(accuracy ~ treated*republican,d=df))}
    else{if(outcome == "preference"&interact==F){return(lm(preference ~ treated,d=df))}
      else{if(outcome == "preference"&interact==T){return(lm(preference ~  treated*republican,d=df))}
    }}}}

regs <- list(reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy",F),
     reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy",T),
     reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy",F),
     reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy",T),
     reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy",F),
     reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy",T))
     
ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)
stargazer(regs, 
          out = "Tabs/tab_effects_beliefs_CP18.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
        #  covariate.labels = c("treated","republican"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

regs <- list(reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference",F),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference",T),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference",F),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference",T),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference",F),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference",T))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_preferences_CP18.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          #covariate.labels = c("treated","republican"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")


## tab_effects_staffer ####
reg_experiment = function(df,outcome){
  lm(preference ~  treated + treated*republican,d=df)}

regs <- list(reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange"&df$Staffer==0,],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban"&df$Staffer==0,],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control"&df$Staffer==0,],"preference"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_staffers.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          add.lines = list(c("Excludes Staffers?","No","Yes","No","Yes","No","Yes")),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F,notes.label = "")
## tab_effects_urban ####
reg_1 = function(df,outcome){
lm(preference ~  treated*Urban_bin,d=df)}

regs <- list(reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
              reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"))


ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_urban.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","GMO","Rent Control"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

## tab_effects_college ####
reg_1 = function(df,outcome){
  lm(preference ~  treated *Education_bin,d=df)}


regs <- list(reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_college.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","GMO","Rent Control"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")
## tab_effects_population ####
reg_1 = function(df,outcome){
  lm(preference ~  treated*Population_bin,d=df)}


regs <- list(reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_population.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","GMO","Rent Control"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")
## tab_effects_voteshare ####
reg_1 = function(df,outcome){
  lm(preference ~  treated*Voteshare_bin,d=df)}


regs <- list(reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_1(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_voteshare.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","GMO","Rent Control"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

## tab_effects_ideo ####
reg_experiment_simple = function(df,outcome){
  lm(preference ~  treated,d=df)}

reg_experiment_interact = function(df,outcome){
  lm(preference ~  treated*moderate + treated*conservative,d=df)}
regs <- list(reg_experiment_simple(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_experiment_interact(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_experiment_simple(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_experiment_interact(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_experiment_simple(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"),
             reg_experiment_interact(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_ideo.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")


## tab_effects_belief_bin ####
reg_experiment = function(df,outcome){
  lm(accuracy ~  treated*republican,d=df)}
reg_experiment_bin = function(df,outcome){
  lm(accuracy_bin ~  treated*republican,d=df)}
regs <- list(reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy"),
             reg_experiment_bin(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy"),
             reg_experiment_bin(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy"),
             reg_experiment_bin(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_belief_bin.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          add.lines = list(c("Binary Outcome?","No","Yes","No","Yes","No","Yes")),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

## tab_effects_preference_bin ####
reg_experiment = function(df,outcome){
  lm(preference ~  treated*republican,d=df)}
reg_experiment_bin = function(df,outcome){
  lm(preference_bin ~  treated*republican,d=df)}
regs <- list(reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_experiment_bin(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_experiment_bin(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference"),
             reg_experiment(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"),
             reg_experiment_bin(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference"))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_effects_preference_bin.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          add.lines = list(c("Binary Outcome?","No","Yes","No","Yes","No","Yes")),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", 
          dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")


## Tables for updating ####

c <- "Most Congruent"
b <- "Somewhat Congruent"
a <- "Least Congruent"

df$preference_3_level <- ifelse(df$preference > 0.5, c,"")
df$preference_3_level <- ifelse(df$preference == 0.5,b,df$preference_3_level)
df$preference_3_level <- ifelse(df$preference < 0.5, a,df$preference_3_level)
df$preference_3_level <- factor(df$preference_3_level, levels = c(a,b,c))

df%>%tab(preference_3_level)
c <- "Most Congruent"
b <- "Somewhat Congruent"
a <- "Least Congruent"

df$accuracy_3_level <- ifelse(df$accuracy > 0.5, c,"")
df$accuracy_3_level <- ifelse(df$accuracy == 0.5,b,df$accuracy_3_level)
df$accuracy_3_level <- ifelse(df$accuracy < 0.5, a,df$accuracy_3_level)
df$accuracy_3_level <- factor(df$accuracy_3_level, levels = c(a,b,c))

df$preference_3_level <- factor(df$preference_3_level, levels = c(b,a,c))
df$accuracy_3_level <- factor(df$accuracy_3_level, levels = c(b,a,c))

reg_update = function(df,outcome,interact){
  
  if(outcome == "accuracy"&interact==F){return(lm(accuracy_response ~ 1,d=df))}
  else{if(outcome == "accuracy"&interact==T){return(lm(accuracy_response ~ accuracy_3_level,d=df))}
    else{if(outcome == "preference"&interact==F){return(lm(preference_response ~ 1,d=df))}
      else{if(outcome == "preference"&interact==T){return(lm(preference_response ~  preference_3_level,d=df))}
      }}}}

regs <- list(reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy",T))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)
stargazer(regs, 
          out = "Tabs/tab_update_beliefs_CP18.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          order = c("Constant"),
          covariate.labels = c("Constant","Least Acccurate","Most Congruent"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

regs <- list(reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference",T))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_update_preferences_CP18.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
         order = c("Constant"),
          covariate.labels = c("Constant","Least congruent","Most congruent"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")


## Table updating by party and prior ####
regs <- list(lm(accuracy_response ~ republican,d=df[df$accuracy==0.5&df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",]),
                lm(accuracy_response ~ republican,d=df[df$accuracy==0.5&df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",]),
                   lm(accuracy_response ~ republican,d=df[df$accuracy==0.5&df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",]),
             lm(preference_response ~ republican,d=df[df$preference==0.5&df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",]),
             lm(preference_response ~ republican,d=df[df$preference==0.5&df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",]),
             lm(preference_response ~ republican,d=df[df$preference==0.5&df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",]))    

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)
stargazer(regs, 
          out = "Tabs/tab_update_party.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          order = c("Constant"),
          covariate.labels = c("Constant","Republican"),
          add.lines = list(c("Outcome",rep(c("Beliefs","Preferences"),3))),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")


## Tables for updating with weights ####
c <- "Most Congruent"
b <- "Somewhat Congruent"
a <- "Least Congruent"

df$preference_3_level <- ifelse(df$preference > 0.5, c,"")
df$preference_3_level <- ifelse(df$preference == 0.5,b,df$preference_3_level)
df$preference_3_level <- ifelse(df$preference < 0.5, a,df$preference_3_level)
df$preference_3_level <- factor(df$preference_3_level, levels = c(a,b,c))

c <- "Most Congruent"
b <- "Somewhat Congruent"
a <- "Least Congruent"

df$accuracy_3_level <- ifelse(df$accuracy > 0.5, c,"")
df$accuracy_3_level <- ifelse(df$accuracy == 0.5,b,df$accuracy_3_level)
df$accuracy_3_level <- ifelse(df$accuracy < 0.5, a,df$accuracy_3_level)
df$accuracy_3_level <- factor(df$accuracy_3_level, levels = c(a,b,c))

df$preference_3_level <- factor(df$preference_3_level, levels = c(b,a,c))
df$accuracy_3_level <- factor(df$accuracy_3_level, levels = c(b,a,c))

reg_update = function(df,outcome,interact){
  if(outcome == "accuracy"&interact==F){return(lm(accuracy_response ~ 1,d=df))}
  else{if(outcome == "accuracy"&interact==T){return(lm(accuracy_response ~ accuracy_3_level,d=df,weights = weight))}
    else{if(outcome == "preference"&interact==F){return(lm(preference_response ~ 1,d=df,weights = weight))}
      else{if(outcome == "preference"&interact==T){return(lm(preference_response ~  preference_3_level,d=df,weights = weight))}
      }}}}

regs <- list(reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"accuracy",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"accuracy",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"accuracy",T))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)
stargazer(regs, 
          out = "Tabs/tab_update_beliefs_CP18_with_weights.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          order = c("Constant"),
          covariate.labels = c("Constant","Least Acccurate","Most Congruent"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

regs <- list(reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Needle Exchange",],"preference",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="GMO Ban",],"preference",T),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference",F),
             reg_update(df[df$survey=="CP18"&df$policymaker==1&df$issue=="Rent Control",],"preference",T))

ses <- lapply(lapply(lapply(regs,vcov),diag),sqrt)

stargazer(regs, 
          out = "Tabs/tab_update_preferences_CP18_with_weights.tex", 
          type= "latex",
          title="",header=T, float = F,
          column.labels = c("Needles","Needles","GMO","GMO","Rent Control","Rent Control"),
          order = c("Constant"),
          covariate.labels = c("Constant","Least congruent","Most congruent"),
          model.numbers = F, omit.stat=c("ser","f","Rsq","adj.rsq"),
          se=ses,digits = 2,  dep.var.caption = "", dep.var.labels.include = F, no.space = T,
          notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, notes.label = "")

##### Familiarity Table #####
df%>%group_by(issue,area)%>%summarise(mean = mean(familiarity_bin,na.rm=T))%>%xtable(.)

df%>%group_by(issue,party_bin)%>%summarise(mean = mean(polarization,na.rm=T))%>%xtable(.)

