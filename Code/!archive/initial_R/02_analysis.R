## Note: I turned off all of the "refresh" in the make_table functions because I don't
## want them syncing with the Overleaf anymore. You will have to turn them back on
## with an updated "save_path" if you want to re-sync.

# Load libraries and data -------------------------------------------------
library(haven)
library(car)
library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)
library(kableExtra)
library(foreign)
library(multcomp)
library(gridExtra)
library(multmod)
source("functions/make_table.R")

df1 <- read_dta("data/cleaned_data.dta") %>% 
  mutate(messenger_new = case_when(messenger == 0 ~ "Other",
                                   messenger == 1 ~ "Messenger"),
         lost_new = case_when(lostT1 == 0 ~ "Won",
                              lostT1 == 1 ~ "Lost"),
         lost_new = relevel(factor(lost_new), "Won"),
         messenger_lost = paste(messenger_new, lost_new, sep = ", "),
         messenger_lost = factor(messenger_lost, levels = c("Messenger, Won", "Other, Won", "Messenger, Lost", "Other, Lost"))) %>% 
  filter(!is.na(lost_new)) %>% 
  group_by(messenger_new, lost_new) %>% 
  mutate(mean_dictator = mean(dictator_all, na.rm = T),
         mean_behavior = mean(behavior_all, na.rm = T)) %>% 
  ungroup()

df2 <- read_dta("data/cleaned_data2.dta") %>% 
  filter(!is.na(lostT1)) %>% 
  mutate(messenger_new = case_when(messenger == 0 ~ "Other",
                                   messenger == 1 ~ "Messenger"),
         lost_new = case_when(lostT1 == 0 ~ "Won",
                              lostT1 == 1 ~ "Lost"),
         lost_new = relevel(factor(lost_new), "Won"),
         messenger_lost = paste(messenger_new, lost_new, sep = ", "),
         messenger_lost = factor(messenger_lost, levels = c("Messenger, Won", "Other, Won", "Messenger, Lost", "Other, Lost"))) %>% 
  mutate(Instructions2 = haven::as_factor(Instructions2),
         Instructions2 = case_when(Instructions2 == "Shared" ~ "Shared Fate",
                                   Instructions2 == "Opposite" ~ "Opposite Fate", 
                                   TRUE ~ Instructions2),
         Instructions2 = factor(Instructions2, levels = c("Shared Fate", "Unrelated Fate", "Opposite Fate"))) %>% 
  mutate(Instructions1 = haven::as_factor(Instructions1),
         Instructions1 = factor(Instructions1, levels = c("0.50 Opposite", "0.05 Opposite", 
                                                          "0.50 Shared", "0.05 Shared", "Unrelated Fate" 
                                                          ))) %>% 
  mutate(likePartner = (likePartner-1)/6)

# Replace with own "save_path"
#save_path <- "../../../../Dropbox/Apps/Overleaf/STM/"

# Study 1 Behavior string
s1behavior_outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all", 
              "trust", "nice", "likeable", "generous", "likeOther")
s1behavior_outcomes_names <- c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index",
                    "Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index")
s1behavior_type = c(rep("behavioral", 3), "", rep("attitude", 4), "")


# Misc Analysis -----------------------------------------------------------


joint_hypothesis <- c("messenger  = 0", "lostT1:messenger = 0")

test <- lm_robust(behavior_all ~ lostT1*messenger, data = df2,
                  subset = Instructions2 == "Opposite Fate")

test2 <-  lm(behavior_all ~ lostT1, data = df2,
                  subset = Instructions2 == "Opposite Fate")

res <- summary(test)

2*pt(-abs(coef(res)[, 3]), test$df)

pt(coef(res)[, 3], test$df, lower = TRUE) # beta < 0
pt(coef(res)[, 3], test$df, lower = FALSE) # beta > 0

#test <- lm_robust(dictator_all ~ lostT1*messenger, data = df1)

texreg(test, include.ci = FALSE)

coefeq <- matrix(data=0, nrow=1, ncol=length(test$coefficients))

colnames(coefeq) <- names(test$coefficients)

coefeq[1,"messenger"] <- 1
coefeq[1,"lostT1:messenger"] <- 1

glht2 <- glht(test, linfct = c("messenger + lostT1:messenger = 0"))
summary(glht2)

glht2 <- glht(test, coefeq, rhs = 0, alternative = "two.sided")
summary(glht2)

rjoint_test <- linearHypothesis(test, joint_hypothesis,
                               test = "F")

joint_test


anova(test, test2)

# Checks ------------------------------------------------------------------

# Robustness checks for eggplant, huber attention check questions
# and manipulation check questions at the end of the survey
round(prop.table(table(df1$scamCheck)), digits = 2)
round(prop.table(table(df2$scamCheck)), digits = 2)

round(prop.table(table(df1$huber_acq_identify)), digits = 2)
round(prop.table(table(df2$huber_acq_identify)), digits = 2)

round(prop.table(table(df1$huber_acq_steal)), digits = 2)
round(prop.table(table(df2$huber_acq_steal)), digits = 2)

round(prop.table(table(df1$taskFair)), digits = 2)
round(prop.table(table(df1$accurateMsg)), digits = 2)
round(prop.table(table(df1$realperson1)), digits = 2)
round(prop.table(table(df1$realperson2)), digits = 2)

round(prop.table(table(df2$taskFair)), digits = 2)
round(prop.table(table(df2$accurateMsg)), digits = 2)
round(prop.table(table(df2$realperson1)), digits = 2)
round(prop.table(table(df2$realperson2)), digits = 2)

round(mean(df1$taskFair, na.rm = T), digits = 2)
round(mean(df1$accurateMsg, na.rm = T), digits = 2)
round(mean(df1$realperson1, na.rm = T), digits = 2)
round(mean(df1$realperson2, na.rm = T), digits = 2)

round(mean(df2$taskFair, na.rm = T), digits = 2)
round(mean(df2$accurateMsg, na.rm = T), digits = 2)
round(mean(df2$realperson1, na.rm = T), digits = 2)
round(mean(df2$realperson2, na.rm = T), digits = 2)



# Study 1 -----------------------------------------------------------------
# *Section 1.1 Summary Tables (Tables 1-9)----------------------------------------------------------
for(i in seq_along(s1behavior_outcomes)){
  
  summary_table <- df1 %>% 
    group_by(messenger_lost) %>% 
    summarise(Mean = mean(!!sym(s1behavior_outcomes[i]), na.rm = T),
              SD = sd(!!sym(s1behavior_outcomes[i]), na.rm = T),
              Median = median(!!sym(s1behavior_outcomes[i]), na.rm = T),
              N = n()) %>% 
    rename(" " = messenger_lost)
  
  make_kable(paste0("study1_", s1behavior_outcomes[i]), summary_table, 
             caption = paste0("Mean outcomes of the ", s1behavior_outcomes_names[i], " ", s1behavior_type[i], " measures by Task 1 win/loss and 
                              presence of a messenger, Study 1"))
}
# *Behavior Histograms (goes in figures, not in Latex) -----------------------------
g <- ggplot(df1 %>% filter(!is.na(lost_new)), aes(x = dictator_all)) +
  geom_histogram(binwidth = 8.3, color = "black", fill = "white", na.rm = TRUE, 
                 aes(y=..density..)) +
  facet_wrap(messenger_new ~ lost_new, labeller = label_wrap_gen(multi_line=FALSE),
             scales = "free_x") + 
  labs(x = "Dictator Game", y = "Density") + 
  geom_vline(xintercept = 25,linetype = "dotted" )+
  scale_x_continuous(limits = c(-5, 55), breaks = seq(0, 50, by = 10)) +
  theme_few()

ggsave(g, file = paste0("figures/", "hist_dictator_all", ".png"))


g <- ggplot(df1 %>% filter(!is.na(lost_new)), aes(x = behavior_all)) +
  geom_histogram(binwidth = .5,color = "black", fill = "white", na.rm = TRUE, 
                 aes(y=..density..)) +
  facet_wrap(messenger_new ~ lost_new, labeller = label_wrap_gen(multi_line=FALSE),
             scales = "free_x")+
  geom_vline(xintercept = 0,linetype = "dotted" )+
  labs(x = "Behavioral Index", y = "Density") + 
  theme_few()

ggsave(g, file = paste0("figures/", "hist_behavior_all", ".png"))

# *Behavior Table 10. Main regression with and without robust ----------------------
outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all")

base_list <- map(outcomes,
                ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1))

# robust_list <- map(outcomes,
#                 ~lm_robust(as.formula(
#                   paste(paste0(.x, "~lostT1*messenger"),
#                         "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
#                   data = df1))

robust_list <- map(outcomes[4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)






# STM effect
pvalues1 <- rep(0, 5)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("messenger + lostT1:messenger = 0")))$test$pvalues)
}

# RTM effect
pvalues2 <- rep(0,5)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[3]
}

# RTM = -STM?
pvalues3 <- rep(0, 5)
for(i in seq_along(list)){
  pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
}

# Messenger Bias
pvalues4 <- rep(0,5)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[4]
}

make_texreg("behavior_regression", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}",
            "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 4), rep("Yes", 1)),
                                   "STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\cdot\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction (including demographic controls).", 
            scalebox = .9,
            refresh = FALSE,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                                                  In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."),
            digits = 2)

s1_behavior_pvalues4 <- round(pvalues4, digits = 2)
s1_behavior_pvalues2 <- round(pvalues2, digits = 2)

# *Behavior Triple Interactions (Tables 11-20) -----------------------------------------------------

covariate <- c("age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld")
covariate_name <- c("Age", "Female", "Ideology", "Education", "Employed", "White", "Hispanic", "Belief in Luck Scale", "Emotional Regulation Scale", "Belief in Just World Scale")


for(i in seq_along(covariate)){
  base_list <- map(outcomes,
                   ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*", covariate[i])), data = df1))
  
  custom_coef_names <- c("lostT1", "messenger", "lostT1:messenger", covariate[i], paste0("lostT1:", covariate[i]), paste0("messenger:", covariate[i]),
                         paste0("lostT1:messenger:", covariate[i]), "(Intercept)")
  custom_coef_labels <- c("Lost T1", "Messenger", "Lost x Messenger", covariate_name[i], paste0("Lost x ", covariate_name[i]), paste0("Messenger x ", covariate_name[i]),
                          paste0("Lost x Messenger x ", covariate_name[i]), "Constant")
  
  custom.coef.map <- setNames(as.list(custom_coef_labels), custom_coef_names)
  
  make_texreg(paste0("behavior_regression_", covariate[i]), base_list,
              custom.model.names = c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index"),
              custom.coef.map = custom.coef.map,
              caption = paste0("Regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction, by ", covariate_name[i], "."),
              scalebox = .8,
              custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
              In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                  In the behavioral index, the DV is calculated by averaging the scores the dictator game, spin the wheel task, and prisoner's dilemma.",
                                  "\\item[\\hspace{-5mm}] %stars."),
              digits = 2, refresh = T)
}

# *Attitude Histograms (goes in figures, not in Latex) -----------------------------
ggplot(df1 %>% filter(!is.na(lost_new)), aes(x = likeOther)) +
  geom_histogram(binwidth = .166, color = "black", fill = "white", na.rm = TRUE, 
                 aes(y=..density..)) +
  facet_wrap(messenger_new ~ lost_new, labeller = label_wrap_gen(multi_line=FALSE),
             scales = "free_x") + 
  labs(x = "Attitudes Index", y = "Density") + 
  theme_few()


ggsave(g, file = paste0("figures/", "likeOther", ".png"))

outcomes <- c("trust", "nice", "likeable", "generous")
outcomes_name <- c("Trustworthy", "Nice", "Likeable", "Generous")
for(i in seq_along(outcomes)){
  
  g <- ggplot(df1 %>% filter(!is.na(lost_new)), aes(x = !!sym(outcomes[i]))) +
    geom_histogram(color = "black", fill = "white", na.rm = TRUE, 
                   aes(y=..density..), breaks = seq(0.5, 7.5, by = 1)) +
    facet_wrap(messenger_new ~ lost_new, labeller = label_wrap_gen(multi_line=FALSE),
               scales = "free_x") + 
    labs(x = outcomes_name[i], y = "Density") + 
    scale_x_continuous(limits = c(0.5, 7.5), breaks = seq(1, 7, by = 1)) +
    theme_few()
  
  
  ggsave(g, file = paste0("figures/", outcomes[i], ".png"))
}



# *Attitude Table 21. Main regression with and without robust ----------------------
outcomes <- c("trust", "nice", "likeable", "generous", "likeOther")

base_list <- map(outcomes,
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1))

robust_list <- map(outcomes[5],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)

# STM effect
pvalues1 <- rep(0, 6)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("messenger + lostT1:messenger = 0")))$test$pvalues)
}

# RTM effect
pvalues2 <- rep(0,6)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[3]
}

# RTM = -STM?
pvalues3 <- rep(0, 6)
for(i in seq_along(list)){
  pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
}

# Messenger bias
pvalues4 <- rep(0,6)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[4]
}

make_texreg("attitude_regression", list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", "\\makecell{Attitudes\\\\Index}", "\\makecell{Attitudes\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 5), rep("Yes", 1)),
                                   "STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\cdot\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3),
            caption = "Regression of attitude measures on losing in task 1, being paired with the messenger, and their interaction (including demographic controls).",
            scalebox = .8,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."),
            digits = 2, refresh = F)

s1_attitude_pvalues4 <- round(pvalues4, digits = 2)
s1_attitude_pvalues3 <- round(pvalues3, digits = 2)

# *Attitude Triple Interaction (Tables 19-31) --------------------------------------
covariate <- c("age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld")
covariate_name <- c("Age", "Female", "Ideology", "Education", "Employed", "White", "Hispanic", "Belief in Luck Scale", "Emotional Regulation Scale", "Belief in Just World Scale")

for(i in seq_along(covariate)){
  base_list <- map(outcomes,
                   ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*", covariate[i])), data = df1))
  
  custom_coef_names <- c("lostT1", "messenger", "lostT1:messenger", covariate[i], paste0("lostT1:", covariate[i]), paste0("messenger:", covariate[i]),
                         paste0("lostT1:messenger:", covariate[i]), "(Intercept)")
  custom_coef_labels <- c("Lost T1", "Messenger", "Lost x Messenger", covariate_name[i], paste0("Lost x ", covariate_name[i]), paste0("Messenger x ", covariate_name[i]),
                          paste0("Lost x Messenger x ", covariate_name[i]), "Constant")
  
  custom.coef.map <- setNames(as.list(custom_coef_labels), custom_coef_names)
  
  make_texreg(paste0("attitude_regression_", covariate[i]), base_list,
              custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index"),
              custom.coef.map = custom.coef.map,
              caption = paste0("Regressions of the attitude measures on losing in task 1, being paired with the messenger, and their interaction, by ", covariate_name[i], "."),
              scalebox = .8,
              custom.note = paste("\\item[\\hspace{1mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses.",
                                  "\\item[\\hspace{1mm}] %stars."),
              digits = 2, refresh = T)
}


# Study 1 Main Figures ----------------------------------------------------

caption_all <- map_chr(s1behavior_outcomes_names, ~paste0("Difference in the DV of the ", .x, " for messengers and non-messengers 
                                              by losing (STM) and winning (RTM), Study 1"))

footnote_all <- c(
  "In the dictator game, the DV is giving up to 50 cents to the partner.",
  "In the spin the wheel task, the DV is a binary measure of choosing the partner to spin the wheel on one's behalf instead of the computer.",
  "In the prisoner's dilemma, the DV is a binary measure of choosing cooperation.",
  "In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.",
  "The trustworthy DV asks respondents to rate the messenger's trustworthiness on a 7-point Likert scale.",
  "The nice DV asks respondents to rate the messenger's niceness on a 7-point Likert scale.",
  "The likeable DV asks respondents to rate the messenger's likability on a 7-point Likert scale.",
  "The generous DV asks respondents to rate the messenger's generosity on a 7-point Likert scale.",
  "In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.")

all_list = list()

# First creates individual figures for each of the outcome variables
for(i in 1:9){
  d <- df1 %>% 
    mutate(messenger_new = relevel(factor(messenger_new), ref = "Other")) %>% 
    group_by(lost_new) %>% 
    reframe(tidy(lm_robust(as.formula(paste0(s1behavior_outcomes[i], "~ messenger_new")), data = pick(everything())))) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(lost_new = case_when(lost_new == "Won" ~ "Reward the\nMessenger",
                                lost_new == "Lost" ~ "Shoot the\nMessenger"),
           lost_new = fct_rev(factor(lost_new)))
  
  # Change y-axis label and limit/breaks depending on the outcome measure in question.
  if(s1behavior_outcomes[i] == "dictator_all"){
    yaxis = "Cents Given"
  } else if(s1behavior_outcomes[i] == "spin_all"){
    yaxis = "% Choose Human"
  } else if(s1behavior_outcomes[i] == "spin_all"){
    yaxis = "% Cooperate"
  } else if(s1behavior_outcomes[i] %in% c("trust", "nice", "likeable", "generous")){
    yaxis = "Rating"
  } else if(s1behavior_outcomes[i] %in% c("behavior_all", "likeOther")){
    yaxis = "Index"
  }
  
  if(s1behavior_outcomes[i] %in% c("spin_all", "pd_all", "behavior_all")){
    lim = c(-.3, .35)
    breaks = round(seq(-.3, .3, .1), digits = 2)
  } else if(s1behavior_outcomes[i] == "dictator_all"){
    lim = c(-4,4.55)
    breaks = seq(-4, 4, 1)
  } else if(s1behavior_outcomes[i] %in% c("trust", "nice", "likeable", "generous")){
    lim = c(-.5, .5)
    breaks = seq(-.5, .5, .1)
  } else if(s1behavior_outcomes[i] == "likeOther"){
    lim = c(-.1,.1)
    breaks = seq(-.1, .1, .05)
  }
  
  g <- ggplot(d, aes(x = lost_new, y = estimate)) +
    geom_bar(stat="identity", fill="lightgray", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                  position=position_dodge(.9)) +
    scale_y_continuous(limits = lim, breaks = breaks)+
    theme_few() + 
    geom_hline(yintercept = 0) +
    labs(title = s1behavior_outcomes_names[i], x= NULL, y = yaxis) + 
    theme(axis.text.x = element_text(colour = c("red", "green4")))

  
  ggsave(g, file = paste0(save_path, "figures/study1", s1behavior_outcomes[i], ".png"),
         height = 5.5, width = 4)
  
  tex <- paste0("\\renewcommand{\\baselinestretch}{1.25}%
\\begin{figure}[!t]%
  \\centering
  \\captionsetup{width=0.5\\linewidth}
  \\includegraphics[width=0.5\\linewidth]{figures/study1", s1behavior_outcomes[i], ".png}
  \\caption{", caption_all[i], ". 
  \\textit{Note: OLS regression with robust standard errors, with error bars representing 95\\% confidence intervals. ", footnote_all[i], "}}
  \\label{fig:study1", s1behavior_outcomes[i], "}
\\end{figure}%
\\renewcommand{\\baselinestretch}{1.67}%")
  
  cat(tex, sep = "\n", file = paste0(save_path, "figures_tex/study1_", s1behavior_outcomes[i], ".tex"))
  
  all_list[[i]] <- g
}

# Now, combine plots into two: behavior outcomes (1-4) and attitude outcomes (5-9)
behavior_list = all_list[1:4]
attitude_list = all_list[5:9]

# Make new ggplot for these two groups of outcomes (behaviors and attitudes)
study1_all <- function(object){
  all <- do.call("grid.arrange", c(object, nrow = 1))
  
  if(str_detect(deparse(substitute(object)), "behavior")){
    name <- "behavior"
    footnote <- "In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma."
    footnote_addition <- paste0(" The p-values of the test that $RTM = -STM$ are ", s1_behavior_pvalues3[1], ", ", s1_behavior_pvalues3[2], 
                                ", ", s1_behavior_pvalues3[3], ", and ", s1_behavior_pvalues3[4], 
    ", respectively, for each facet and the index. The p-values of TMB (RTM - STM) are ",
    s1_behavior_pvalues4[1], ", ", s1_behavior_pvalues4[2], ", ", s1_behavior_pvalues4[3], ", and ", 
    s1_behavior_pvalues4[4], ", respectively, for each facet and the index.")

  } else if (str_detect(deparse(substitute(object)), "attitude")){
    name <- "attitude"
    footnote <- "The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1."
    footnote_addition <- paste0(" The p-values of the test that $RTM = -STM$ are ", s1_attitude_pvalues3[1], ", ", s1_attitude_pvalues3[2], 
                                ", ", s1_attitude_pvalues3[3], ", ", s1_attitude_pvalues3[4], ", and ", s1_attitude_pvalues3[5], 
                                ", respectively, for each facet and index. The p-values of TMB (RTM - STM) are ",
                                s1_attitude_pvalues4[1], ", ", s1_attitude_pvalues4[2], ", ", s1_attitude_pvalues4[3], ", and ", 
                                s1_attitude_pvalues4[4], ", respectively, for each facet and the index.")
  }
  
  ggsave(all, file = paste0(save_path, "figures/study1_", name, "_list.png"),
         height = 5.5, width = 12)
  
  
  caption <- paste0("Difference in the DVs of the ", name, " measures for messengers and non-messengers 
                                              by losing (STM) and winning (RTM), Study 1")
  
  tex <- paste0("\\renewcommand{\\baselinestretch}{1.25}%
\\begin{figure}[!t]%
  \\centering
  \\includegraphics[width=1.0\\textwidth]{figures/study1_", name, "_list.png}
  \\caption{", caption, ". 
  \\textit{Note: OLS regression with robust standard errors, with error bars representing 95\\% confidence intervals. ", footnote, footnote_addition, "}}
  \\label{fig:", name, "_list}
\\end{figure}%
\\renewcommand{\\baselinestretch}{1.67}%")
  
  cat(tex, sep = "\n", file = paste0(save_path, "figures_tex/study1_", name, "_list.tex"))
  
}

study1_all(behavior_list)
study1_all(attitude_list)



# Study 2 -----------------------------------------------------------------

# *Behavior: Main regression with and without robust ----------------------
outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all")

# While these functions create regression tables, I have them to return pvalues4 
# (TMB) for use later on
study2_behavior <- function(fate){
base_list <- map(outcomes,
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), subset = Instructions2 ==fate, data = df2))

# robust_list <- map(outcomes,
#                 ~lm_robust(as.formula(
#                   paste(paste0(.x, "~lostT1*messenger"),
#                         "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
#                   data = df1))

robust_list <- map(outcomes[4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df2, subset = Instructions2 == fate))

list <- append(base_list, robust_list)

pvalues1 <- rep(0, 5)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("messenger + lostT1:messenger = 0")))$test$pvalues)
}

pvalues2 <- rep(0,5)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[3]
}

pvalues3 <- rep(0, 5)
for(i in seq_along(list)){
  pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
}

pvalues4 <- rep(0,5)
for(i in seq_along(list)){
  pvalues4[i] <-  list[[i]]$p.value[4]
}

make_texreg(paste0("behavior_", word(fate), "_regression"), list,
            custom.model.names = c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index",
                                   "Behavioral Index"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 4), rep("Yes", 1)),
                                   "STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\cdot\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3),
            caption = paste0("Regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction in the ", fate,
                             " condition (including demographic controls)."), 
            scalebox = .7,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                                                  In the behavioral index, the DV is calculated by averaging the scores the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."),
            digits = 2)

return(pvalues4)
}

unrelated_behavior_p <- study2_behavior("Unrelated Fate")[1:4] 
shared_behavior_p <- study2_behavior("Shared Fate")[1:4] 
opposite_behavior_p <- study2_behavior("Opposite Fate")[1:4] 



# *Attitude: Main regression with and without robust ----------------------

outcomes <- c("likePartner_1", "likePartner_2", "likePartner_3", "likePartner_4", "likePartner")


# While these functions create regression tables, I have them to return pvalues4
# (TMB) for use later on
study2_attitude <- function(fate){
  base_list <- map(outcomes,
                   ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), subset = Instructions2 ==fate, data = df2))

robust_list <- map(outcomes[5],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     subset = Instructions2 ==fate, data = df2))

list <- append(base_list, robust_list)

pvalues1 <- rep(0, 6)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("messenger + lostT1:messenger = 0")))$test$pvalues)
}

pvalues2 <- rep(0,6)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[3]
}

pvalues3 <- rep(0, 6)
for(i in seq_along(list)){
  pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
}

pvalues4 <- rep(0,6)
for(i in seq_along(list)){
  pvalues4[i] <-  list[[i]]$p.value[4]
}

make_texreg(paste0("attitude_", word(fate), "_regression"), list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index", "Attitudes Index"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 5), rep("Yes", 1)),
                                   "STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\cdot\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3),
            caption = paste0("Regressions of the attitude measures on losing in task 1, being paired with the messenger, and their interaction in the ", fate,
                             " condition (including demographic controls)."), 
            scalebox = .75,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
            The trustworthy, nice, likeable, and generous DVs ask respondents to rate the messenger's trustworthiness, niceness, likeability, and generosity, respectively,
on a 7-point Likert scale. In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1. 
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."),
            digits = 2, refresh = T)

return(pvalues4)
}

unrelated_attitude_p <- study2_attitude("Unrelated Fate")[1:5] 
shared_attitude_p <- study2_attitude("Shared Fate")[1:5] 
opposite_attitude_p <- study2_attitude("Opposite Fate")[1:5] 



# *Study 2 Main Figures ----------------------------------------------------
outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all", 
              "likePartner_1", "likePartner_2", "likePartner_3", "likePartner_4", "likePartner")
outcomes_names <- c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index",
                    "Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index")

caption_all <- map_chr(outcomes_names, ~paste0("Difference in messenger and non-messenger ratings of the ", .x, 
                                               " DV by losing (Shoot the Messenger) and winning (Reward the Messenger) across Unrelated Fate, Shared Fate, Opposite Fate alignments, Study 2"))

footnote_all <- c(
  "In the dictator game, the DV is giving up to 50 cents to the partner.",
  "In the spin the wheel task, the DV is a binary measure of choosing the partner to spin the wheel on one's behalf instead of the computer.",
  "In the prisoner's dilemma, the DV is a binary measure of choosing cooperation.",
  "In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.",
  "The trustworthy DV asks respondents to rate the messenger's trustworthiness on a 7-point Likert scale.",
  "The nice DV asks respondents to rate the messenger's niceness on a 7-point Likert scale.",
  "The likeable DV asks respondents to rate the messenger's likability on a 7-point Likert scale.",
  "The generous DV asks respondents to rate the messenger's generosity on a 7-point Likert scale.",
  "In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.")


behavior_tmb_pvalues <- c(round(shared_behavior_p[4], digits = 2),
  round(unrelated_behavior_p[4], digits = 2), round(opposite_behavior_p[4], digits = 2),
  s1_behavior_pvalues4[4])

attitude_tmb_pvalues <- c(round(shared_attitude_p[5], digits = 2),
                          round(unrelated_attitude_p[5], digits = 2), round(opposite_attitude_p[5], digits = 2),
                          s1_attitude_pvalues4[5])

study2_figures <- function(group = FALSE,
                           caption = NULL,
                           footnote = NULL,
                           fate){
  
  
  for(i in 1:9){
    
    exp1 <- df1 %>% 
      mutate(messenger_new = relevel(factor(messenger_new), ref = "Other")) %>% 
      group_by(lost_new) %>% 
      reframe(tidy(lm_robust(as.formula(paste0(s1behavior_outcomes[i], "~ messenger_new")), data = pick(everything())))) %>% 
      filter(term != "(Intercept)") %>% 
      mutate(lost_new = case_when(lost_new == "Won" ~ "Reward the\nMessenger",
                                  lost_new == "Lost" ~ "Shoot the\nMessenger"),
             lost_new = fct_rev(factor(lost_new))) %>% 
      mutate(Instructions2 = "Experiment 1")
    
    # This is for 5 vs 50 because it's "Instructions 1"
    if(as_label(enquo(fate)) == "Instructions1"){
      d <- df2 %>% 
        mutate(messenger_new = relevel(factor(messenger_new), ref = "Other")) %>% 
        group_by(lost_new, {{fate}}) %>% 
        reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "~ messenger_new")), data = pick(everything())))) %>% 
        filter(term != "(Intercept)") %>% 
        mutate(lost_new = case_when(lost_new == "Won" ~ "Reward the\nMessenger",
                                    lost_new == "Lost" ~ "Shoot the\nMessenger"),
               lost_new = fct_rev(factor(lost_new))) 
      # this is for SPILL-OVER, so we don't group by messenger_new
    }else if(group == FALSE){
      d <- df2 %>% 
      mutate(messenger_new = relevel(factor(messenger_new), ref = "Other")) %>% 
      group_by(lost_new, {{fate}}) %>% 
      reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "~ messenger_new")), data = pick(everything())))) %>% 
      filter(term != "(Intercept)") %>% 
      mutate(lost_new = case_when(lost_new == "Won" ~ "Reward the\nMessenger",
                                  lost_new == "Lost" ~ "Shoot the\nMessenger"),
             lost_new = fct_rev(factor(lost_new))) %>% 
      rbind(exp1) 

    # This is for non-spillover, so we have a constant number for "Other" depending on 
    # win or loss
    }  else if(group == TRUE){
      d <- df2 %>% 
        mutate(messenger_new = relevel(factor(messenger_new), ref = "Other")) %>%
        group_by(lost_new, messenger_new) %>% 
        mutate(!!sym(paste0(outcomes[i],"_other")) := case_when(messenger_new == "Other" & lost_new == "Won" ~ mean(!!sym(outcomes[i]), na.rm = T),
                                                                messenger_new == "Other" & lost_new == "Lost" ~ mean(!!sym(outcomes[i]), na.rm = T),
                                                                messenger_new == "Messenger" ~ !!sym(outcomes[i]))) %>% 
        ungroup() %>% 
        group_by(lost_new, {{fate}}) %>%
        reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "_other", "~ messenger_new")), data = pick(everything())))) %>% 
        filter(term != "(Intercept)") %>% 
        mutate(lost_new = case_when(lost_new == "Won" ~ "Reward the\nMessenger",
                                    lost_new == "Lost" ~ "Shoot the\nMessenger"),
               lost_new = fct_rev(factor(lost_new))) %>% 
        rbind(exp1) 
    } 
    
    # Change y-axis label and limit/breaks depending on the outcome measure in question.
    if(outcomes[i] == "dictator_all"){
      yaxis = "Cents Given"
    } else if(outcomes[i] == "spin_all"){
      yaxis = "% Choose Human"
    } else if(outcomes[i] == "spin_all"){
      yaxis = "% Cooperate"
    } else if(outcomes[i] %in% c("likePartner_1", "likePartner_2", "likePartner_3", "likePartner_4")){
      yaxis = "Rating"
    } else if(outcomes[i] %in% c("behavior_all", "likePartner")){
      yaxis = "Index"
    }
    
    if(outcomes[i] %in% c("spin_all", "pd_all")){
      lim = c(-.4, .4)
      breaks = round(seq(-.4, .4, .1), digits = 2)
    } else if(outcomes[i] == "behavior_all"){
      lim = c(-.5, .5)
      breaks = round(seq(-.5, .5, .1), digits = 2)
    } else if(outcomes[i] == "dictator_all"){
      lim = c(-6,6)
      breaks = seq(-6, 6, 1)
      if(as_label(enquo(fate)) == "Instructions1"){
        lim = c(-8,8)
        breaks = seq(-8, 8, 1)
      }
    } else if(outcomes[i] %in% c("likePartner_1", "likePartner_2", "likePartner_3", "likePartner_4")){
      lim = round(c(-.7, 1), 2)
      breaks = round(seq(-.7, 1, .1), 2)
    } else if (outcomes[i] == "likePartner"){
      lim = c(-.1, .13)
      breaks = seq(-.1, .1, .05)
    } 
    
    # Now, graph the thing
    g <- ggplot(d, aes(x = lost_new, y = estimate)) +
      geom_bar(stat="identity", fill="lightgray", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                    position=position_dodge(.9)) +
      facet_wrap(vars({{fate}}), nrow = 1)+
      scale_y_continuous(limits = lim, breaks = breaks)+
      theme_few() + 
      geom_hline(yintercept = 0) +
      labs(title = outcomes_names[i], x= NULL, y = yaxis) +
      theme(axis.text.x = element_text(colour = c("red", "green4")))
    
    
    if(group == TRUE){
      name = paste0("study2_grouped_main_", outcomes[i])
      caption_additional = " (assuming no spill-over from messenger alignment in Task 1 for response towards non-messengers)"
    } else if(as_label(enquo(fate)) == "Instructions1"){
      name = paste0("study2_main_fivecent_", outcomes[i])
      caption_additional = " (including .05 and 50 cent differences)"
    } else if (group == FALSE){
      name = paste0("study2_main_", outcomes[i])
      caption_additional = ""
    }
  
    
    if(i == 4){
      footnote_additional = paste0(" The p-values of TMB (RTM - STM) are ", behavior_tmb_pvalues[1], ", ", 
                                   behavior_tmb_pvalues[2], ", ", behavior_tmb_pvalues[3],
                                   ", and ", behavior_tmb_pvalues[4], ", respectively, for the Shared, Unrelated, Opposite Fate conditions, and Experiment 1.")
      footnote[i] = paste0(footnote[i], footnote_additional)
    } else if (i == 9){
      footnote_additional = paste0(" The p-values of TMB (RTM - STM) are ", attitude_tmb_pvalues[1], ", ", 
                                   attitude_tmb_pvalues[2], ", ",
                                   attitude_tmb_pvalues[3], ", and ", attitude_tmb_pvalues[4], ", respectively, for the Shared, Unrelated, Opposite Fate conditions, and Experiment 1.")
      footnote[i] = paste0(footnote[i], footnote_additional)
      
    }
    
    ggsave(g, file = paste0(save_path, "figures/", name, ".png"),
           height = 5.5, width = 10)
    
    tex <- paste0("\\renewcommand{\\baselinestretch}{1.25}%
\\begin{figure}[!t]%
  \\centering
  \\includegraphics[width=1.0\\textwidth]{figures/", name, ".png}
  \\caption{", caption[i], caption_additional, ". 
  \\textit{Note: OLS regression with robust standard errors, with error bars representing 95\\% confidence intervals. ", footnote[i], "}}
  \\label{fig:", name, "}
\\end{figure}%
\\renewcommand{\\baselinestretch}{1.67}%")
    
    cat(tex, sep = "\n", file = paste0(save_path, "figures_tex/", name, ".tex"))
    
  }
}



study2_figures(footnote = footnote_all, caption = caption_all, fate = Instructions2)
study2_figures(group = TRUE, footnote = footnote_all, caption = caption_all, fate = Instructions2)
study2_figures(footnote = footnote_all, caption = caption_all, fate = Instructions1)


# *Difference in Means (Tables 31-34) -------------------------------------
outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all", 
              "likePartner_1", "likePartner_2", "likePartner_3", "likePartner_4", "likePartner")
outcomes_names <- c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index",
                    "Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index")
type = c(rep("behavioral", 3), "", rep("attitude", 4), "")


for(i in seq_along(outcomes)){
  
  
  summary_table <- df2 %>% 
    group_by(Instructions2, messenger_lost) %>% 
    summarise("outcome" = mean(!!sym(outcomes[i]), na.rm = T)) %>% 
    arrange(desc(Instructions2)) %>% 
    rename(" " = Instructions2) %>% 
    mutate(outcome = round(outcome, digits = 2)) %>% 
    pivot_wider(names_from = messenger_lost, values_from = "outcome")
  
  make_kable(paste0("study2_", outcomes[i]), summary_table, 
             caption = paste0("Mean outcomes of the ", outcomes_names[i], " ", type[i], " measures by Task 1 win/loss, 
                              presence of a messenger, and linked fate, Study 2"))
}



# Misc --------------------------------------------------------------------

# *Correlation Index - null results -------------------------------------------------------

df1 %>% 
  group_by(lost_new, messenger_new) %>% 
  summarise(corr = cor(behavior_all, likeOther, use = "complete.obs")) %>% 
  pivot_wider(names_from = messenger_new, values_from = corr) %>% 
  rename(" "  = lost_new) %>% 
  mutate(Type = "Experiment 1")

ggplot(df1, aes(x = behavior_all, y = likeOther)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~messenger_lost) + 
  theme_few()+
  labs(x = "Behavior Index", y = "Attitude Index", title = "Experiment 1")


map(unique(df2$Instructions2), ~df2 %>% 
  group_by(df2$.x, messenger_new, lost_new) %>% 
  summarise(corr = cor(behavior_all, likePartner, use = "complete.obs")) %>% 
  pivot_wider(names_from = messenger_new, values_from = corr) %>% 
    rename(" "  = lost_new) %>% 
    mutate(Type = .x))

map(unique(df2$Instructions2), ~ggplot(df2 %>% filter(Instructions2 == .x), 
                                       aes(x = behavior_all, y = likePartner)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~messenger_lost) + 
  theme_few() + 
  labs(title = .x, x = "Behavior Index", y = "Attitude Index"))



# *Standardized means with p-values ---------------------------------------------------------------
outcomes1 <- c("behavior_all","likeOther","dictator_all", "spin_all"  ,   "pd_all"     ,
              "trust"    ,    "nice"       ,  "likeable"    ,
              "generous"        )
outcomes2 <- c("behavior_all","likePartner","dictator_all", "spin_all"  ,   "pd_all"     ,
              "likePartner_1"    ,    "likePartner_2"       ,  "likePartner_3"    ,
              "likePartner_4"        )
outcomes3 <- c("behavior_all", "likePartner",
               "behavior_all", "likePartner",
               "behavior_all", "likePartner")
outcomes_names <- c("\\makecell{Behavioral\\\\Index}","\\makecell{Attitudes\\\\Index}",
                    "\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                    "\\makecell{Prisoner's\\\\Dilemma}", 
                               "Trustworthy", "Nice", "Likeable", "Generous")

df2 %>% 
  group_by(Instructions2) %>% 
  summarise(sd = sd(likePartner, na.rm = T))

# This section was originally called Cohen's D?
# Something about how psychologists interpret the relative magnitude of coefficients
# by standardizing the betas by the SD
make_cohens <- function(fate, refresh){
  if(fate == "Experiment 1"){
    outcomes <- outcomes1
    base_list <- map(outcomes,
                     ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1,
                                return_vcov = TRUE))
    footnote_additional = ""
    sd <- map_dbl(outcomes, ~sd(df1[[.x]], na.rm = TRUE))
    
    
  } else{
    outcomes <- outcomes2
    base_list <- map(outcomes,
                     ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df2,
                                subset = Instructions2 == fate,
                                return_vcov = TRUE))
    footnote_additional = "Positive coefficients for STM actually indicate a \\textit{reward the messenger} effect."
    sd <- map_dbl(outcomes, ~sd(df2[[.x]], na.rm = TRUE))
    
  }
  

# RTM 

rtm <- map_dbl(1:length(outcomes), ~base_list[[.x]]$coefficients[3])
rtm_p <- map_dbl(1:length(outcomes), ~base_list[[.x]]$p.value[3])
rtm_stars <- rep(0, length(outcomes))
for(i in seq_along(rtm_p)){
  if(rtm_p[i] < .001){
    rtm_stars[i] = "***"
  } else if(rtm_p[i] < .01){
    rtm_stars[i] = "**"
  } else if(rtm_p[i] < .05){
    rtm_stars[i] = "*"
  } else{
    rtm_stars[i] = " "
  }
}

rtm_rounded_p <- rep(0,length(outcomes))
for(i in seq_along(rtm_p)){
  if(rtm_p[i] < .001){
    rtm_rounded_p[i] = "< .001"
  } else if(rtm_p[i] < .01){
    rtm_rounded_p[i] = "< .01"
  } else{
    rtm_rounded_p[i] = as.character(round(rtm_p[i], digits = 2))
  }
}

# STM 

stm <- map_dbl(1:length(outcomes), ~(base_list[[.x]]$coefficients[3]+base_list[[.x]]$coefficients[4]))
stm_p <- rep(0, length(outcomes))
for(i in seq_along(base_list)){
  stm_p[i] <-  as.numeric(summary(glht(base_list[[i]], linfct = c("messenger + lostT1:messenger = 0")))$test$pvalues)
}
stm_stars <- rep(0, length(outcomes))
for(i in seq_along(stm_p)){
  if(stm_p[i] < .001){
    stm_stars[i] = "***"
  } else if(stm_p[i] < .01){
    stm_stars[i] = "**"
  } else if(stm_p[i] < .05){
    stm_stars[i] = "*"
  } else{
    stm_stars[i] = " "
  }
}

stm_rounded_p <- rep(0,length(outcomes))
for(i in seq_along(stm_p)){
  if(stm_p[i] < .001){
    stm_rounded_p[i] = "< .001"
  } else if(stm_p[i] < .01){
    stm_rounded_p[i] = "< .01"
  } else{
    stm_rounded_p[i] = as.character(round(stm_p[i], digits = 2))
  }
}

# Standardize Things

rtm_sd <- round(rtm/sd, digits = 2)
stm_sd <- round(stm/sd, digits = 2)

rtm_all <- paste0(rtm_sd, rtm_stars)
stm_all <- paste0(stm_sd, stm_stars)



# TMB

tmb_sd <- rtm_sd-stm_sd
tmb_p <- map_dbl(1:9, ~base_list[[.x]]$p.value[4])

tmb_stars <- rep(0, length(outcomes))
for(i in seq_along(tmb_p)){
  if(tmb_p[i] < .001){
    tmb_stars[i] = "***"
  } else if(tmb_p[i] < .01){
    tmb_stars[i] = "**"
  } else if(tmb_p[i] < .05){
    tmb_stars[i] = "*"
  } else{
    tmb_stars[i] = " "
  }
}


tmb_rounded_p <- rep(0,length(outcomes))
for(i in seq_along(tmb_p)){
  if(tmb_p[i] < .001){
    tmb_rounded_p[i] = "< .001"
  } else if(tmb_p[i] < .01){
    tmb_rounded_p[i] = "< .01"
  } else{
    tmb_rounded_p[i] = as.character(round(tmb_p[i], digits = 2))
  }
}

tmb_all <- paste0(tmb_sd, tmb_stars)



data <- as.data.frame(list("Standardized RTM" = rtm_all, "p1" = rtm_rounded_p,
                           "Standardized STM" = stm_all, "p2" = stm_rounded_p,
                           "Standardized TMB" = tmb_all, "p3" = tmb_rounded_p),
                      row.names = outcomes_names) %>% 
  rename("Standardized RTM" = Standardized.RTM,
         "Standardized STM" = Standardized.STM,
         "Standardized TMB" = Standardized.TMB,
         "$p$-values " = p1,
         "$p$-values" = p2,
         "$p$-values  " = p3) %>% 
  t()

return(data)

if(fate == "Experiment 1"){
  fate_name = "Study 1"
} else if (fate == "Unrelated Fate"){
  fate_name = "Study 2, Unrelated Fate"
}else if (fate == "Shared Fate"){
  fate_name = "Study 2, Shared Fate"
}else if (fate == "Opposite Fate"){
  fate_name = "Study 2, Opposite Fate"
}
make_kable(paste0("cohens_d_", fate), data, scalebox = .7, refresh = refresh,
           caption = paste0("Summary of Magnitudes in SD and Statistical Significance Levels, ", fate_name),
           footnote = paste0("***$p < .001$; **$p < .01$; *$p < .05$. ", footnote_additional))

return(data)
}

# turned refresh off because you have to edit the \\hlines in Latex
make_cohens("Experiment 1", refresh = FALSE)
cohen_unrelated <- make_cohens("Unrelated Fate", refresh = F)
cohen_shared <- make_cohens("Shared Fate", refresh = F)
cohen_opposite <- make_cohens("Opposite Fate", refresh = F)

# Combine all three fates for Experiment 2 together
cohens_all <- as.data.frame(c(as.data.frame(cohen_shared)[1:2],
                      as.data.frame(cohen_unrelated)[1:2],
                      as.data.frame(cohen_opposite)[1:2])) %>% 
  `colnames<-`(c("\\makecell{Behavioral\\\\Index}", "\\makecell{Attitudes\\\\Index}",
                 "\\makecell{Behavioral\\\\Index}", "\\makecell{Attitudes\\\\Index}",
                 "\\makecell{Behavioral\\\\Index}", "\\makecell{Attitudes\\\\Index}")) %>% 
  `rownames<-`(c("Standardized RTM", "$p$-values",
                 "Standardized RTM ", "$p$-values ",
                 "Standardized TMB  ", "$p$-values  "))

# I don't think the add_header function works at all. Don't use right now,
# will fix later maybe?
make_kable("cohens_all", cohens_all, 
           add_header = c("Shared Fate" = 1:2,
                          "Unrelated Fate" = 3:4,
                          "Opposite Fate" = 5:6),
           caption = "Summary of Magnitudes in SD and Statistical Significance Levels, Study 2",
           refresh = F)
  
  
