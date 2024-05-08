# Load libraries and data -------------------------------------------------
library(haven)
library(tidyverse)
library(broom)
library(estimatr)
library(ggthemes)
library(gridExtra)
library(multcomp)
library(car)
library(texreg)
library(factoextra)
library(AER)
library(kableExtra)

devtools::source_gist("230a9f9f6a1671954800d93ea4124060")

options(knitr.table.format = "latex")

save_path <- "../NHB Submission/latex/"

outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all", 
              "trust", "nice", "likeable", "generous", "attitude_all")
outcomes_names <- c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index",
                    "Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index")
outcomes_names_labeled <- c("A. Dictator Game", "B. Spin the Wheel", "C. Prisoner's Dilemma", "D. Behavioral Index",
                            "A. Trustworthy", "B. Nice", "C. Likeable", "D. Generous", "E. Attitudes Index")


# *Study 1 ----------------------------------------------------------------
# Balance tests -----------------------------------------------------------

## By Messenger
with(df1, t.test(age[messenger == 1], age[messenger == 0]))
chisq.test(df1$sex, df1$messenger, correct = FALSE)
chisq.test(df1$pid, df1$messenger, correct = FALSE)
chisq.test(df1$leaners, df1$messenger, correct = FALSE)
chisq.test(df1$strongPID, df1$messenger, correct = FALSE)
with(df1, t.test(ideology[messenger == 1], ideology[messenger == 0]))
with(df1, t.test(education[messenger == 1], education[messenger == 0]))
chisq.test(df1$race, df1$messenger, correct = FALSE)

## By win/lose
with(df1, t.test(age[lostT1 == 1], age[lostT1 == 0]))
chisq.test(df1$sex, df1$lostT1, correct = FALSE)
chisq.test(df1$pid, df1$lostT1, correct = FALSE)
chisq.test(df1$leaners, df1$lostT1, correct = FALSE)
chisq.test(df1$strongPID, df1$lostT1, correct = FALSE)
with(df1, t.test(ideology[lostT1 == 1], ideology[lostT1 == 0]))
with(df1, t.test(education[lostT1 == 1], education[lostT1 == 0]))
chisq.test(df1$race, df1$lostT1, correct = FALSE)

## By Task
with(df1, t.test(age[counting == 1], age[counting == 0]))
chisq.test(df1$sex, df1$counting, correct = FALSE)
chisq.test(df1$pid, df1$counting, correct = FALSE)
chisq.test(df1$leaners, df1$counting, correct = FALSE)
chisq.test(df1$strongPID, df1$counting, correct = FALSE)
with(df1, t.test(ideology[counting == 1], ideology[counting == 0]))
with(df1, t.test(education[counting == 1], education[counting == 0]))
chisq.test(df1$race, df1$counting, correct = FALSE)

## By emotional message
with(df1, t.test(age[emotionalMsg == 1], age[emotionalMsg == 0]))
chisq.test(df1$sex, df1$emotionalMsg, correct = FALSE)
chisq.test(df1$pid, df1$emotionalMsg, correct = FALSE) # p < .05
chisq.test(df1$leaners, df1$emotionalMsg, correct = FALSE)
chisq.test(df1$strongPID, df1$emotionalMsg, correct = FALSE) # p < .05
with(df1, t.test(ideology[emotionalMsg == 1], ideology[emotionalMsg == 0]))
with(df1, t.test(education[emotionalMsg == 1], education[emotionalMsg == 0]))
chisq.test(df1$race, df1$emotionalMsg, correct = FALSE)

# Scale validations --------------------------------------------------------
## PCA for Attitudes Index
attitudes_index <- df1 |> 
  select(trust, nice, likeable, generous) |> 
  filter(!is.na(trust) & !is.na(nice) & !is.na(likeable) & !is.na(generous))

attitudes.pca <- prcomp(attitudes_index, scale = TRUE)

get_eigenvalue(attitudes.pca)
fviz_eig(attitudes.pca)

## Scale: positive attitudes towards the messenger
## FIRST: 0.91
cronbach.alpha(df1 %>% select(matches("likeMessengerFirst_.*")), na.rm = TRUE)
## AFTER: 0.95
cronbach.alpha(df1 %>% select(matches("likeMessengerAfter_.*")), na.rm = TRUE)

## Scale: positive attitudes towards the non-messenger
## \alpha: 0.94
cronbach.alpha(df1 %>% select(matches("likePartner_.*")), na.rm = TRUE)

## Scale: Just World: .92
cronbach.alpha(df1 %>% select(matches("justWorld_\\d")), na.rm = TRUE)
## Scale: Emotion Regulation = .90 
cronbach.alpha(df1 %>% select(matches("emReg_\\d")), na.rm = TRUE)
## Scale: Luck Belief = .87
cronbach.alpha(df1 %>% select(matches("luck_\\d")), na.rm = TRUE)



# Table A1. Behavior regression (for Figure 2) ----------------------

base_list <- map(outcomes[1:4],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1))

list <- base_list

# STM effect
pvalues1 <- rep(0, 4)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], c("messenger + lostT1:messenger = 0")))$test$pvalues)
}

# RTM effect
pvalues2 <- rep(0,4)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[3]
}

# RTM = -STM?
pvalues3 <- rep(0, 4)
for(i in seq_along(list)){
  pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
}

# TMB
joint_hypothesis <- c("messenger = 0", "lostT1:messenger = 0")

pvalues4 <- rep(0,4)
for(i in seq_along(list)){
  pvalues4[i] <-  linearHypothesis(list[[i]], joint_hypothesis, test = "F")$`Pr(>F)`[2]
}

make_texreg("behavior_regression", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2 \\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                   "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
            caption = "Regressions of behavioral measures on losing in task 1, being paired with the messenger, and their interaction", 
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.",
                                "\\item[\\hspace{-5mm}] %stars."))

s1_behavior_pvalues3 <- ifelse(round(pvalues3, digits = 2) == 0, "0.00", round(pvalues3, digits = 2)) #RTM = -STM
s1_behavior_pvalues4 <- ifelse(round(pvalues4, digits = 2) == 0, "0.00", round(pvalues4, digits = 2)) #TMB


# Table A2. Emotionality of message ------------------------------------------

base_list <- map(outcomes[1:4],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*emotionalMsg")), data = df1))

list <- base_list
make_texreg("behavior_regression_emotional", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "emotionalMsg" = "Emotional Message",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:emotionalMsg" = "Lost x Emotional",
                                   "messenger:emotionalMsg" = "Messenger x Emotional",
                                   "lostT1:messenger:emotionalMsg" = "Lost x Messenger x Emotional",
                                   "(Intercept)" = "Constant"),
            caption = "Regressions of behavioral measures on losing in task 1, being paired with the messenger, emotionality of message, and their interaction", 
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.",
                                "\\item[\\hspace{-5mm}] %stars."))
# Table A3. Type of task -------------------------------------------
base_list <- map(outcomes[1:4],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*counting")), data = df1))

list <- base_list

make_texreg("behavior_regression_counting", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "counting" = "Counting Task",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:counting" = "Lost x Counting",
                                   "messenger:counting" = "Messenger x Counting",
                                   "lostT1:messenger:counting" = "Lost x Messenger x Counting",
                                   "(Intercept)" = "Constant"),
            caption = "Regressions of behavioral measures on losing in task 1, being paired with the messenger, type of task, and their interaction", 
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.",
                                "\\item[\\hspace{-5mm}] %stars."))


# Table A4. Attitude regression (for Figure 3) ----------------------

base_list <- map(outcomes[5:9],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1))

list <- base_list

# STM effect
pvalues1 <- rep(0, 5)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], c("messenger + lostT1:messenger = 0")))$test$pvalues)
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

# TMB
joint_hypothesis <- c("messenger  = 0", "lostT1:messenger = 0")

pvalues4 <- rep(0,5)
for(i in seq_along(list)){
  pvalues4[i] <-  linearHypothesis(list[[i]], joint_hypothesis, test = "F")$`Pr(>F)`[2]
}

make_texreg("attitude_regression", list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", 
                                   "\\makecell{Attitudes\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                   "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
            caption = "Regressions of attitude measures on losing in task 1, being paired with the messenger, and their interaction",
            scalebox = .88,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.",
                                "\\item[\\hspace{-5mm}] %stars."))


s1_attitude_pvalues3 <- ifelse(round(pvalues3, digits = 2) == 0, "0.00", round(pvalues3, digits = 2)) #RTM = -STM
s1_attitude_pvalues4 <- ifelse(round(pvalues4, digits = 2) == 0, "0.00", round(pvalues4, digits = 2)) #TMB



# Figures 2 and 3 ----------------------------------------------------------------


caption_all <- map_chr(outcomes_names, ~paste0("Difference in the DV of the ", .x, " for messengers and non-messengers 
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
  "In the attitudes index, the DV is calculated by averaging the ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.")

all_list = list()

# First creates individual figures for each of the outcome variables
for(i in 1:9){
  d <- df1 %>% 
    group_by(lostT1) %>% 
    reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "~ messenger")), data = pick(everything())))) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(lostT1 = case_when(lostT1 == 0 ~ "Reward the\nMessenger",
                              lostT1 == 1 ~ "Shoot the\nMessenger")) %>% 
    mutate(lostT1 = fct_rev(lostT1)) 
  
  # Change y-axis label and limit/breaks depending on the outcome measure in question.
  if(outcomes[i] == "dictator_all"){
    yaxis = "Cents Given"
  } else if(outcomes[i] == "spin_all"){
    yaxis = "% Choose Human"
  } else if(outcomes[i] == "spin_all"){
    yaxis = "% Cooperate"
  } else if(outcomes[i] %in% c("trust", "nice", "likeable", "generous")){
    yaxis = "Rating"
  } else if(outcomes[i] %in% c("behavior_all", "attitude_all")){
    yaxis = "Index"
  }
  
  if(outcomes[i] %in% c("spin_all", "pd_all", "behavior_all")){
    lim = c(-.3, .35)
    breaks = round(seq(-.3, .3, .1), digits = 2)
  } else if(outcomes[i] == "dictator_all"){
    lim = c(-4,4.55)
    breaks = seq(-4, 4, 1)
  } else if(outcomes[i] %in% c("trust", "nice", "likeable", "generous")){
    lim = c(-.5, .5)
    breaks = seq(-.5, .5, .1)
  } else if(outcomes[i] == "attitude_all"){
    lim = c(-.1,.1)
    breaks = seq(-.1, .1, .05)
  }
  
  
  
  g <- ggplot(d, aes(x = lostT1, y = estimate)) +
    geom_bar(stat="identity", fill="lightgray", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                  position=position_dodge(.9)) +
    scale_y_continuous(limits = lim, breaks = breaks)+
    theme_few() + 
    geom_hline(yintercept = 0) +
    labs(title = outcomes_names_labeled[i], x= NULL, y = yaxis) + 
    theme(axis.text.x = element_text(colour = c("red", "green4")))
  
  
  #   ggsave(g, file = paste0(save_path, "figures/study1", outcomes[i], ".png"),
  #          height = 5.5, width = 4)
  #   
  #   tex <- paste0("\\renewcommand{\\baselinestretch}{1.25}%
  # \\begin{figure}[!t]%
  #   \\centering
  #   \\captionsetup{width=0.5\\linewidth}
  #   \\includegraphics[width=0.5\\linewidth]{figures/study1", outcomes[i], ".png}
  #   \\caption{", caption_all[i], ". 
  #   \\textit{Note: OLS regression with robust standard errors, with error bars representing 95\\% confidence intervals. ", footnote_all[i], "}}
  #   \\label{fig:study1", outcomes[i], "}
  # \\end{figure}%
  # \\renewcommand{\\baselinestretch}{1.67}%")
  #   
  #   cat(tex, sep = "\n", file = paste0(save_path, "figures_tex/study1_", outcomes[i], ".tex"))
  
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
    footnote <- "In the dictator game (Panel A), the dependent variable (DV) is giving up to 50 cents to the partner. 
                 In the spin the wheel task (Panel B), the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                 In the prisoner’s dilemma (Panel C), the DV is choosing cooperation. 
                 In the behavioral index (Panel D), the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma."
    footnote_addition <- paste0(" The p-values of the test that $RTM = -STM$ are ", s1_behavior_pvalues3[1], ", ", s1_behavior_pvalues3[2], 
                                ", ", s1_behavior_pvalues3[3], ", and ", s1_behavior_pvalues3[4], 
                                ", respectively, for each facet and the index. ", "The p-values of the messenger bias are ",
                                s1_behavior_pvalues4[1], ", ", s1_behavior_pvalues4[2], ", ", s1_behavior_pvalues4[3], ", and ", 
                                s1_behavior_pvalues4[4], ", respectively, for each facet and the index.")
    
  } else if (str_detect(deparse(substitute(object)), "attitude")){
    name <- "attitude"
    footnote <- "The trustworthy (Panel A), nice (Panel B), likeable (Panel C), and generous (Panel D) dependent variables asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index (Panel E), the DV is calculated by averaging the ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1."
    footnote_addition <- paste0(" The p-values of the test that $RTM = -STM$ are ", s1_attitude_pvalues3[1], ", ", s1_attitude_pvalues3[2], 
                                ", ", s1_attitude_pvalues3[3], ", ", s1_attitude_pvalues3[4], ", and ", s1_attitude_pvalues3[5], 
                                ", respectively, for each facet and index.", " The p-values of the messenger bias are ",
                                s1_attitude_pvalues4[1], ", ", s1_attitude_pvalues4[2], ", ", s1_attitude_pvalues4[3], ", ",
                                s1_attitude_pvalues4[4], ", and ", s1_attitude_pvalues4[5],
                                ", respectively, for each facet and the index.")
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




# Table B1. Sample Demographics -----------------------------------------------------

sample1_df <- df1 |> 
  select(dictator_all, spin_all, pd_all,
         trust, nice, likeable, generous,
         "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld")

table(exp1_sample$rowname)

exp1_sample <- as.data.frame(round(apply(sample1_df, 2, mean, na.rm = TRUE), digits = 3)) %>% 
  cbind(round(apply(sample1_df, 2, sd, na.rm = TRUE), digits = 3)) %>% 
  cbind(round(apply(sample1_df, 2, min, na.rm = TRUE), digits = 3)) %>% 
  cbind(round(apply(sample1_df, 2, max, na.rm = TRUE), digits = 3)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = case_when(rowname == "dictator_all" ~ "Dictator Game Outcome",
                             rowname == "spin_all" ~ "Spin the Wheel Outcome",
                             rowname == "pd_all" ~ "Prisoner's Dilemma Outcome",
                             rowname == "trust" ~ "Trustworthiness Outcome",
                             rowname == "nice" ~ "Niceness Outcome",
                             rowname == "likeable" ~ "Likeable Outcome",
                             rowname == "generous" ~ "Generous Outcome",
                             rowname == "age" ~ "Age",
                             rowname == "female" ~ "Female",
                             rowname == "ideology" ~ "Ideology",
                             rowname == "education" ~ "Education",
                             rowname == "employed" ~ "Employed",
                             rowname == "white" ~ "White",
                             rowname == "hispNew" ~ "Hispanic",
                             rowname == "believeLuck" ~ "Belief in Luck Scale",
                             rowname == "emotionReg" ~ "Emotion Regulation Scale",
                             rowname == "justWorld" ~ "Just World Scale"))
                               

make_kable(exp1_sample, col.names = c("Variable", "Mean", "SD", "Min", "Max"),
           caption = "Sample demographics, Experiment 1")
  
  
# Table B2 ("Table 1 w/ logit"). Behavior regressions w/ logit ---------------------------------------

tobit_dictator <- tobit(dictator_all ~ lostT1*messenger, left = 0, right = 50, data = df1, cluster = ResponseId)
summ_tobit_dictator <- summary(tobit_dictator)

logit_spin <- glm(spin_all ~ lostT1*messenger, family = binomial(link = "logit"),
                  data = df1)
summ_logit_spin <- coeftest(logit_spin, vcov = vcovHC, type = "HC2")

logit_pd <- glm(pd_all ~ lostT1*messenger, family = binomial(link = "logit"),
                data = df1)
summ_logit_pd <- coeftest(logit_pd, vcov = vcovHC, type = "HC2")

make_texreg("behavior_logit_regression", list(tobit_dictator, logit_spin, logit_pd),
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            caption = "Generalized linear regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction", 
            override.se = list(summ_tobit_dictator$coefficients[,2],
                               summ_logit_spin[,2],
                               summ_logit_pd[,2]),
            override.pvalues = list(summ_tobit_dictator$coefficients[,4],
                                    summ_logit_spin[,4],
                                    summ_logit_pd[,4]),
            multiple.tasks = "glm",
            refresh = TRUE,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} Tobit regression with robust standard errors in parentheses for the first column (censoring at 0 and 50 cents),
                                and logistic regressions with robust standard errors in parentheses for the second and third column.
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation.",
                                "\\item[\\hspace{-5mm}] %stars."))



# Table B3 ("Table 2 w/ logit"). Emotionality w/ logit ---------------------------------------

tobit_dictator <- tobit(dictator_all ~ lostT1*messenger*emotionalMsg, left = 0, right = 50, data = df1, cluster = ResponseId)
summ_tobit_dictator <- summary(tobit_dictator)

logit_spin <- glm(spin_all ~ lostT1*messenger*emotionalMsg, family = binomial(link = "logit"),
                  data = df1)
summ_logit_spin <- coeftest(logit_spin, vcov = vcovHC, type = "HC2")

logit_pd <- glm(pd_all ~ lostT1*messenger*emotionalMsg, family = binomial(link = "logit"),
                data = df1)
summ_logit_pd <- coeftest(logit_pd, vcov = vcovHC, type = "HC2")

make_texreg("behavior_logit_regression_emotional", list(tobit_dictator, logit_spin, logit_pd),
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "emotionalMsg" = "Emotional Message",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:emotionalMsg" = "Lost x Emotional",
                                   "messenger:emotionalMsg" = "Messenger x Emotional",
                                   "lostT1:messenger:emotionalMsg" = "Lost x Messenger x Emotional",
                                   "(Intercept)" = "Constant"),
            caption = "Generalized linear regressions of the behavioral measures on losing in task 1, being paired with the messenger, emotionality of message, and their interaction", 
            scalebox = .9,
            override.se = list(summ_tobit_dictator$coefficients[,2],
                               summ_logit_spin[,2],
                               summ_logit_pd[,2]),
            override.pvalues = list(summ_tobit_dictator$coefficients[,4],
                                    summ_logit_spin[,4],
                                    summ_logit_pd[,4]),
            multiple.tasks = "glm",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} Tobit regression with robust standard errors in parentheses for the first column (censoring at 0 and 50 cents),
                                and logistic regressions with robust standard errors in parentheses for the second and third column.
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation.",
                                "\\item[\\hspace{-5mm}] %stars."))



# Table B4 ("Table 3 w/ logit"). Type of task w/ logit ---------------------------------------

tobit_dictator <- tobit(dictator_all ~ lostT1*messenger*counting, left = 0, right = 50, data = df1, cluster = ResponseId)
summ_tobit_dictator <- summary(tobit_dictator)

logit_spin <- glm(spin_all ~ lostT1*messenger*counting, family = binomial(link = "logit"),
                  data = df1)
summ_logit_spin <- coeftest(logit_spin, vcov = vcovHC, type = "HC2")

logit_pd <- glm(pd_all ~ lostT1*messenger*counting, family = binomial(link = "logit"),
                data = df1)
summ_logit_pd <- coeftest(logit_pd, vcov = vcovHC, type = "HC2")

make_texreg("behavior_logit_regression_counting", list(tobit_dictator, logit_spin, logit_pd),
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "counting" = "Counting Task",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:counting" = "Lost x Counting",
                                   "messenger:counting" = "Messenger x Counting",
                                   "lostT1:messenger:counting" = "Lost x Messenger x Counting",
                                   "(Intercept)" = "Constant"),
            caption = "Generalized linear regressions of the behavioral measures on losing in task 1, being paired with the messenger, type of task, and their interaction", 
            override.se = list(summ_tobit_dictator$coefficients[,2],
                               summ_logit_spin[,2],
                               summ_logit_pd[,2]),
            override.pvalues = list(summ_tobit_dictator$coefficients[,4],
                                    summ_logit_spin[,4],
                                    summ_logit_pd[,4]),
            multiple.tasks = "glm",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} Tobit regression with robust standard errors in parentheses for the first column (censoring at 0 and 50 cents),
                                and logistic regressions with robust standard errors in parentheses for the second and third column.
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation.",
                                "\\item[\\hspace{-5mm}] %stars."))


# Table B5. ("Table 1 w/ demographics"). Behavior regression w/ demographics ----------------------------------------------------------------

robust_list <- map(outcomes[1:4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))


list <- robust_list

# STM effect
pvalues1 <- rep(0, 4)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], c("messenger + lostT1:messenger = 0")))$test$pvalues)
}

# RTM effect
pvalues2 <- rep(0,4)
for(i in seq_along(list)){
  pvalues2[i] <-  list[[i]]$p.value[3]
}

# RTM = -STM?
pvalues3 <- rep(0, 4)
for(i in seq_along(list)){
  pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
}

# TMB
joint_hypothesis <- c("messenger = 0", "lostT1:messenger = 0")

pvalues4 <- rep(0,4)
for(i in seq_along(list)){
  pvalues4[i] <-  linearHypothesis(list[[i]], joint_hypothesis, test = "F")$`Pr(>F)`[2]
}

make_texreg("behavior_regression_demographic", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "age" = "Age",
                                   "female" = "Female",
                                   "ideology" = "Ideology",
                                   "education" = "Education",
                                   "employed" = "Employed",
                                   "white" = "White",
                                   "hispNew" = "Hispanic",
                                   "believeLuck" = "Beliefs in luck",
                                   "emotionReg" = "Emotion regulation",
                                   "justWorld" = "Just world",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2 \\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                   "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction (including demographic controls)", 
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))

# Table B6. ("Table 2 w/ demographics"). Emotionality w/ demographics --------
robust_list <- map(outcomes[1:4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger*emotionalMsg"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- robust_list
make_texreg("behavior_regression_emotional_demographic", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "emotionalMsg" = "Emotional Message",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:emotionalMsg" = "Lost x Emotional",
                                   "messenger:emotionalMsg" = "Messenger x Emotional",
                                   "lostT1:messenger:emotionalMsg" = "Lost x Messenger x Emotional",
                                   "age" = "Age",
                                   "female" = "Female",
                                   "ideology" = "Ideology",
                                   "education" = "Education",
                                   "employed" = "Employed",
                                   "white" = "White",
                                   "hispNew" = "Hispanic",
                                   "believeLuck" = "Beliefs in luck",
                                   "emotionReg" = "Emotion regulation",
                                   "justWorld" = "Just world",
                                   "(Intercept)" = "Constant"),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, emotionality of message, and their interaction (including demographic controls)", 
            multiple.tasks = "normal",
            scalebox = .9,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))





# Table B7. ("Table 3 w/ demographics"). Type of task w/ demographics --------

robust_list <- map(outcomes[1:4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger*counting"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))


list <- robust_list

make_texreg("behavior_regression_counting_demographic", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "counting" = "Counting Task",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:counting" = "Lost x Counting",
                                   "messenger:counting" = "Messenger x Counting",
                                   "lostT1:messenger:counting" = "Lost x Messenger x Counting",
                                   "age" = "Age",
                                   "female" = "Female",
                                   "ideology" = "Ideology",
                                   "education" = "Education",
                                   "employed" = "Employed",
                                   "white" = "White",
                                   "hispNew" = "Hispanic",
                                   "believeLuck" = "Beliefs in luck",
                                   "emotionReg" = "Emotion regulation",
                                   "justWorld" = "Just world",
                                   "(Intercept)" = "Constant"),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, type of task, and their interaction (including demographic controls)", 
            multiple.tasks = "normal",
            scalebox = .9,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))



# Table B8. ("Table 4 w/ demographics"). Attitude regression w/ demographics --------

robust_list <- map(outcomes[5:9],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- robust_list

# STM effect
pvalues1 <- rep(0, 5)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], c("messenger + lostT1:messenger = 0")))$test$pvalues)
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

# TMB
joint_hypothesis <- c("messenger  = 0", "lostT1:messenger = 0")

pvalues4 <- rep(0,5)
for(i in seq_along(list)){
  pvalues4[i] <-  linearHypothesis(list[[i]], joint_hypothesis, test = "F")$`Pr(>F)`[2]
}

make_texreg("attitude_regression_demographic", list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", 
                                   "\\makecell{Attitudes\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "age" = "Age",
                                   "female" = "Female",
                                   "ideology" = "Ideology",
                                   "education" = "Education",
                                   "employed" = "Employed",
                                   "white" = "White",
                                   "hispNew" = "Hispanic",
                                   "believeLuck" = "Beliefs in luck",
                                   "emotionReg" = "Emotion regulation",
                                   "justWorld" = "Just world",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                   "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
            caption = "Regressions of attitude measures on losing in task 1, being paired with the messenger, and their interaction (including demographic controls)",
            scalebox = .9,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))


# *Study 2 ----------------------------------------------------------------




