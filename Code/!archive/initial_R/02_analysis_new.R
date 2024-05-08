## Note: I turned off all of the "refresh" in the make_table functions because I don't
## want them syncing with the Overleaf anymore. You will have to turn them back on
## with an updated "save_path" if you want to re-sync.

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

devtools::source_gist("230a9f9f6a1671954800d93ea4124060")

 save_path <- "paper/"

outcomes <- c("dictator_all", "spin_all", "pd_all", "behavior_all", 
                         "trust", "nice", "likeable", "generous", "attitude_all")
outcomes_names <- c("Dictator Game", "Spin the Wheel", "Prisoner's Dilemma", "Behavioral Index",
                            "Trustworthy", "Nice", "Likeable", "Generous", "Attitudes Index")
outcomes_names_labeled <- c("A. Dictator Game", "B. Spin the Wheel", "C. Prisoner's Dilemma", "D. Behavioral Index",
                               "A. Trustworthy", "B. Nice", "C. Likeable", "D. Generous", "E. Attitudes Index")

# *Behavior Table A1. Main regression with and without robust ----------------------

base_list <- map(outcomes[1:4],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1))

robust_list <- map(outcomes[4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)

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
                                   "RTM = $-$STM: $2 \\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                   "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction (including demographic controls).", 
            scalebox = .9,
            multiple.tasks = "normal",
            refresh = TRUE,
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                                                  In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))

s1_behavior_pvalues3 <- ifelse(round(pvalues3, digits = 2) == 0, "0.00", round(pvalues3, digits = 2)) #RTM = -STM
s1_behavior_pvalues4 <- ifelse(round(pvalues4, digits = 2) == 0, "0.00", round(pvalues4, digits = 2)) #RTM = -STM


# *Attitude Table A2. Main regression with and without robust ----------------------

base_list <- map(outcomes[5:9],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), data = df1))

robust_list <- map(outcomes[9],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)

# STM effect
pvalues1 <- rep(0, 6)
for(i in seq_along(list)){
  pvalues1[i] <-  as.numeric(summary(glht(list[[i]], c("messenger + lostT1:messenger = 0")))$test$pvalues)
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

# TMB
joint_hypothesis <- c("messenger  = 0", "lostT1:messenger = 0")

pvalues4 <- rep(0,6)
for(i in seq_along(list)){
  pvalues4[i] <-  linearHypothesis(list[[i]], joint_hypothesis, test = "F")$`Pr(>F)`[2]
}

make_texreg("attitude_regression", list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", 
                                   "\\makecell{Attitudes\\\\Index}", "\\makecell{Attitudes\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 5), rep("Yes", 1)),
                                   "STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                   "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                   "RTM = $-$STM: $2\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                   "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
            caption = "Regression of attitude measures on losing in task 1, being paired with the messenger, and their interaction (including demographic controls).",
            scalebox = .8,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
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



# Table A5. Tobit/Logit regressions ---------------------------------------
library(AER)

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
            caption = "Generalized linear regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction.", 
            scalebox = .9,
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



# Table A8 Counting x Messenger -------------------------------------------
base_list <- map(outcomes[1:4],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*counting")), data = df1))

robust_list <- map(outcomes[4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger*counting"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)

make_texreg("behavior_regression_counting", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}",
                                   "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "counting" = "Counting Task",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:counting" = "Lost x Counting",
                                   "messenger:counting" = "Messenger x Counting",
                                   "lostT1:messenger:counting" = "Lost x Messenger x Counting",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 4), rep("Yes", 1))),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, performing the counting task, and their interaction.", 
            scalebox = .9,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))

base_list <- map(outcomes[5:9],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*counting")), data = df1))

robust_list <- map(outcomes[9],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger*counting"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)

make_texreg("attitude_regression_counting", list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", 
                                   "\\makecell{Attitudes\\\\Index}", "\\makecell{Attitudes\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "counting" = "Counting Task",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:counting" = "Lost x Counting",
                                   "messenger:counting" = "Messenger x Counting",
                                   "lostT1:messenger:counting" = "Lost x Messenger x Counting",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 5), rep("Yes", 1))),
            caption = "Regressions of the attitude measures on losing in task 1, being paired with the messenger, performing the counting task, and their interaction.", 
            scalebox = .85,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))


# Table A9 Emotional x Messenger ------------------------------------------

base_list <- map(outcomes[1:4],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*emotionalMsg")), data = df1))

robust_list <- map(outcomes[4],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger*emotionalMsg"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)
make_texreg("behavior_regression_emotional", list,
            custom.model.names = c("\\makecell{Dictator\\\\Game}", "\\makecell{Spin the\\\\Wheel}", 
                                   "\\makecell{Prisoner's\\\\Dilemma}", "\\makecell{Behavioral\\\\Index}",
                                   "\\makecell{Behavioral\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "emotionalMsg" = "Emotional Message",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:emotionalMsg" = "Lost x Emotional",
                                   "messenger:emotionalMsg" = "Messenger x Emotional",
                                   "lostT1:messenger:emotionalMsg" = "Lost x Messenger x Emotional",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 4), rep("Yes", 1))),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, receiving an emotional message, and their interaction.", 
            scalebox = .9,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the standardized scores of the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))

base_list <- map(outcomes[5:9],
                 ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger*emotionalMsg")), data = df1))


robust_list <- map(outcomes[9],
                   ~lm_robust(as.formula(
                     paste(paste0(.x, "~lostT1*messenger*emotionalMsg"),
                           "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                     data = df1))

list <- append(base_list, robust_list)

make_texreg("attitude_regression_emotional", list,
            custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", 
                                   "\\makecell{Attitudes\\\\Index}", "\\makecell{Attitudes\\\\Index}"),
            custom.coef.map = list("lostT1" = "Lost T1",
                                   "messenger" = "Messenger",
                                   "emotionalMsg" = "Emotional Message",
                                   "lostT1:messenger" = "Lost x Messenger",
                                   "lostT1:emotionalMsg" = "Lost x Emotional",
                                   "messenger:emotionalMsg" = "Messenger x Emotional",
                                   "lostT1:messenger:emotionalMsg" = "Lost x Messenger x Emotional",
                                   "(Intercept)" = "Constant"),
            custom.gof.rows = list("Demographic Controls" = c(rep("No", 5), rep("Yes", 1))),
            caption = "Regressions of the behavioral measures on losing in task 1, being paired with the messenger, receiving an emotional message, and their interaction.", 
            scalebox = .85,
            multiple.tasks = "normal",
            custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
The trustworthy, nice, likeable, and generous DVs asks respondents to rate these messenger's characteristics on a 7-point Likert scale, where a score of 1 indicates that the messenger does not have that trait at all, while a score of 7 means that a trait describes the messenger extremely well.
    In the attitudes index, the DV is calculated by averaging the standardized ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                "\\item[\\hspace{-5mm}] %stars."))

# Study 2 -----------------------------------------------------------------


# *Behavior: Main regression with and without robust ----------------------

# While these functions create regression tables, I have them to return pvalues4 
# (TMB) for use later on
study2_behavior <- function(fate){
  base_list <- map(outcomes[1:4],
                   ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), subset = Instructions2 ==fate, data = df2))
  
  robust_list <- map(outcomes[4],
                     ~lm_robust(as.formula(
                       paste(paste0(.x, "~lostT1*messenger"),
                             "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                       data = df2, subset = Instructions2 == fate))
  
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
  
  # STM = -RTM
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
  
  make_texreg(paste0("behavior_", str_to_lower(word(fate)), "_regression"), list,
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
                                     "RTM = $-$STM: $2\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                     "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
              caption = paste0("Regressions of the behavioral measures on losing in task 1, being paired with the messenger, and their interaction in the ", fate,
                               " condition (including demographic controls)."), 
              scalebox = .9,
              multiple.tasks = "normal",
              custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
                                In the dictator game, the dependent variable (DV) is giving up to 50 cents to the partner. 
                                In the spin the wheel task, the DV is choosing the partner to spin the wheel on one’s behalf instead of the computer. 
                                In the prisoner’s dilemma, the DV is choosing cooperation. 
                                In the behavioral index, the DV is calculated by averaging the scores the dictator game, spin the wheel task, and prisoner's dilemma.
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                  "\\item[\\hspace{-5mm}] %stars."))
  return(list(pvalues3, pvalues4))
}


## While these wrapper functions save into the folder, they also return the
## p-values for both RTM = -STM and TMB
unrelated_behavior_p <- study2_behavior("Unrelated Fate")
shared_behavior_p <- study2_behavior("Shared Fate")
opposite_behavior_p <- study2_behavior("Opposite Fate")



# *Attitude: Main regression with and without robust ----------------------
# While these functions create regression tables, I have them to return pvalues4
# (TMB) for use later on
study2_attitude <- function(fate){
  base_list <- map(outcomes[5:9],
                   ~lm_robust(as.formula(paste0(.x, "~lostT1*messenger")), subset = Instructions2 ==fate, data = df2))
  
  robust_list <- map(outcomes[9],
                     ~lm_robust(as.formula(
                       paste(paste0(.x, "~lostT1*messenger"),
                             "age", "female", "ideology", "education", "employed", "white", "hispNew", "believeLuck", "emotionReg", "justWorld", sep = "+")),
                       subset = Instructions2 ==fate, data = df2))
  
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
  
  # RTM = -STM
  pvalues3 <- rep(0, 6)
  for(i in seq_along(list)){
    pvalues3[i] <-  as.numeric(summary(glht(list[[i]], linfct = c("2*messenger + lostT1:messenger = 0")))$test$pvalues)
  }
  
  # TMB
  joint_hypothesis <- c("messenger  = 0", "lostT1:messenger = 0")
  
  pvalues4 <- rep(0,6)
  for(i in seq_along(list)){
    pvalues4[i] <-  linearHypothesis(list[[i]], joint_hypothesis, test = "F")$`Pr(>F)`[2]
  }
  
  make_texreg(paste0("attitude_", str_to_lower(word(fate)), "_regression"), list,
              custom.model.names = c("Trustworthy", "Nice", "Likeable", "Generous", 
                                     "\\makecell{Attitudes\\\\Index}", "\\makecell{Attitudes\\\\Index}"),
              custom.coef.map = list("lostT1" = "Lost T1",
                                     "messenger" = "Messenger",
                                     "lostT1:messenger" = "Lost x Messenger",
                                     "(Intercept)" = "Constant"),
              custom.gof.rows = list("Demographic Controls" = c(rep("No", 5), rep("Yes", 1)),
                                     "STM: $\\beta_2 + \\beta_3 = 0$ $p$-values" = pvalues1,
                                     "RTM: $\\beta_2 = 0$ $p$-values" = pvalues2,
                                     "RTM = $-$STM: $2\\beta_2+\\beta_3 = 0$ $p$-values" = pvalues3,
                                     "TMB: $\\beta_2 = 0, \\beta_3 = 0$ $p$-values" = pvalues4),
              caption = paste0("Regressions of the attitude measures on losing in task 1, being paired with the messenger, and their interaction in the ", fate,
                               " condition (including demographic controls)."), 
              multiple.tasks = "normal",
              scalebox = .8,
              custom.note = paste("\\item[\\hspace{-5mm}] \\textit{Note:} OLS regressions with robust standard errors in parentheses. 
            The trustworthy, nice, likeable, and generous DVs ask respondents to rate the messenger's trustworthiness, niceness, likeability, and generosity, respectively,
on a 7-point Likert scale. In the attitudes index, the DV is calculated by averaging the ratings of the trustworthy, nice, likeable, and generous DVs to an index ranging from 0 to 1. 
                                Demographic controls include age, gender, ideology,
                                education, employment, white, Hispanic, the belief in luck scale, the emotion regulation scale, and the just world scale.",
                                  "\\item[\\hspace{-5mm}] %stars."))
  
  return(list(pvalues3, pvalues4))
}

unrelated_attitude_p <- study2_attitude("Unrelated Fate")
shared_attitude_p <- study2_attitude("Shared Fate")
opposite_attitude_p <- study2_attitude("Opposite Fate")



# *Study 2 Main Figures ----------------------------------------------------


caption_all <- map_chr(outcomes_names, ~paste0("Difference in messenger and non-messenger ratings of the ", .x, 
                                               " by losing (STM) and winning (RTM) across Unrelated Fate, Shared Fate, Opposite Fate conditions, Study 2"))

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


# Take the returned functions from the table creations
# And store vectores of p-values for each of the fate conditions
# Arranged in Shared, Unrelated, Opposite, and Experiment 1 conditions

behavior_mag_pvalues <- c(ifelse(round(shared_behavior_p[[1]][4], digits = 2) == 0, "0.00", round(shared_behavior_p[[1]][4], digits = 2)),
                          ifelse(round(unrelated_behavior_p[[1]][4], digits = 2) == 0, "0.00", round(unrelated_behavior_p[[1]][4], digits = 2)),
                          ifelse(round(opposite_behavior_p[[1]][4], digits = 2) == 0, "0.00", round(opposite_behavior_p[[1]][4], digits = 2)),
                          s1_behavior_pvalues3[4])

behavior_tmb_pvalues <- c(ifelse(round(shared_behavior_p[[2]][4], digits = 2) == 0, "0.00", round(shared_behavior_p[[2]][4], digits = 2)),
                          ifelse(round(unrelated_behavior_p[[2]][4], digits = 2) == 0, "0.00", round(unrelated_behavior_p[[2]][4], digits = 2)),
                          ifelse(round(opposite_behavior_p[[2]][4], digits = 2) == 0, "0.00", round(opposite_behavior_p[[2]][4], digits = 2)),
                          s1_behavior_pvalues4[4])
                          

attitude_mag_pvalues <- c(ifelse(round(shared_attitude_p[[1]][5], digits = 2) == 0, "0.00", round(shared_attitude_p[[1]][5], digits = 2)),
                          ifelse(round(unrelated_attitude_p[[1]][5], digits = 2) == 0, "0.00", round(unrelated_attitude_p[[1]][5], digits = 2)),
                          ifelse(round(opposite_attitude_p[[1]][5], digits = 2) == 0, "0.00", round(opposite_attitude_p[[1]][5], digits = 2)),
                          s1_attitude_pvalues3[5])

attitude_tmb_pvalues <- c(ifelse(round(shared_attitude_p[[2]][5], digits = 2) == 0, "0.00", round(shared_attitude_p[[2]][5], digits = 2)),
                          ifelse(round(unrelated_attitude_p[[2]][5], digits = 2) == 0, "0.00", round(unrelated_attitude_p[[2]][5], digits = 2)),
                          ifelse(round(opposite_attitude_p[[2]][5], digits = 2) == 0, "0.00", round(opposite_attitude_p[[2]][5], digits = 2)),
                          s1_attitude_pvalues4[5])

fate <- quo(Instructions1)

d$Instructions1

study2_figures <- function(group = FALSE,
                           caption = NULL,
                           footnote = NULL,
                           fate){
  ## This is a wrapper function that just initiates a for loop for 
  ## each out of the outcome variables (4 behavioral, 5 attitudes)
  
  ## Any group = TRUE code is deprecated, this was assuming no spillover
  ## in negative affect towards non-messengers when losing or winning
  for(i in 1:9){
    
    # This is to compare to Experiment 1
    exp1 <- df1 %>% 
      group_by(lostT1) %>% 
      reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "~ messenger")), data = pick(everything())))) %>% 
      filter(term != "(Intercept)") %>% 
      mutate(lostT1 = case_when(lostT1 == 0 ~ "Reward the\nMessenger",
                                  lostT1 == 1 ~ "Shoot the\nMessenger"),
             lostT1 = fct_rev(factor(lostT1))) %>% 
      mutate(Instructions2 = "D. Study 1")
    
    # This is for 5 vs 50 because it's "Instructions 1"
    if(as_label(enquo(fate)) == "Instructions1"){
      d <- df2 %>% 
        group_by(lostT1, {{fate}}) %>% 
        reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "~ messenger")), data = pick(everything())))) %>% 
        filter(term != "(Intercept)") %>% 
        mutate(lostT1 = case_when(lostT1 == 0 ~ "Reward the\nMessenger",
                                    lostT1 == 1 ~ "Shoot the\nMessenger"),
               lostT1 = fct_rev(factor(lostT1))) |> 
      mutate({{fate}} := case_when({{fate}} == "$0.05 Shared Fate" ~ "B. $0.05 Shared Fate",
                                   {{fate}} == "$0.50 Shared Fate" ~ "A. $0.50 Shared Fate",
                                   {{fate}} == "Unrelated Fate" ~ "C. Unrelated Fate",
                                   {{fate}} == "$0.05 Opposite Fate" ~ "D. $0.05 Opposite Fate",
                                   {{fate}} == "$0.50 Opposite Fate" ~ "E. $0.50 Opposite Fate"))
      # This is for shared, unrelated, and opposite fate because it's "Instructions 2"
    }else if(group == FALSE){
      d <- df2 %>% 
        group_by(lostT1, {{fate}}) %>% 
        reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "~ messenger")), data = pick(everything())))) %>% 
        filter(term != "(Intercept)") %>% 
        mutate(lostT1 = case_when(lostT1 == 0 ~ "Reward the\nMessenger",
                                    lostT1 == 1 ~ "Shoot the\nMessenger"),
               lostT1 = fct_rev(factor(lostT1))) %>% 
        mutate({{fate}} := case_when({{fate}} == "Shared Fate" ~ "A. Shared Fate",
                                     {{fate}} == "Unrelated Fate" ~ "B. Unrelated Fate",
                                     {{fate}} == "Opposite Fate" ~ "C. Opposite Fate")) |> 
        rbind(exp1) 
      
      # (DEPRECATED) This is for non-spillover, so we have a constant number for "Other" depending on 
      # win or loss
    }  else if(group == TRUE){
      d <- df2 %>% 
        group_by(lostT1, messenger) %>% 
        mutate(!!sym(paste0(outcomes[i],"_other")) := case_when(messenger == 0 & lostT1 == 0 ~ mean(!!sym(outcomes[i]), na.rm = T),
                                                                messenger == 0 & lostT1 == 1 ~ mean(!!sym(outcomes[i]), na.rm = T),
                                                                messenger == 1 ~ !!sym(outcomes[i]))) %>% 
        ungroup() %>% 
        group_by(lostT1, {{fate}}) %>%
        reframe(tidy(lm_robust(as.formula(paste0(outcomes[i], "_other", "~ messenger")), data = pick(everything())))) %>% 
        filter(term != "(Intercept)") %>% 
        mutate(lostT1 = case_when(lostT1 == "Won" ~ "Reward the\nMessenger",
                                    lostT1 == "Lost" ~ "Shoot the\nMessenger"),
               lostT1 = fct_rev(factor(lostT1))) %>% 
        rbind(exp1) 
    } 
    
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
    } else if(outcomes[i] %in% c("trust", "nice", "likeable", "generous")){
      lim = round(c(-.7, 1), 2)
      breaks = round(seq(-.7, 1, .1), 2)
    } else if (outcomes[i] == "attitude_all"){
      lim = c(-.1, .13)
      breaks = seq(-.1, .1, .05)
    } 
    

    # Now, graph the thing
    g <- ggplot(d, aes(x = lostT1, y = estimate)) +
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
    
    # Create additional captions depending on type
    if(group == TRUE){
      name = paste0("study2_grouped_main_", outcomes[i])
      caption_additional = " (assuming no spill-over from messenger alignment in Task 1 for response towards non-messengers)"
    } else if(as_label(enquo(fate)) == "Instructions1"){
      name = paste0("study2_main_fivecent_", outcomes[i])
      caption_additional = " (including 5 and 50 cent differences)"
    } else if (group == FALSE){
      name = paste0("study2_main_", outcomes[i])
      caption_additional = ""
    }
    
    # Create different footnotes for index figures
    if(i == 4 & as_label(enquo(fate)) == "Instructions2"){
      footnote_additional = paste0(" The Shared Fate (Panel A), Unrelated Fate (Panel B), and Opposite Fate (Panel C) conditions are when the partner wins when the respondent wins, the partner winning are unrelated to the respondent winning, and the partner wins when the respondent loses, respectively. Study 1 (Panel D) repeats the index measure from Study 1 as a reference, where respondents were not explicitly given the alignment of their partner.",
                                   " The p-values of the test that $RTM = -STM$ are ", behavior_mag_pvalues[1], ", ",
                                   behavior_mag_pvalues[2], ", ", behavior_mag_pvalues[3],
                                   ", and ", behavior_mag_pvalues[4], ", respectively, for the Shared, Unrelated, Opposite Fate conditions, and Study 1.",
                                   " The p-values of the messenger bias are ", behavior_tmb_pvalues[1], ", ", 
                                   behavior_tmb_pvalues[2], ", ", behavior_tmb_pvalues[3],
                                   ", and ", behavior_tmb_pvalues[4], ", respectively, for the Shared, Unrelated, Opposite Fate conditions, and Study 1.")
      footnote[i] = paste0(footnote[i], footnote_additional)
    } else if (i == 9 & as_label(enquo(fate)) == "Instructions2"){
      footnote_additional = paste0(" The Shared Fate (Panel A), Unrelated Fate (Panel B), and Opposite Fate (Panel C) conditions are when the partner wins when the respondent wins, the partner winning are unrelated to the respondent winning, and the partner wins when the respondent loses, respectively. Study 1 (Panel D) repeats the index measure from Study 1 as a reference, where respondents were not explicitly given the alignment of their partner.",
                                   " The p-values of the test that $RTM = -STM$ are ", attitude_mag_pvalues[1], ", ",
                                   attitude_mag_pvalues[2], ", ", attitude_mag_pvalues[3],
                                   ", and ", attitude_mag_pvalues[4], ", respectively, for the Shared, Unrelated, Opposite Fate conditions, and Study 1.",
                                   " The p-values of the messenger bias are ", attitude_tmb_pvalues[1], ", ", 
                                   attitude_tmb_pvalues[2], ", ",
                                   attitude_tmb_pvalues[3], ", and ", attitude_tmb_pvalues[4], ", respectively, for the Shared, Unrelated, Opposite Fate conditions, and Study 1.")
      footnote[i] = paste0(footnote[i], footnote_additional)
      
    } else if (as_label(enquo(fate)) == "Instructions1"){
      footnote_additional = paste0(" The \\$0.05 Shared Fate (Panel A) and \\$0.50 Shared Fate (Panel B) conditions are when the partner wins \\$0.05 or \\$0.50, respectively, when the respondent wins.",
                                   " The Unrelated Fate (Panel C) condition is when the partner winnings are unrelated to the respondent winning.",
                                   " The \\$0.05 Opposite Fate (Panel D) and \\$0.50 Opposite Fate (Panel E) conditions are when the partner wins \\$0.05 or \\$0.50, respectively, when the respondent loses.")
      footnote[i] = paste0(footnote[i], footnote_additional)
    }
    
    # Only save the Behavior Index and the Attitudes Index
    if(i == 4 | i == 9){
    
    ggsave(g, file = paste0(save_path, "figures/", name, ".png"),
           height = 5.5, width = 10)
    

    tex <- paste0("\\renewcommand{\\baselinestretch}{1.25}%
\\begin{figure}[!t]%
  \\centering
  \\includegraphics[width=1.0\\textwidth]{figures/", name, ".png}
  \\caption{", caption[i], caption_additional, ". 
  \\textit{Note: OLS regression with robust standard errors, with error bars representing 95\\% confidence intervals.",
  footnote[i], "}}
  \\label{fig:", name, "}
\\end{figure}%
\\renewcommand{\\baselinestretch}{1.67}%")
    
    cat(tex, sep = "\n", file = paste0(save_path, "figures_tex/", name, ".tex"))
    
    }
    
  }
}


# Instructions2 = Shared, Unrelated, and Opposite Fates
study2_figures(footnote = footnote_all, caption = caption_all, fate = Instructions2)

# Instructions1 = Shared 5, Shared 50, Unrelated, Opposite 5, Opposite 50
study2_figures(footnote = footnote_all, caption = caption_all, fate = Instructions1)


# (TODO) Table A3. Sample demographics ------------------------------------


# (TODO) Table A4. Balance tests. -----------------------------------------

