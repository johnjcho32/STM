
# Load libraries and datasets ---------------------------------------------
library(haven)
library(tidyverse)
library(ltm)
select <- dplyr::select

df1 <- read_stata("data/Study1_STM_RECEIVERS.dta") %>% 
  ## Change all -99 measures to NA
  mutate(across(everything(), ~ case_when(.x == -99 ~ NA,
                                          TRUE ~ .x))) %>% 
  ## Get rid of attention check people
  ## Drop 5 people who are missing lostT1
  filter(huber_acq_identify == 3,
         !is.na(coinToss) | !is.na(numberCount)) |> 
  ## Generate various covariate measures
  mutate(female = case_when(sex == 1 ~ 1,
                            TRUE ~ 0),
         employed = case_when(occupation == 1 | occupation == 2 ~ 1,
                              occupation > 2 ~ 0),
         white = case_when(race == 1 ~ 1,
                           race != 1 ~ 0),
         hispNew = case_when(hispanic != 1 ~ 1,
                             hispanic == 1 ~ 0),
         justWorld = (rowMeans(across(matches("justWorld_\\d")), na.rm = TRUE)-1)/5,
         emotionReg = (rowMeans(across(matches("emReg_\\d")), na.rm = TRUE)-1)/4,
         believeLuck = (rowMeans(across(matches("luck_\\d")), na.rm = TRUE)-1)/5,
         angry = (rowMeans(across(c(anger, angerFirst, sadness, sadnessFirst)), na.rm = TRUE))/10,
         happiness = (rowMeans(across(c(happy, enthusiasm, happyFirst, enthusiasmFirst)), na.rm = TRUE))/10,
         confusion = (rowMeans(across(c(confused, confusedFirst)), na.rm = TRUE))/10,
         indifference = (rowMeans(across(c(indifferent, indifferentFirst)), na.rm = TRUE))/10) |> 
  ## Partisanship (including leaners), 1 = Strong Dem, 7 = Strong Rep
  mutate(pid7 = case_when(pid == 1 & strongPID == 1 ~ 1,
                          pid == 1 & strongPID == 2 ~ 2,
                          pid == 3 & leaners == 1 | pid == 4 & leaners == 1 ~ 3,
                          pid == 3 & leaners == 2 | pid == 4 & leaners == 2 ~ 4,
                          pid == 3 & leaners == 3 | pid == 4 & leaners == 3 ~ 5,
                          pid == 2 & strongPID == 2 ~ 6,
                          pid == 2 & strongPID == 1 ~ 7)) |> 
  # ATTITUDES
  # INDIVIDUAL ATTITUDES
  ## Rename 1 to trust DV, 2 to nice DV, 3 to likeable DV, and 4 to generous DV
  rename_with(.fn = ~ case_when(
    str_detect(.x, "_1$") ~ str_replace(.x, "_1$", "_trust"),
    str_detect(.x, "_2$") ~ str_replace(.x, "_2$", "_nice"),
    str_detect(.x, "_3$") ~ str_replace(.x, "_3$", "_likeable"),
    str_detect(.x, "_4$") ~ str_replace(.x, "_4$", "_generous"),
    TRUE ~ .x),
    .cols = matches("likeMessenger|likePartner")) %>% 
  ## trustMessenger: create a Messenger only DV for each attitude (likeMessengerAfter and likeMessengerFirst, for the 4 attitudes)
  mutate(across(matches("likeMessengerAfter_"), 
                ~ coalesce(.x, get(str_replace(cur_column(), "After", "First"))),
                .names = '{paste0(stringr::str_extract(.col, "_(.*)", group = 1), "Messenger")}')) %>% 
  ## trustPartner: Create an After-games only DV for each attitude (likeMessengerAfter and likePartner, for the 4 attitudes)
  mutate(across(matches("likeMessengerAfter_"), 
                ~ coalesce(.x, get(str_replace(cur_column(), "MessengerAfter", "Partner"))),
                .names = '{paste0(stringr::str_extract(.col, "_(.*)", group = 1), "Partner")}')) %>% 
  ## trust: Finally, create a DV for each attitude outcome (combine trustMessenger with trustPartner, etc.)
  mutate(across(matches("Messenger$", ignore.case = FALSE),
                ~ coalesce(.x, get(str_replace(cur_column(), "Messenger", "Partner"))),
                .names = '{stringr::str_extract(.col, "(.*)Messenger", group = 1)}')) %>% 
  # ATTITUDES INDEX
  ## Create meaned dependent variables, depending on the order
  ## Either asked about attitudes towards messengers before, after, or partner after
  mutate(likeMessengerFirst = rowMeans(across(matches("likeMessengerFirst_"))),
         likeMessengerAfter = rowMeans(across(matches("likeMessengerAfter_"))),
         likePartner = rowMeans(across(matches("likePartner_")))) |> 
  ## Combine attitudes towards the messenger (first and after)
  # mutate(likeMessengerTotal = coalesce(likeMessengerFirst, likeMessengerAfter)) %>% 
  # ## Combine attitudes towards the partner or messenger after
  # mutate(likePartner_After = coalesce(likePartner, likeMessengerAfter)) %>% 
  ## Combine all attitude measures (this is the Attitudes Index)
  mutate(attitude_all = (coalesce(likeMessengerFirst, likeMessengerAfter, likePartner)-1)/6) %>%
  # BEHAVIORS
  # INDIVIDUAL BEHAVIORS
  ## Create behavioral measures
  mutate(dictator_all = coalesce(dictator_messenger_1, dictator_other_1),
         spin_all = coalesce(spin_messenger, spin_other),
         pd_all = coalesce(pd_messenger, pd_other)) %>% 
  # BEHAVIORAL INDEX
  ## Standardize behavioral measures
  mutate(zDictator_all = (dictator_all-mean(dictator_all))/sd(dictator_all),
         zPd_all = (pd_all-mean(pd_all))/sd(pd_all),
         zSpin_all = (spin_all-mean(spin_all))/sd(spin_all)) %>% 
  ## Create behavioral index
  mutate(behavior_all = rowMeans(across(c(zDictator_all, zPd_all, zSpin_all)), na.rm = TRUE)) %>% 
  # MISC
  ## Create indicators for other randomizations
  mutate(thinkReal = rowMeans(across(c(realperson1, realperson2)), na.rm = TRUE),
         lostT1 = case_when(coinToss == 0 | numberCount == 0 ~ 1,
                            coinToss == 1 | numberCount == 1 ~ 0),
         # lostT1 = factor(lostT1, labels = c("Won", "Lost")),
         counting = case_when(Task1 == "Counting" ~ 1,
                              TRUE ~ 0),
         # counting = factor(counting, labels = c("Prediction", "Count")),
         emotionalMsg = case_when(Task1CorrectMsg == "Congratulations! You won!" | 
                                    Task1IncorrectMsg == "Too bad! You lost!" ~ 1,
                                  TRUE ~ 0),
         # emotionalMsg = factor(emotionalMsg, labels = c("Neutral Message", "Emotional Message")),
         messenger = case_when(Task2PartnerShort == "the messenger from part 1" ~ 1,
                               TRUE ~ 0),
         attitudeOrder = case_when(!is.na(likeMessengerFirst_generous) ~ "MessengerFirst",
                                   !is.na(likeMessengerAfter_generous) ~ "MessengerAfter",
                                   !is.na(likePartner_generous) ~ "PartnerAfter"),
         gameOrder = case_when(FL_81_DO == "Dictator|PD|SpintheWheel" ~ 1, ##DPS
                               FL_81_DO == "Dictator|SpintheWheel|PD" ~ 2, ##DSP
                               FL_81_DO == "PD|Dictator|SpintheWheel" ~ 3, ##PDS
                               FL_81_DO == "PD|SpintheWheel|Dictator" ~ 4, ##PSD
                               FL_81_DO == "SpintheWheel|Dictator|PD" ~ 5, ##SDP
                               FL_81_DO == "SpintheWheel|PD|Dictator" ~ 6))
  

df2 <- read_stata("data/Study2_Receiver.dta") |> 
  # Drop this MTurk worker
  # Drop 10 people who are missing lostT1 data
  filter(MturkID != "rwerwerewrewr",
         !is.na(coinToss) | !is.na(numberCount)) |> 
  ## Generate various covariate measures
  mutate(female = case_when(sex == 1 ~ 1,
                            TRUE ~ 0),
         employed = case_when(occupation == 1 | occupation == 2 ~ 1,
                              occupation > 2 ~ 0),
         white = case_when(race == 1 ~ 1,
                           race != 1 ~ 0),
         hispNew = case_when(hispanic != 1 ~ 1,
                             hispanic == 1 ~ 0),
         justWorld = (rowMeans(across(matches("justWorld_\\d")), na.rm = TRUE)-1)/5,
         emotionReg = (rowMeans(across(matches("emReg_\\d")), na.rm = TRUE)-1)/4,
         believeLuck = (rowMeans(across(matches("luck_\\d")), na.rm = TRUE)-1)/5,
         angry = (rowMeans(across(c(anger, sadness)), na.rm = TRUE))/10,
         happiness = (rowMeans(across(c(happy, enthusiasm)), na.rm = TRUE))/10,
         confusion = confused/10,
         indifference = indifferent/10) |> 
  ## Partisanship (including leaners), 1 = Strong Dem, 7 = Strong Rep
  mutate(pid7 = case_when(pid == 1 & strongPID == 1 ~ 1,
                          pid == 1 & strongPID == 2 ~ 2,
                          pid == 3 & leaners == 1 | pid == 4 & leaners == 1 ~ 3,
                          pid == 3 & leaners == 2 | pid == 4 & leaners == 2 ~ 4,
                          pid == 3 & leaners == 3 | pid == 4 & leaners == 3 ~ 5,
                          pid == 2 & strongPID == 2 ~ 6,
                          pid == 2 & strongPID == 1 ~ 7)) |> 
  ## Create indicators for other randomizations
  mutate(thinkReal = rowMeans(across(c(realperson1, realperson2)), na.rm = TRUE),
         lostT1 = case_when(coinToss == 0 | numberCount == 0 ~ 1,
                            coinToss == 1 | numberCount == 1 ~ 0),
         # lostT1 = factor(lostT1, labels = c("Won", "Lost")),
         counting = case_when(Task1 == "Counting" ~ 1,
                              TRUE ~ 0),
         # counting = factor(counting, labels = c("Prediction", "Count")),
         messenger = case_when(Task2PartnerShort == "the messenger from part 1" ~ 1,
                               TRUE ~ 0),
         gameOrder = case_when(FL_81_DO == "Dictator|PD|SpintheWheel" ~ 1, ##DPS
                               FL_81_DO == "Dictator|SpintheWheel|PD" ~ 2, ##DSP
                               FL_81_DO == "PD|Dictator|SpintheWheel" ~ 3, ##PDS
                               FL_81_DO == "PD|SpintheWheel|Dictator" ~ 4, ##PSD
                               FL_81_DO == "SpintheWheel|Dictator|PD" ~ 5, ##SDP
                               FL_81_DO == "SpintheWheel|PD|Dictator" ~ 6)) |> 
  # Attitudes
  ## ATTITUDES INDEX
  mutate(attitude_all = (rowMeans(across(matches("likePartner_")))-1)/6) |> 
  # Rename attitudes measures
  rename(trust = likePartner_1,
         nice = likePartner_2,
         likeable = likePartner_3,
         generous = likePartner_4) |> 
  # Behaviors
  ## Create behavioral measures
  mutate(dictator_all = coalesce(dictator_messenger_1, dictator_other_1),
         spin_all = coalesce(spin_messenger, spin_other),
         pd_all = coalesce(pd_messenger, pd_other)) |> 
  # BEHAVIORAL INDEX
  ## Standardize behavioral measures
  mutate(zDictator_all = (dictator_all-mean(dictator_all))/sd(dictator_all),
         zPd_all = (pd_all-mean(pd_all))/sd(pd_all),
         zSpin_all = (spin_all-mean(spin_all))/sd(spin_all)) |> 
  ## Create behavioral index
  mutate(behavior_all = rowMeans(across(c(zDictator_all, zPd_all, zSpin_all)), na.rm = TRUE)) |> 
  # Misc
  mutate(reward = reward - 5,
         punish = punish - 5) |> 
  # Conditions
  mutate(condition_num = case_when(
    otherValues == 0 & messenger == 0 & counting == 1 ~ 0, 
    otherValues == 0 & messenger == 0 & counting == 0 ~ 1, 
    otherValues == 0 & messenger == 1 & counting == 1 ~ 2, 
    otherValues == 0 & messenger == 1 & counting == 0 ~ 3, 
    Instructions1 == 0 & messenger == 0 & counting == 1 ~ 4, 
    Instructions1 == 0 & messenger == 0 & counting == 0 ~ 5, 
    Instructions1 == 0 & messenger == 1 & counting == 1 ~ 6, 
    Instructions1 == 0 & messenger == 1 & counting == 0 ~ 7, 
    Instructions1 == 1 & messenger == 0 & counting == 1 ~ 8, 
    Instructions1 == 1 & messenger == 0 & counting == 0 ~ 9, 
    Instructions1 == 1 & messenger == 1 & counting == 1 ~ 10,
    Instructions1 == 1 & messenger == 1 & counting == 0 ~ 11,
    Instructions1 == 2 & messenger == 0 & counting == 1 ~ 12,
    Instructions1 == 2 & messenger == 0 & counting == 0 ~ 13,
    Instructions1 == 2 & messenger == 1 & counting == 1 ~ 14,
    Instructions1 == 2 & messenger == 1 & counting == 0 ~ 15,
    Instructions1 == 3 & messenger == 0 & counting == 1 ~ 16,
    Instructions1 == 3 & messenger == 0 & counting == 0 ~ 17,
    Instructions1 == 3 & messenger == 1 & counting == 1 ~ 18,
    Instructions1 == 3 & messenger == 1 & counting == 0 ~ 19),
    condition = factor(condition_num, 
                       labels = c("Zero; Other; Count", "Zero; Other; Prediction",
                                  "Zero; Messenger; Count", "Zero; Messenger; Prediction",
                                  "0.05 Shared; Other; Count", "0.05 Shared; Other; Prediction",
                                  "0.05 Shared; Messenger; Count", "0.05 Shared; Messenger; Prediction",
                                  "0.05 Opposite; Other; Count", "0.05 Opposite; Other; Prediction",
                                  "0.05 Opposite; Messenger; Count", "0.05 Opposite; Messenger; Prediction",
                                  "0.50 Shared; Other; Count", "0.50 Shared; Other; Prediction",
                                  "0.50 Shared; Messenger; Count", "0.50 Shared; Messenger; Prediction",
                                  "0.50 Opposite; Other; Count", "0.50 Opposite; Other; Prediction",
                                  "0.50 Opposite; Messenger; Count", "0.50 Opposite; Messenger; Prediction"))) |> 
  # Relabel Instructions1 and Instructions2
  mutate(Instructions1 = Instructions1 + 1,
         Instructions1 = case_when(otherValues == 0 ~ 0,
                                   TRUE ~ Instructions1),
         Instructions2 = case_when(Instructions1 == 0 ~ 0, # Unrelated Fate
                                   Instructions1 == 1 | Instructions1 == 3 ~ 1, # Shared Fate
                                   Instructions1 == 2 | Instructions1 == 4 ~ 2), # Opposite Fate
         Instructions1 = factor(Instructions1, 
                                levels = c(3, 1, 0, 2, 4),
                                label = c ("$0.50 Shared Fate", "$0.05 Shared Fate", 
                                           "Unrelated Fate", "$0.05 Opposite Fate",
                                           "$0.50 Opposite Fate")),
         Instructions2 = factor(Instructions2,
                                levels = c(1, 0, 2),
                                label = c("Shared Fate", "Unrelated Fate", "Opposite Fate")))


# Stata coding for Instructions1 variable:
# 0 "Zero"
# 1 "0.05 Shared"
# 2 "0.05 Opposite"
# 3 "0.50 Shared"
# 4 "0.50 Opposite"
# 
#   
# 
# 0 - Zero; Other; Count                         N = 253
# 1 - Zero; Other; Prediction                    N = 252
# 2 - Zero; Messenger; Count                     N = 249
# 3 - Zero; Messenger; Prediction                N = 254
# 4 - 0.05 Shared; Other; Count                  N = 124
# 5 - 0.05 Shared; Other; Prediction             N = 126
# 6 - 0.05 Shared; Messenger; Count              N = 123
# 7 - 0.05 Shared; Messenger; Prediction         N = 125
# 8 - 0.05 Opposite; Other; Count                N = 124
# 9 - 0.05 Opposite; Other; Prediction           N = 129
# 10 - 0.05 Opposite; Messenger; Count           N = 123
# 11 - 0.05 Opposite; Messenger; Prediction      N = 128
# 12 - 0.50 Shared; Other; Count                 N = 127
# 13 - 0.50 Shared; Other; Prediction            N = 125
# 14 - 0.50 Shared; Messenger; Count             N = 124
# 15 - 0.50 Shared; Messenger; Prediction        N = 129
# 16 - 0.50 Opposite; Other; Count               N = 121
# 17 - 0.50 Opposite; Other; Prediction          N = 126
# 18 - 0.50 Opposite; Messenger; Count           N = 125
# 19 - 0.50 Opposite; Messenger; Prediction      N = 123


  


