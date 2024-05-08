*************************************************************************
* Pre-analysis plan code for the first 'Shoot the Messenger' experiment *
************************************************************************* 
set more off

drop if MturkID == "rwerwerewrewr"

// generate variables

//female
gen female = 1 if sex == 1
replace female = 0 if sex == 0 | sex == 2 | sex == 3

gen employed = 1 if occupation == 1 | occupation == 2
replace employed = 0 if occupation > 2 & occupation !=.

gen white = 1 if race == 1
replace white = 0 if race != 1 & race !=. 

gen hispNew = 1 if hispanic != 1 & hispanic !=.
replace hispNew = 0 if hispanic == 1

egen justWorld = rmean(justWorld_1 justWorld_2 justWorld_3 justWorld_4 justWorld_5 justWorld_6 justWorld_7)
replace justWorld = (justWorld-1)/5

egen emotionReg = rmean(emReg_1 emReg_2 emReg_3 emReg_4 emReg_5)
replace emotionReg = (emotionReg-1)/4

egen believeLuck = rmean(luck_1 luck_2 luck_3 luck_4 luck_5 luck_6 luck_7)
replace believeLuck = (believeLuck-1)/5

egen angry = rmean(anger sadness)
replace angry = angry/10

egen happiness = rmean (happy enthus)
replace happiness = happiness/10

egen likePartner = rmean(likePartner_1 likePartner_2 likePartner_3 likePartner_4)

egen thinkReal = rmean (realperson1 realperson2)

gen lostT1 = 1 if coinToss == 0 | numberCount == 0
replace lostT1 = 0 if coinToss == 1 | numberCount == 1

gen counting = 1 if Task1 == "Counting"
replace counting = 0 if counting == .

gen messenger = 1 if Task2PartnerShort == "the messenger from part 1" 
replace messenger = 0 if messenger == .

egen dictator_all = rmean(dictator_messenger_1 dictator_other_1)
egen spin_all = rmean(spin_messenger spin_other)
egen pd_all = rmean(pd_messenger pd_other)

***********************************
* standardize behavioral measures *
***********************************
//messenger
egen zDictator_m = std(dictator_m)
egen zPd_m = std(pd_m)
egen zSpin_m = std(spin_m)

sum zDictator_m zPd_m zSpin_m

egen behavior_m = rmean(zDictator_m zPd_m zSpin_m)

sum behavior_m

//other
egen zDictator_o = std(dictator_o)
egen zPd_o = std(pd_o)
egen zSpin_o = std(spin_o)

sum zDictator_o zPd_o zSpin_o

egen behavior_o = rmean(zDictator_o zPd_o zSpin_o)

sum behavior_o

//all participants
egen zDictator_all = std(dictator_all)
egen zPd_all = std(pd_all)
egen zSpin_all = std(spin_all)

sum zDictator_all zPd_all zSpin_all

egen behavior_all = rmean(zDictator_all zPd_all zSpin_all)

sum behavior_all

replace reward = reward - 5
replace punish = punish - 5


**************
* Conditions *
**************
/*
Stata coding for Instructions1 variable:
0 "Zero"
1 "0.05 Shared"
2 "0.05 Opposite"
3 "0.50 Shared"
4 "0.50 Opposite"
*/

/*
0 - Zero; Other; Count                         N = 253
1 - Zero; Other; Prediction                    N = 252
2 - Zero; Messenger; Count                     N = 249
3 - Zero; Messenger; Prediction                N = 254
4 - 0.05 Shared; Other; Count                  N = 124
5 - 0.05 Shared; Other; Prediction             N = 126
6 - 0.05 Shared; Messenger; Count              N = 123
7 - 0.05 Shared; Messenger; Prediction         N = 125
8 - 0.05 Opposite; Other; Count                N = 124
9 - 0.05 Opposite; Other; Prediction           N = 129
10 - 0.05 Opposite; Messenger; Count           N = 123
11 - 0.05 Opposite; Messenger; Prediction      N = 128
12 - 0.50 Shared; Other; Count                 N = 127
13 - 0.50 Shared; Other; Prediction            N = 125
14 - 0.50 Shared; Messenger; Count             N = 124
15 - 0.50 Shared; Messenger; Prediction        N = 129
16 - 0.50 Opposite; Other; Count               N = 121
17 - 0.50 Opposite; Other; Prediction          N = 126
18 - 0.50 Opposite; Messenger; Count           N = 125
19 - 0.50 Opposite; Messenger; Prediction      N = 123
*/

gen condition = 0 if otherValues == 0 & messenger == 0 & counting == 1
replace condition = 1 if otherValues == 0 & messenger == 0 & counting == 0
replace condition = 2 if otherValues == 0 & messenger == 1 & counting == 1
replace condition = 3 if otherValues == 0 & messenger == 1 & counting == 0
replace condition = 4 if Instructions1 == 0 & messenger == 0 & counting == 1
replace condition = 5 if Instructions1 == 0 & messenger == 0 & counting == 0
replace condition = 6 if Instructions1 == 0 & messenger == 1 & counting == 1
replace condition = 7 if Instructions1 == 0 & messenger == 1 & counting == 0
replace condition = 8 if Instructions1 == 1 & messenger == 0 & counting == 1
replace condition = 9 if Instructions1 == 1 & messenger == 0 & counting == 0
replace condition = 10 if Instructions1 == 1 & messenger == 1 & counting == 1
replace condition = 11 if Instructions1 == 1 & messenger == 1 & counting == 0
replace condition = 12 if Instructions1 == 2 & messenger == 0 & counting == 1
replace condition = 13 if Instructions1 == 2 & messenger == 0 & counting == 0
replace condition = 14 if Instructions1 == 2 & messenger == 1 & counting == 1
replace condition = 15 if Instructions1 == 2 & messenger == 1 & counting == 0
replace condition = 16 if Instructions1 == 3 & messenger == 0 & counting == 1
replace condition = 17 if Instructions1 == 3 & messenger == 0 & counting == 0
replace condition = 18 if Instructions1 == 3 & messenger == 1 & counting == 1
replace condition = 19 if Instructions1 == 3 & messenger == 1 & counting == 0

label def condition 0 "Zero; Other; Count" 1 "Zero; Other; Prediction"  ///
	2 "Zero; Messenger; Count" 3 "Zero; Messenger; Prediction" ///
	4 "0.05 Shared; Other; Count" 5 "0.05 Shared; Other; Prediction" ///
	6 "0.05 Shared; Messenger; Count" 7 "0.05 Shared; Messenger; Prediction" ///
	8 "0.05 Opposite; Other; Count" 9 "0.05 Opposite; Other; Prediction" ///
	10 "0.05 Opposite; Messenger; Count" 11 "0.05 Opposite; Messenger; Prediction" ///
	12 "0.50 Shared; Other; Count" 13 "0.50 Shared; Other; Prediction" ///
	14 "0.50 Shared; Messenger; Count" 15 "0.50 Shared; Messenger; Prediction" ///
	16 "0.50 Opposite; Other; Count" 17 "0.50 Opposite; Other; Prediction" ///
	18 "0.50 Opposite; Messenger; Count" 19 "0.50 Opposite; Messenger; Prediction"
label values condition condition

replace Instructions1 = Instructions1+1
replace Instructions1 = 0 if otherValues == 0
label def instructions1 0 "Unrelated Fate" 1 "0.05 Shared" 2 "0.05 Opposite" ///
		3 "0.50 Shared" 4 "0.50 Opposite" 
label values Instructions1 instructions1

gen Instructions2 = 0 if Instructions1 == 0
replace Instructions2 = 1 if (Instructions1 == 1 | Instructions1 == 3)
replace Instructions2 = 2 if (Instructions1 == 2 | Instructions1 == 4)

label def instructions2 0 "Unrelated Fate" 1 "Shared" 2 "Opposite"
label values Instructions2 instructions2

**************************
* Descriptive statistics *
**************************
//scales

//0.91
alpha justWorld_1 justWorld_2 justWorld_3 justWorld_4 justWorld_5 justWorld_6 justWorld_7, item

//0.90
alpha emReg_1 emReg_2 emReg_3 emReg_4 emReg_5, item

//0.87
alpha luck_1 luck_2 luck_3 luck_4 luck_5 luck_6 luck_7, item

sum justWorld
sum emotionReg
sum believeLuck

//emotions after the message
pwcorr anger sadness, sig
pwcorr happy enthus, sig
pwcorr confused indifferent, sig

sum angry
sum happiness
sum confused
sum indifferent

//crimeStory
tab huber_acq_typical
tab huber_acq_toomuch
tab huber_acq_identify
tab huber_acq_steal

//quizzes
tab q1
tab q2
tab q3
tab q4
tab q5
tab q6
tab q7
tab q8

//dictator game
sum dictator_all

//spin_all the wheel
tab spin_all

//prisoner's dilemma
tab pd_all

//attitudes toward the task
sum taskFair

//attitudes toward the message
sum accurateMsg

pwcorr taskFair accurateMsg, sig


//scale: positive attitudes toward the partner
alpha likePartner_1 likePartner_2 likePartner_3 likePartner_4, detail

//attitudes toward the partner
sum likePartner

//believability of survey
sum thinkReal

pwcorr realperson1 realperson2, sig


sort condition
by condition: tab lostT1
sort counting
by counting: tab lostT1


********************************************************************************
//3! possible combinations for game order
gen gameOrder = 0 if FL_81_DO == "Dictator|PD|SpintheWheel"     //DPS
replace gameOrder = 1 if FL_81_DO == "Dictator|SpintheWheel|PD" //DSP
replace gameOrder = 2 if FL_81_DO == "PD|Dictator|SpintheWheel" //PDS
replace gameOrder = 3 if FL_81_DO == "PD|SpintheWheel|Dictator" //PSD
replace gameOrder = 4 if FL_81_DO == "SpintheWheel|Dictator|PD" //SDP
replace gameOrder = 5 if FL_81_DO == "SpintheWheel|PD|Dictator" //SPD
