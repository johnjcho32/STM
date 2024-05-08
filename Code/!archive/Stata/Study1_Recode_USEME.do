*********************************************************
* Recode for the first 'Shoot the Messenger' experiment *
********************************************************* 
//drop people who did not pass screeners
drop if huber_acq_identify !=3

// generate variables
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

egen angry = rmean(anger sadness angerFirst sadnessFirst)
replace angry = angry/10

egen happiness = rmean(happy enthusiasm happyFirst enthusiasmFirst)
replace happiness = happiness/10

egen confusion = rmean(confused confusedFirst)
replace confusion = confusion/10

egen indifference = rmean(indifferent indifferentFirst)
replace indifference = indifference/10

//female
gen female = 1 if sex == 1
replace female = 0 if sex == 0 | sex == 2 | sex == 3

//partisanship (including leaners); 1 = Strong Dem; 7 = Strong Rep
gen pid7 = 1 if pid == 1 & strongPID == 1
replace pid7 = 2 if pid == 1 & strongPID == 2
replace pid7 = 3 if pid == 3 & leaners == 1 | pid == 4 & leaners == 1
replace pid7 = 4 if pid == 3 & leaners == 2 | pid == 4 & leaners == 2
replace pid7 = 5 if pid == 3 & leaners == 3 | pid == 4 & leaners == 3
replace pid7 = 6 if pid == 2 & strongPID == 2
replace pid7 = 7 if pid == 2 & strongPID == 1


replace likeMessengerAfter_2 = . if likeMessengerAfter_2 < 0
replace likeMessengerFirst_4 = . if likeMessengerFirst_4 < 0
replace likeMessengerAfter_4 = . if likeMessengerAfter_4 < 0

//scale: positive attitudes toward the messenger
// FIRST: 0.91
alpha likeMessengerFirst_1 likeMessengerFirst_2 likeMessengerFirst_3 likeMessengerFirst_4, item

//AFTER: 0.95
alpha likeMessengerAfter_1 likeMessengerAfter_2 likeMessengerAfter_3 likeMessengerAfter_4, item


//scale: positive attitudes toward the partner: 0.89
replace likePartner_1 = . if likePartner_1 < 0
replace likePartner_2 = . if likePartner_2 < 0
replace likePartner_3 = . if likePartner_3 < 0
replace likePartner_4 = . if likePartner_4 < 0

alpha likePartner_1 likePartner_2 likePartner_3 likePartner_4, detail

egen likeMessengerFirst = rmean(likeMessengerFirst_1 likeMessengerFirst_2 likeMessengerFirst_3 likeMessengerFirst_4)
egen likeMessengerAfter = rmean(likeMessengerAfter_1 likeMessengerAfter_2 likeMessengerAfter_3 likeMessengerAfter_4)
egen likePartner = rmean(likePartner_1 likePartner_2 likePartner_3 likePartner_4)

//positive attitudes toward the messenger (combined first-after)
egen likeMessengerTotal = rmean(likeMessengerFirst likeMessengerAfter)

//positive attitudes toward either the partner or the messenger after)
egen likePartner_After = rmean (likePartner likeMessengerAfter)

//like other person, before/after, messenger/non-messenger combined
egen likeOther= rmean(likeMessengerFirst likeMessengerAfter likePartner)
replace likeOther = (likeOther-1)/6

egen thinkReal = rmean (realperson1 realperson2)

gen lostT1 = 1 if coinToss == 0 | numberCount == 0
replace lostT1 = 0 if coinToss == 1 | numberCount == 1

gen counting = 1 if Task1 == "Counting"
replace counting = 0 if counting == .

gen emotionalMsg = 1 if Task1CorrectMsg == "Congratulations! You won!" | Task1IncorrectMsg == "Too bad! You lost!"
replace emotionalMsg = 0 if emotionalMsg == .

gen messenger = 1 if Task2PartnerShort == "the messenger from part 1" 
replace messenger = 0 if messenger == .

egen dictator_all = rmean(dictator_messenger_1 dictator_other_1)
egen spin_all = rmean(spin_messenger spin_other)
egen pd_all = rmean(pd_messenger pd_other)

//standardize behavioral measures
egen zDictator_all = std(dictator_all)
egen zPd_all = std(pd_all)
egen zSpin_all = std(spin_all)

sum zDictator_all zPd_all zSpin_all

egen behavior_all = rmean(zDictator_all zPd_all zSpin_all)

sum behavior_all


label define lost1 0 "Won" 1 "Lost"
label values lostT1 lost1 
tab lostT1

label define pd_messenger 0 "Defect" 1 "Cooperate"
label values pd_messenger pd_messenger 
tab pd_messenger

label define pd_other 0 "Defect" 1 "Cooperate"
label values pd_other pd_other 
tab pd_other

label define pd_all 0 "Defect" 1 "Cooperate"
label values pd_all pd_all 
tab pd_all

label define spin_all 0 "Computer" 1 "Human"
label values spin_all spin_all 
tab spin_all

label define spin_messenger 0 "Computer" 1 "Messenger"
label values spin_messenger spin_messenger 
tab spin_messenger

label define spin_other 0 "Computer" 1 "Other"
label values spin_other spin_other 
tab spin_other

label define counting 0 "Prediction" 1 "Count"
label values counting counting 
tab counting

label define emotionalMsg 0 "Neutral Message" 1 "Emotional Message"
label values emotionalMsg emotionalMsg 
tab emotionalMsg

label variable likeMessengerFirst_1 "Trustworthy"
label variable likePartner_1 "Trustworthy"
label variable likeMessengerAfter_1 "Trustworthy"

label variable likeMessengerFirst_2 "Nice"
label variable likePartner_2 "Nice"
label variable likeMessengerAfter_2 "Nice"

label variable likeMessengerFirst_3 "Likeable"
label variable likePartner_3 "Likeable"
label variable likeMessengerAfter_3 "Likeable"

label variable likeMessengerFirst_4 "Generous"
label variable likePartner_4 "Generous"
label variable likeMessengerAfter_4 "Generous"

egen trustMessenger = rmean(likeMessengerFirst_1 likeMessengerAfter_1)
egen niceMessenger = rmean(likeMessengerFirst_2 likeMessengerAfter_2)
egen likeableMessenger = rmean(likeMessengerFirst_3 likeMessengerAfter_3)
egen generousMessenger = rmean (likeMessengerFirst_4 likeMessengerAfter_4)

egen trustPartner = rmean(likePartner_1 likeMessengerAfter_1)
egen nicePartner = rmean(likePartner_2 likeMessengerAfter_2)
egen likeablePartner = rmean(likePartner_3 likeMessengerAfter_3)
egen generousPartner = rmean (likePartner_4 likeMessengerAfter_4)

egen trust = rmean(trustMessenger trustPartner)
egen nice = rmean(niceMessenger nicePartner)
egen likeable = rmean(likeableMessenger likeablePartner)
egen generous = rmean(generousMessenger generousPartner)

********************************************************************************
//3! possible combinations for game order
gen gameOrder = 0 if FL_81_DO == "Dictator|PD|SpintheWheel"     //DPS
replace gameOrder = 1 if FL_81_DO == "Dictator|SpintheWheel|PD" //DSP
replace gameOrder = 2 if FL_81_DO == "PD|Dictator|SpintheWheel" //PDS
replace gameOrder = 3 if FL_81_DO == "PD|SpintheWheel|Dictator" //PSD
replace gameOrder = 4 if FL_81_DO == "SpintheWheel|Dictator|PD" //SDP
replace gameOrder = 5 if FL_81_DO == "SpintheWheel|PD|Dictator" //SPD
