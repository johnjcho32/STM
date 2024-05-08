**************************
* STM Study 1 - Analysis *
**************************

*****************
* Balance tests *
*****************
//by messenger
sort messenger
ttest age, by (messenger)
tab sex messenger, chi2
tab pid messenger, chi2
tab leaners messenger, chi2
tab strongPID messenger, chi2
ttest ideology, by (messenger)
ttest education, by (messenger)
tab race messenger, chi2

//by task
sort counting
ttest age, by (counting)
tab sex counting, chi2
tab pid counting, chi2
tab leaners counting, chi2
tab strongPID counting, chi2
ttest ideology, by (counting)
ttest education, by (counting)
tab race counting, chi2

//by win/lose
sort lostT1
ttest age, by (lostT1)
tab sex lostT1, chi2
tab pid lostT1, chi2
tab leaners lostT1, chi2
tab strongPID lostT1, chi2
ttest ideology, by (lostT1)
ttest education, by (lostT1)
tab race lostT1, chi2

//by type of message
sort emotionalMsg
ttest age, by (emotionalMsg)
tab sex emotionalMsg, chi2
tab pid emotionalMsg, chi2 //p < .05
tab leaners emotionalMsg, chi2
tab strongPID emotionalMsg, chi2 //p < .05
ttest ideology, by (emotionalMsg)
ttest education, by (emotionalMsg)
tab race emotionalMsg, chi2

*****************************************
*Balance tests using  Conditional logit *
*****************************************
*(1) messenger
clogit messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld, group(lostT1)
outreg2 using azg.doc, sideway stats(coef se aster) dec(2)

*(2) emotional message
clogit emotionalMsg age female ideology education employed white hispanic believeLuck emotionReg justWorld, group(lostT1)
outreg2 using azg.doc, sideway stats(coef se aster) dec(2)

*(3) counting task
clogit counting age female ideology education employed white hispanic believeLuck emotionReg justWorld, group(lostT1)
outreg2 using azg.doc, sideway stats(coef se aster) dec(2)

*******************************
* Graph the data by condition *
*******************************
sort messenger
scatter dictator_all messenger

//CDFs by win/lose
sort lostT1
*DG
distplot dictator_all, over(lostT1) by(messenger) xtitle("Amount given to partner (in cents)") ytitle("Cumulative probability") title("Dictator Game")
*Spin
distplot spin_all, over(lostT1) by(messenger) xtitle("Choose who spins the wheel on your behalf") ytitle("Cumulative probability") title("Spin the Wheel Task")
*PD
distplot pd_all, over(lostT1) by(messenger) xtitle("Cooperate (C) or Defect (D)") ytitle("Cumulative probability") title("Prisoner's Dilemma")

*Behavioral index
distplot behavior_all, over(lostT1) by(messenger) xtitle("Prosocial behavior") ytitle("Cumulative probability") title("Behavior Index")


*attitudes
distplot likeMessengerTotal, over(lostT1) by(messenger) title("Positive Attitudes toward the Messenger")


//by win/lose
sort lostT1
*DG
hist dictator_messenger, by(lostT1) percent ylabel(0(10)50) xlabel (0(10)50)
graph save dM.gph, replace

hist dictator_other, by(lostT1) percent ylabel(0(10)50) xlabel (0(10)50)
graph save dO.gph, replace

graph combine dM.gph dO.gph

*STW
hist spin_messenger, by(lostT1) percent ylabel(0(10)50) xlabel (0 "PC" 1 "Human") xtitle("Choose foor wheel spin") title("Messenger")
graph save sM.gph, replace

hist spin_other, by(lostT1) percent ylabel(0(10)50) xlabel (0 "PC" 1 "Human") xtitle("Choose foor wheel spin") title("Other")
graph save sO.gph, replace

graph combine sM.gph sO.gph


*PD
hist pd_messenger, by(lostT1) percent ylabel(0(10)80) xlabel (0 "D" 1 "C") xtitle("Defect (D) or Cooperate (C)") title("Messenger")
graph save pdM.gph, replace

hist pd_other, by(lostT1) percent ylabel(0(10)80) xlabel (0 "D" 1 "C") xtitle("Defect (D) or Cooperate (C)") title("Other")
graph save pdO.gph, replace

graph combine pdM.gph pdO.gph


*attitudes
hist likeMessengerTotal, by(lostT1) percent ylabel(0(10)30) title("Messenger")
graph save aM.gph, replace

hist likePartner, by(lostT1) percent ylabel(0(10)30) title("Other")
graph save aO.gph, replace

graph combine aM.gph aO.gph

//correlation behaviors and attitudes
pwcorr behavior_all likePartner_After,sig
pwcorr likePartner_After likePartner, sig

**************************
* Descriptive statistics *
**************************
//scales
alpha justWorld_1 justWorld_2 justWorld_3 justWorld_4 justWorld_5 justWorld_6 justWorld_7, detail
alpha emReg_1 emReg_2 emReg_3 emReg_4 emReg_5, detail
alpha luck_1 luck_2 luck_3 luck_4 luck_5 luck_6 luck_7, detail

sum justWorld
sum emotionReg
sum believeLuck

//emotions after the message
pwcorr anger sadness, sig
pwcorr angerFirst sadnessFirst, sig

pwcorr happy enthusiasm, sig
pwcorr happyFirst enthusiasmFirst, sig

pwcorr confusion indifference, sig

sum angry
sum happiness
sum confusion
sum indifference

//believability of messenger
pwcorr realperson1 realperson2, sig

/*
//crimeStory
tab huber_acq_typical
tab huber_acq_toomuch
tab huber_acq_identify
tab huber_acq_steal
*/

//quizzes
tab q1
tab q2
tab q3
tab q4
tab q5
tab q6
tab q7

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


//attitudes toward the messenger
sum likeMessengerFirst
sum likeMessengerAfter
sum likeMessengerTotal

sort lostT1
by lostT1: sum likeMessengerFirst
by lostT1: sum likeMessengerAfter

by lostT1: sum dictator_messenger if likeMessengerFirst !=.
by lostT1: sum dictator_messenger if likeMessengerAfter !=.

by lostT1: tab spin_messenger if likeMessengerFirst !=.
by lostT1: tab spin_messenger if likeMessengerAfter !=.

by lostT1: tab pd_messenger if likeMessengerFirst !=.
by lostT1: tab pd_messenger if likeMessengerAfter !=.


//attitudes toward the partner
sum likePartner

//believability of survey
sum thinkReal


************
* Analyses *
************

// by emotional vs. neutral message
//Type of message makes no difference
*************
log using TypeOfMessage

sort emotionalMsg
ttest dictator_all, by(emotionalMsg)
tab spin_all emotionalMsg, chi2
tab pd_all emotionalMsg, chi2

ttest dictator_messenger, by(emotionalMsg)
ttest dictator_other, by(emotionalMsg)

tab spin_messenger emotionalMsg, chi2
tab spin_other emotionalMsg, chi2

tab pd_messenger emotionalMsg, chi2
tab pd_other emotionalMsg, chi2

ttest likeMessengerTotal, by(emotionalMsg)

log close
translate TypeOfMessage.smcl TypeOfMessage.pdf
*****

ttest thinkReal, by(emotionalMsg)
ttest taskFair, by(emotionalMsg)
ttest accurateMsg, by(emotionalMsg)
ttest angry, by(emotionalMsg)
ttest happiness, by(emotionalMsg)
ttest confusion, by(emotionalMsg)
ttest indifference, by(emotionalMsg)


//by type of Task 1 (prediction vs. counting)
//Type of task made sometimes a difference
tab lostT1 counting, chi2 //no difference in winning/losing by task

log using TypeOfTask

sort counting
ttest dictator_all, by(counting)
tab spin_all counting, chi2
tab pd_all counting, chi2

ttest dictator_messenger, by(counting)
ttest dictator_other, by(counting)

tab spin_messenger counting, chi2 //choose marginally more often messenger for spinning wheel with skill task
tab spin_other counting, chi2 //choose more often other participant for spinning wheel with skill task

tab pd_messenger counting, chi2
tab pd_other counting, chi2 //cooperate more with other participant with skill task

ttest likeMessengerTotal, by(counting) //Messenger liked more with luck task

log close
translate TypeOfTask.smcl TypeOfTask.pdf
******************************************

ttest thinkReal, by(counting)
ttest taskFair, by(counting)

ttest accurateMsg, by(counting)
ttest angry, by(counting)
ttest happiness, by(counting)
ttest confusion, by(counting)
ttest indifference, by(counting)

// by win / lose task 1
log using WinOrLose

sort lostT1
ttest dictator_all, by(lostT1)
tab spin_all lostT1, chi2
tab pd_all lostT1, chi2

ttest dictator_messenger, by(lostT1) //giving less to messenger when lose
ttest dictator_other, by(lostT1) //no difference in giving to other

tab spin_messenger lostT1, col chi2 //messenger chosen less often to spin when lose
tab spin_other lostT1, col chi2 //no difference in choosing other to spin

tab pd_messenger lostT1, col chi2 //less cooperation with messenger when lose
tab pd_other lostT1, col chi2 //no difference in cooperating with other

ttest likeMessengerTotal, by(lostT1) //messenger is liked less when lose
ttest likePartner, by(lostT1) //other is liked no differently when lose

log close
translate WinOrLose.smcl WinOrLose.pdf 

ttest thinkReal, by(lostT1)
ttest taskFair, by(lostT1)
ttest accurateMsg, by(lostT1)
ttest angry, by(lostT1)
ttest happiness, by(lostT1)
ttest confusion, by(lostT1)
ttest indifference, by(lostT1)


//by play games with messenger / another participant
log using Messenger

sort messenger
ttest dictator_all, by(messenger)
tab spin_all messenger, chi2 //messenger chosen more often than other for spinning wheel
tab pd_all messenger, chi2

ttest likeMessengerTotal, by(messenger) //messenger liked more when assigned to playing with partner

ttest thinkReal, by(messenger)

log close
translate Messenger.smcl Messenger.pdf

**********************************
* Figure 1 - Behavioral measures *
**********************************
//dictator game
ttest dictator_all if messenger == 1, by (lostT1)
ttest dictator_all if messenger == 0, by (lostT1)

ttest dictator_all if lostT1 == 1, by (messenger)
ttest dictator_all if lostT1 == 0, by (messenger)

//spin the wheel task
tab spin_all lost if messenger ==1, chi2
tab spin_all lost if messenger ==0, chi2

tab spin_all messenger if lost ==1, chi2
tab spin_all messenger if lost ==0, chi2

//prisoner's dilemma
tab pd_all lost if messenger ==1, chi2
tab pd_all lost if messenger ==0, chi2

tab pd_all messenger if lost ==1, chi2
tab pd_all messenger if lost ==0, chi2


*********************************
* Figure 2 - Attitudes measures *
*********************************
//trustworthiness
sort lostT1 messenger
by lostT1 messenger: sum trust

ttest trust if messenger == 1, by (lostT1)
ttest trust if messenger == 0, by (lostT1)

ttest trust if lostT1 == 1, by (messenger)
ttest trust if lostT1 == 0, by (messenger)

//Niceness
sort lostT1 messenger
by lostT1 messenger: sum nice

ttest nice if messenger == 1, by (lostT1)
ttest nice if messenger == 0, by (lostT1)

ttest nice if lostT1 == 1, by (messenger)
ttest nice if lostT1 == 0, by (messenger)

//Likeability
sort lostT1 messenger
by lostT1 messenger: sum likeable

ttest likeable if messenger == 1, by (lostT1)
ttest likeable if messenger == 0, by (lostT1)

ttest likeable if lostT1 == 1, by (messenger)
ttest likeable if lostT1 == 0, by (messenger)

//Generosity
sort lostT1 messenger
by lostT1 messenger: sum generous

ttest generous if messenger == 1, by (lostT1)
ttest generous if messenger == 0, by (lostT1)

ttest generous if lostT1 == 1, by (messenger)
ttest generous if lostT1 == 0, by (messenger)






//regressions

********************************************
* Table 1 - Behavioral measures one by one *
********************************************
*(1)
reg dictator_all i.lostT1##i.messenger, robust
//outreg2 using ad.doc, sideway stats(coef se aster) dec(2)
est store Full
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win

test 1.lostT1#1.messenger = 0 //difference in slopes


//tobit regression
tobit dictator_all i.lostT1##i.messenger, ll(0) ul(50) robust
outreg2 using q.doc, sideway stats(coef se aster) dec(2)


*(2)
reg spin_all i.lostT1##i.messenger, robust
//outreg2 using ad.doc, sideway stats(coef se aster) dec(2)
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between choosing the messenger vs. other if lost
test 1.messenger  = 0 //difference between choosing the messenger vs. other if win

test 1.lostT1#1.messenger = 0 //difference in slopes


logit spin_all i.lostT1##i.messenger, or


*(3)
reg pd_all i.lostT1##i.messenger, robust
//outreg2 using ad.doc, sideway stats(coef se aster) dec(2)
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between choosing C toward messenger vs. other if lost
test 1.messenger  = 0 //difference between choosing C toward the messenger vs. other if win

test 1.lostT1#1.messenger = 0 //difference in slopes

logit pd_all i.lostT1##i.messenger, or


**********************************
* Table 2 - Attitudes one by one *
**********************************
*(1)
reg trust i.lostT1##i.messenger, robust
outreg2 using b0.doc, sideway stats(coef se aster) dec(2)
est store Full
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between trustwrthy messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win

*(2)
reg nice i.lostT1##i.messenger, robust
outreg2 using b0.doc, sideway stats(coef se aster) dec(2)
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between nice messenger vs. other if lost
test 1.messenger  = 0 //difference between choosing the messenger vs. other if win

*(3)
reg likeable i.lostT1##i.messenger, robust
outreg2 using b0.doc, sideway stats(coef se aster) dec(2)
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between likeable messenger vs. other if lost
test 1.messenger  = 0 //difference between choosing C toward the messenger vs. other if win

*(4)
reg generous i.lostT1##i.messenger, robust
outreg2 using b0.doc, sideway stats(coef se aster) dec(2)
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between generous messenger vs. other if lost
test 1.messenger  = 0 //difference between choosing C toward the messenger vs. other if win



*********************************************************************
* check whether order of game mattered (FL_81_DO variable, recoded) *
*********************************************************************
//PD
reg dictator_all i.lostT1##i.messenger gameOrder, robust

//Spin
reg spin_all i.lostT1##i.messenger gameOrder, robust

//PD
reg pd_all i.lostT1##i.messenger gameOrder, robust















***********
* Table 2 *
***********
*(1)
reg dictator_all i.counting##i.messenger##i.lostT1, robust
outreg2 using ac.doc, sideway stats(coef se aster) dec(2)

*(2)
reg spin_all i.counting##i.messenger##i.lostT1, robust
outreg2 using ac.doc, sideway stats(coef se aster) dec(2)

*(3)
reg pd_all i.counting##i.messenger##i.lostT1, robust
outreg2 using ac.doc, sideway stats(coef se aster) dec(2)

***********
* Table 3 *
***********
//Just participants who answered the attitudes questions toward partner/messenger after
*(1)
reg likePartner_After i.lostT1##i.messenger, robust
outreg2 using aa.doc, sideway stats(coef se aster) dec(2) replace

*(2)
reg likeMessengerTotal i.lostT1##i.counting##i.emotionalMsg, robust

log using attitudes
***********
* Table 4 *
***********
//Attitudes one by one
//Toward the Messenger
*(1) coefficient of losing significant
reg trustMessenger i.lostT1##i.counting##i.emotionalMsg, robust
outreg2 using z00.doc, sideway stats(coef se aster) dec(2)

*(2) coefficient of losing significant; emotional message: coefficient + interaction significant
reg niceMessenger i.lostT1##i.counting##i.emotionalMsg, robust
outreg2 using z00.doc, sideway stats(coef se aster) dec(2)

*(3) coefficient of losing significant; emotional message: coefficient + interaction significant
reg likeableMessenger i.lostT1##i.counting##i.emotionalMsg, robust
outreg2 using z00.doc, sideway stats(coef se aster) dec(2)

*(4) coefficient of losing significant; emotional message: coefficient significant
reg generousMessenger i.lostT1##i.counting##i.emotionalMsg, robust
outreg2 using z00.doc, sideway stats(coef se aster) dec(2)

//Toward the Partner
*(1) messenger, lost*messenger significant
reg trustPartner i.lostT1##i.counting##i.emotionalMsg##i.messenger, robust
outreg2 using z000.doc, sideway stats(coef se aster) dec(2)

*(2) lost*messenger significant
reg nicePartner i.lostT1##i.counting##i.emotionalMsg##i.messenger, robust
outreg2 using z000.doc, sideway stats(coef se aster) dec(2)

*(3) messenger, lost*messenger significant
reg likeablePartner i.lostT1##i.counting##i.emotionalMsg##i.messenger, robust
outreg2 using z000.doc, sideway stats(coef se aster) dec(2)

*(4) messenger, lost*messenger significant
reg generousPartner i.lostT1##i.counting##i.emotionalMsg##i.messenger, robust
outreg2 using z000.doc, sideway stats(coef se aster) dec(2)

//do people think that the messenger is a real person depending on winning/losing?
//YES, but effect is very small.
ttest thinkReal, by(lostT1)

log close
translate attitudes.smcl attitudes.pdf

************
* Appendix *
************

************
* Table A1 *
************
//demographics
sum age
tab sex
tab pid
tab leaners
tab strongPID
sum ideology
sum education
tab race


************
* Table A2 *
************
//robustness checks: Table 1 with logit (dichotomous outcome measures)
*(1)
logit spin_all i.lostT1##i.messenger, robust
outreg2 using aza.doc, sideway stats(coef se aster) dec(2)

*(2)
logit pd_all i.lostT1##i.messenger, robust
outreg2 using aza.doc, sideway stats(coef se aster) dec(2)


************
* Table A3 *
************
//robustness checks: Table 2 with logit (dichotomous outcome measures)
*(1)
logit spin_all i.counting##i.messenger##i.lostT1, robust
outreg2 using azb.doc, sideway stats(coef se aster) dec(2)

*(2)
logit pd_all i.counting##i.messenger##i.lostT1, robust
outreg2 using azb.doc, sideway stats(coef se aster) dec(2)


************
* Table A4 *
************
//robustness checks: Table 1 with demographics
reg dictator_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using azc.doc, sideway stats(coef se aster) dec(2)

*(2)
reg spin_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using azc.doc, sideway stats(coef se aster) dec(2)

*(3)
reg pd_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using azc.doc, sideway stats(coef se aster) dec(2)

************
* Table A5 *
************
//robustness checks: Table 2 with demographics
*(1)
reg dictator_all i.counting##i.messenger##i.lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using azd.doc, sideway stats(coef se aster) dec(2)

*(2)
reg spin_all i.counting##i.messenger##i.lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using azd.doc, sideway stats(coef se aster) dec(2)

*(3)
reg pd_all i.counting##i.messenger##i.lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using azd.doc, sideway stats(coef se aster) dec(2)


************
* Table A6 *
************
reg behavior_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust


//robustness checks: Table 3 with demographics
reg likePartner_After i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld, robust
outreg2 using aze.doc, sideway stats(coef se aster) dec(2) replace

************
* Table A7 *
************
//emotional message makes no difference
*(1)
reg dictator_all i.counting##i.messenger##i.lostT1##i.emotionalMsg, robust
outreg2 using azf.doc, sideway stats(coef se aster) dec(2)

*(2)
reg spin_all i.counting##i.messenger##i.lostT1##i.emotionalMsg, robust
outreg2 using azf.doc, sideway stats(coef se aster) dec(2)

*(3)
reg pd_all i.counting##i.messenger##i.lostT1##i.emotionalMsg, robust
outreg2 using azf.doc, sideway stats(coef se aster) dec(2)

*****************************
* Analysis of believability *
*****************************
//in behavioral games, more altruism, halo effect, and cooperation when participants think messenger is real
reg dictator_all thinkReal, robust
reg spin_all thinkReal, robust
reg pd_all thinkReal, robust

//Attitudes Toward the Messenger are better when respondent thinks they are a real person
*(1)
reg trustMessenger thinkReal, robust

*(2)
reg niceMessenger thinkReal, robust

*(3)
reg likeableMessenger thinkReal, robust

*(4) 
reg generousMessenger thinkReal, robust

//Attitudes Toward the Partner are better when respondent thinks they are a real person
*(1) 
reg trustPartner thinkReal, robust

*(2)
reg nicePartner thinkReal, robust

*(3) 
reg likeablePartner thinkReal, robust

*(4)
reg generousPartner thinkReal, robust


//robustness checks: Table 1 with demographics + thinkReal
*(1)
reg dictator_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust

*(2)
reg spin_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust

*(3)
reg pd_all i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust


//robustness checks: Table 2 with demographics + thinkReal
*(1)
reg dictator_all i.counting##i.messenger##i.lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust

*(2)
reg spin_all i.counting##i.messenger##i.lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust

*(3)
reg pd_all i.counting##i.messenger##i.lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust



//robustness checks: Table 3 with demographics + thinkReal
reg likePartner_After i.lostT1##i.messenger age female ideology education employed white hispanic believeLuck emotionReg justWorld thinkReal, robust



***********************
* Behavioral measures *
***********************
//regressions only with people who are skeptical that messenger is real (N = 280)
*(1) interaction p = .051 (marginally significant)
reg dictator_all i.lostT1##i.messenger if thinkReal < 3, robust

*(2) interaction significant
reg spin_all i.lostT1##i.messenger if thinkReal < 3, robust

*(3) interaction n.s.
reg pd_all i.lostT1##i.messenger if thinkReal < 3, robust


//regressions only with people who are convinced that messenger is real (N = 595)
*(1)
reg dictator_all i.lostT1##i.messenger if thinkReal > 3, robust

*(2)
reg spin_all i.lostT1##i.messenger if thinkReal > 3, robust

*(3)
reg pd_all i.lostT1##i.messenger if thinkReal > 3, robust


//regressions only with people who said "Neither likely nor unlikely" (N = 1,119)
*(1)
reg dictator_all i.lostT1##i.messenger if thinkReal == 3, robust

*(2)
reg spin_all i.lostT1##i.messenger if thinkReal == 3, robust

*(3)
reg pd_all i.lostT1##i.messenger if thinkReal == 3, robust

*************
* Attitudes *
*************

//regressions only with people who are skeptical that messenger is real (N = 181)
*(1) significant interaction
reg trustPartner i.lostT1##i.messenger if thinkReal < 3, robust

*(2) n.s.
reg nicePartner i.lostT1##i.messenger if thinkReal < 3, robust

*(3) n.s.
reg likeablePartner i.lostT1##i.messenger if thinkReal < 3, robust

*(4) n.s.
reg generousPartner i.lostT1##i.messenger if thinkReal < 3, robust

//regressions only with people who are convinced that messenger is real (N = 383)
*(1) 
reg trustPartner i.lostT1##i.messenger if thinkReal > 3, robust

*(2) 
reg nicePartner i.lostT1##i.messenger if thinkReal > 3, robust

*(3) 
reg likeablePartner i.lostT1##i.messenger if thinkReal > 3, robust

*(4)
reg generousPartner i.lostT1##i.messenger if thinkReal > 3, robust


//regressions only with people who said "Neither likely nor unlikely" (N = 764)
*(1) 
reg trustPartner i.lostT1##i.messenger if thinkReal == 3, robust

*(2) 
reg nicePartner i.lostT1##i.messenger if thinkReal == 3, robust

*(3) 
reg likeablePartner i.lostT1##i.messenger if thinkReal == 3, robust

*(4)
reg generousPartner i.lostT1##i.messenger if thinkReal == 3, robust


***********************************
* Heterogeneity (nothing to show) *
***********************************
//just world scale (0=strongly disagree; 1=strongly agree)
reg dictator_all i.lostT1##i.messenger##c.justWorld, robust

reg spin_all i.lostT1##i.messenger##c.justWorld, robust

reg pd_all i.lostT1##i.messenger##c.justWorld , robust

//emotion regulation scale (0=calm person; 1=hothead)
reg dictator_all i.lostT1##i.messenger##c.emotionReg, robust

reg spin_all i.lostT1##i.messenger##c.emotionReg, robust

reg pd_all i.lostT1##i.messenger##c.emotionReg , robust

//belief in luck scale (0=calm person; 1=hothead)
reg dictator_all i.lostT1##i.messenger##c.believeLuck, robust

reg spin_all i.lostT1##i.messenger##c.believeLuck, robust

reg pd_all i.lostT1##i.messenger##c.believeLuck, robust


//using behavioral index
reg behavior_all i.lostT1##i.messenger##c.justWorld, robust
outreg2 using czz.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.emotionReg, robust
outreg2 using czh.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.believeLuck, robust
outreg2 using czi.doc, sideway stats(coef se aster) dec(2)

//using attitudes index
reg likePartner_After i.lostT1##i.messenger##c.justWorld, robust
outreg2 using dzg.doc, sideway stats(coef se aster) dec(2)

reg likePartner_After i.lostT1##i.messenger##c.emotionReg, robust
outreg2 using dza.doc, sideway stats(coef se aster) dec(2)

reg likePartner_After i.lostT1##i.messenger##c.believeLuck, robust
outreg2 using dzb.doc, sideway stats(coef se aster) dec(2)

*****************************
* Regressions with controls *
*****************************
reg behavior_all i.lostT1##i.messenger justWorld emotionReg believeLuck female age ideology education  employed white hispNew, robust
outreg2 using cze.doc, sideway stats(coef se aster) dec(2)
reg likePartner_After i.lostT1##i.messenger justWorld emotionReg believeLuck female age ideology education  employed white hispNew, robust
outreg2 using cze.doc, sideway stats(coef se aster) dec(2)
