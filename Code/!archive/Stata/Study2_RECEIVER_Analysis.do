log using study2

//correlation attitudes-behavior
pwcorr behavior_all likePartner, sig

//BEHAVIORS
//pooled regression
reg behavior_all i.messenger##i.lostT1##i.Instructions2, robust
//outreg2 using bez.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger (unrelated fate)
//test 1.messenger#1.Instructions2 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 = 0 //
//test 1.messenger#2.Instructions2 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0 //

test 1.messenger = 0 //effect of winning when messenger compared to non-messenger (unrelated fate)

test 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 = 0 //effect of losing when messenger (shared fate)
test 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0 //effect of losing when messenger (opposite fate)


test 1.lostT1#1.messenger = 0 //difference in slopes against non-messenger (unrelated fate)

test 1.Instructions2 + 1.lostT1#1.Instructions2 + 1.messenger#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 = 0 //effect of losing when messenger in shared fate compared to unrelated fate
test 1.Instructions2 + 1.messenger#1.Instructions2 = 0 //effect of winning when messenger in shared fate compared to unrelated fate

test 2.Instructions2 + 1.lostT1#2.Instructions2 + 1.messenger#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0 //effect of losing when messenger in opposite fate compared to unrelated fate
test 2.Instructions2 + 1.messenger#2.Instructions2 = 0 //effect of winning when messenger in opposite fate compared to unrelated fate

//difference between win-lose difference in unrelated fate and win-lose difference in shared fate
//test 1.messenger#1.lostT1 - 

//Unrelated condition
lincom _cons + 1.messenger //messenger, win
lincom _cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger //messenger, lose

lincom _cons //non-messenger, win
lincom _cons + 1.lostT1 //non-messenger, lose

test (_cons + 1.messenger) - (_cons) = 0 //messenger - nonmessenger, win
test (_cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger) - (_cons + 1.lostT1) = 0 //messenger - nonmessenger, lose

//difference-in-differences
test ((_cons + 1.messenger) - (_cons)) - ((_cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger) - (_cons + 1.lostT1)) = 0

//Shared condition
lincom _cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 //messenger, win
lincom _cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 //messenger, lose
	          
lincom _cons + 1.Instructions2 //non-messenger, win
lincom _cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2 //non-messenger, lose

test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.Instructions2) = 0 
test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2) = 0																 

//difference-in-differences
test ((_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.Instructions2)) - ((_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2)) = 0
																				 
//Opposite condition
lincom _cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 //messenger, win
lincom _cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 //messenger, lose
 
lincom _cons + 2.Instructions2 //non-messenger, win
lincom _cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2 //non-messenger, lose	

test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 2.Instructions2) = 0 
test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2) = 0

//difference-in-differences
test ((_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 2.Instructions2)) - ((_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2)) = 0


//check effect of game order
reg behavior_all i.messenger##i.lostT1##i.Instructions2 gameOrder, robust



//ATTITUDES
//pooled regression
reg likePartner i.messenger##i.lostT1##i.Instructions2, robust
//outreg2 using bey.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1#1.messenger = 0 //difference in slopes
test 1.Instructions2 + 1.lostT1#1.Instructions2 + 1.messenger#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 = 0
test 1.Instructions2 + 1.messenger#1.Instructions2 = 0

test 2.Instructions2 + 1.lostT1#2.Instructions2 + 1.messenger#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0
test 2.Instructions2 + 1.messenger#2.Instructions2 = 0

//Unrelated condition
lincom _cons + 1.messenger //messenger, win
lincom _cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger //messenger, lose

test (_cons + 1.messenger) - (_cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger) = 0

lincom _cons //non-messenger, win
lincom _cons + 1.lostT1 //non-messenger, lose

test (_cons) - (_cons + 1.lostT1) = 0

test (_cons + 1.messenger) - (_cons) = 0 //messenger - nonmessenger, win
test (_cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger) - (_cons + 1.lostT1) = 0 //messenger - nonmessenger, lose

//difference-in-differences
test ((_cons + 1.messenger) - (_cons)) - ((_cons + 1.messenger + 1.lostT1 + 1.lostT1#1.messenger) - (_cons + 1.lostT1)) = 0

//Shared condition
lincom _cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 //messenger, win
lincom _cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 //messenger, lose

test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) = 0
	          
lincom _cons + 1.Instructions2 //non-messenger, win
lincom _cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2 //non-messenger, lose

test (_cons + 1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2) = 0

test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.Instructions2) = 0 
test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2) = 0																 

//difference-in-differences
test ((_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.Instructions2)) - ((_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2)) = 0
																				 
//Opposite condition
lincom _cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 //messenger, win
lincom _cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 //messenger, lose
 
test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) = 0
 
lincom _cons + 2.Instructions2 //non-messenger, win
lincom _cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2 //non-messenger, lose	

test (_cons + 2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2) = 0

test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 2.Instructions2) = 0 
test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2) = 0

//difference-in-differences
test ((_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 2.Instructions2)) - ((_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2)) = 0




***********************************
* Analysis for Study 2 - Receiver *
***********************************
/*
Stata coding for Instructions1 variable:
0 "Zero"
1 "0.05 Shared"
2 "0.05 Opposite"
3 "0.50 Shared"
4 "0.50 Opposite"
*/

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
distplot likePartner, over(lostT1) by(messenger) title("Positive Attitudes toward the Messenger")

**************
* Histograms *
**************
//by win/lose
sort lostT1
*DG
//
by lostT1: sum dictator_messenger
hist dictator_messenger, by(lostT1) percent ylabel(0(10)50) xlabel (0(10)50) yline(17.75, lpattern(shortdash) lcolor(red)) title("Messenger")
graph save dM.gph, replace

by lostT1: sum dictator_other
hist dictator_other, by(lostT1) percent ylabel(0(10)50) xlabel (0(10)50) yline(17.75, lpattern(shortdash) lcolor(red)) title("Other")
graph save dO.gph, replace

graph combine dM.gph dO.gph

*STW
by lostT1: sum spin_messenger
hist spin_messenger, by(lostT1) percent ylabel(0(10)70) xlabel (0 "PC" 1 "Human") xtitle("Choose foor wheel spin") yline(62, lpattern(shortdash) lcolor(red))  title("Messenger")
graph save sM.gph, replace

by lostT1: sum spin_other
hist spin_other, by(lostT1) percent ylabel(0(10)70) xlabel (0 "PC" 1 "Human") xtitle("Choose foor wheel spin") yline(50, lpattern(shortdash) lcolor(red)) title("Other")
graph save sO.gph, replace

graph combine sM.gph sO.gph


*PD
by lostT1: sum pd_messenger
hist pd_messenger, by(lostT1) percent ylabel(0(10)80) xlabel (0 "D" 1 "C") xtitle("Defect (D) or Cooperate (C)") yline(76, lpattern(shortdash) lcolor(red)) title("Messenger")
graph save pdM.gph, replace

by lostT1: sum pd_other
hist pd_other, by(lostT1) percent ylabel(0(10)80) xlabel (0 "D" 1 "C") xtitle("Defect (D) or Cooperate (C)") yline(71, lpattern(shortdash) lcolor(red)) title("Other")
graph save pdO.gph, replace

graph combine pdM.gph pdO.gph


*Cumulative measure
by lostT1: sum behavior_m
hist behavior_m, by(lostT1) percent ylabel(0(10)40) xlabel (-1.4 "-1.4" 0 "0" 1.4 "1.4") title("Messenger")
graph save behaviorM.gph, replace

by lostT1: sum behavior_o
hist behavior_o, by(lostT1) percent ylabel(0(10)40) xlabel (-1.4 "-1.4" 0 "0" 1.43 "1.4") title("Other")
graph save behaviorO.gph, replace

graph combine behaviorM.gph behaviorO.gph



*attitudes
hist likeMessengerTotal, by(lostT1) percent ylabel(0(10)30) title("Messenger")
graph save aM.gph, replace

hist likePartner, by(lostT1) percent ylabel(0(10)30) title("Other")
graph save aO.gph, replace

graph combine aM.gph aO.gph



//indexes by neutral, shared, opposed

*Behavioral index
//neutral
distplot behavior_all if (Instructions2 == 0 & messenger == 1), over(lostT1) xtitle("Prosocial behavior") ytitle("Cumulative probability") title("Neutral Interests")
graph save bNeutral.gph, replace

//shared
distplot behavior_all if (Instructions2 == 1 & messenger == 1), over(lostT1) xtitle("Prosocial behavior") ytitle("Cumulative probability") title("Shared Interests") 
graph save bShared.gph, replace

//opposite
distplot behavior_all if (Instructions2 == 2 & messenger == 1), over(lostT1) xtitle("Prosocial behavior") ytitle("Cumulative probability") title("Opposite Interests") 
graph save bOpposite.gph, replace

graph combine bNeutral.gph bShared.gph
graph combine bNeutral.gph bOpposite.gph



*attitudes
distplot likePartner, over(lostT1) by(messenger) title("Positive Attitudes toward the Messenger")

*Attitudes index
//neutral
distplot likePartner if (Instructions2 == 0 & messenger == 1), over(lostT1) xtitle("Positive attitudes") ytitle("Cumulative probability") title("Neutral Interests")
graph save aNeutral.gph, replace

//shared
distplot likePartner if (Instructions2 == 1 & messenger == 1), over(lostT1) xtitle("Positive attitudes") ytitle("Cumulative probability") title("Shared Interests") 
graph save aShared.gph, replace

//opposite
distplot likePartner if (Instructions2 == 2 & messenger == 1), over(lostT1) xtitle("Positive attitudes") ytitle("Cumulative probability") title("Opposite Interests") 
graph save aOpposite.gph, replace

graph combine aNeutral.gph aShared.gph
graph combine aNeutral.gph aOpposite.gph

sum imagineLose
tab imagineLose

sum imagineWin
tab imagineWin

hist imagineWin, width(0.8) title("Imagine Win") percent
graph save win.gph, replace

hist imagineLose, width(0.8) title("Imagine Lose") percent
graph save lose.gph, replace

graph combine win.gph lose.gph, ycommon

sum reward
tab reward

sum punish
tab punish

hist reward, width(0.7) title("Reward") percent
graph save reward.gph, replace

hist punish, width(0.7) title("Punish") percent
graph save punish.gph, replace

graph combine reward.gph punish.gph, ycommon

sort Instructions2
by Instructions2: sum reward
by Instructions2: sum punish
by Instructions2: sum imagineWin
by Instructions2: sum imagineLose

sort lostT1
by lostT1: sum reward
by lostT1: sum punish
by lostT1: sum imagineWin
by lostT1: sum imagineLose 

sort messenger
by messenger: sum reward
by messenger: sum punish
by messenger: sum imagineWin
by messenger: sum imagineLose 

**********************
* Figure 1 - Indexes *
**********************
//POOLED
//behavioral index
sort lostT1 messenger

ttest behavior_all if messenger == 1, by (lostT1)
ttest behavior_all if messenger == 0, by (lostT1)

ttest behavior_all if lostT1 == 1, by (messenger)
ttest behavior_all if lostT1 == 0, by (messenger)

//attitudes index
ttest likePartner if messenger == 1, by (lostT1)
ttest likePartner if messenger == 0, by (lostT1)

ttest likePartner if lostT1 == 1, by (messenger)
ttest likePartner if lostT1 == 0, by (messenger)


//UNRELATED
ttest behavior_all if messenger == 1 & Instructions1 == 0, by (lostT1)
ttest behavior_all if messenger == 0 & Instructions1 == 0, by (lostT1)

ttest behavior_all if lostT1 == 1 & Instructions1 == 0, by (messenger)
ttest behavior_all if lostT1 == 0 & Instructions1 == 0, by (messenger)

//attitudes index
ttest likePartner if messenger == 1 & Instructions1 == 0, by (lostT1)
ttest likePartner if messenger == 0 & Instructions1 == 0, by (lostT1)

ttest likePartner if lostT1 == 1 & Instructions1 == 0, by (messenger)
ttest likePartner if lostT1 == 0 & Instructions1 == 0, by (messenger)


//SHARED
ttest behavior_all if messenger == 1 & (Instructions1 == 1 | Instructions1 == 3), by (lostT1)
ttest behavior_all if messenger == 0 & (Instructions1 == 1 | Instructions1 == 3), by (lostT1)

ttest behavior_all if lostT1 == 1 & (Instructions1 == 1 | Instructions1 == 3), by (messenger)
ttest behavior_all if lostT1 == 0 & (Instructions1 == 1 | Instructions1 == 3), by (messenger)

//compare unrelated and shared
ttest behavior_all if messenger == 1 & lostT1==0 & Instructions2 < 2, by (Instructions2)
ttest behavior_all if messenger == 1 & lostT1==1 & Instructions2 < 2, by (Instructions2)

//attitudes index
ttest likePartner if messenger == 1 & (Instructions1 == 1 | Instructions1 == 3), by (lostT1)
ttest likePartner if messenger == 0 & (Instructions1 == 1 | Instructions1 == 3), by (lostT1)

ttest likePartner if lostT1 == 1 & (Instructions1 == 1 | Instructions1 == 3), by (messenger)
ttest likePartner if lostT1 == 0 &(Instructions1 == 1 | Instructions1 == 3), by (messenger)

//compare unrelated and shared
ttest likePartner if messenger == 1 & lostT1==0 & Instructions2 < 2, by (Instructions2)
ttest likePartner if messenger == 1 & lostT1==1 & Instructions2 < 2, by (Instructions2)


//OPPOSITE
ttest behavior_all if messenger == 1 & (Instructions1 == 2 | Instructions1 == 4), by (lostT1)
ttest behavior_all if messenger == 0 & (Instructions1 == 2 | Instructions1 == 4), by (lostT1)

ttest behavior_all if lostT1 == 1 & (Instructions1 == 2 | Instructions1 == 4), by (messenger)
ttest behavior_all if lostT1 == 0 & (Instructions1 == 2 | Instructions1 == 4), by (messenger)

//compare unrelated and opposite
ttest behavior_all if messenger == 1 & lostT1==0 & Instructions2 != 1, by (Instructions2)
ttest behavior_all if messenger == 1 & lostT1==1 & Instructions2 != 1, by (Instructions2)

//attitudes index
ttest likePartner if messenger == 1 & (Instructions1 == 2 | Instructions1 == 4), by (lostT1)
ttest likePartner if messenger == 0 & (Instructions1 == 2 | Instructions1 == 4), by (lostT1)

ttest likePartner if lostT1 == 1 & (Instructions1 == 2 | Instructions1 == 4), by (messenger)
ttest likePartner if lostT1 == 0 & (Instructions1 == 2 | Instructions1 == 4), by (messenger)

//compare unrelated and opposite
ttest likePartner if messenger == 1 & lostT1==0 & Instructions2 != 1, by (Instructions2)
ttest likePartner if messenger == 1 & lostT1==1 & Instructions2 != 1, by (Instructions2)

****************************************************
* Shoot AND reward the messenger effect - ALL DATA *
****************************************************
//Dictator: both
reg dictator_all i.messenger##i.lostT1, robust

//Spin: only reward the messenger
logit spin_all i.messenger##i.lostT1

//Prisoner: only reward the messenger
logit pd_all i.messenger##i.lostT1

//Prosocial behavior index: both
reg behavior_all i.messenger##i.lostT1, robust
//outreg2 using beh.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win

test 1.lostT1#1.messenger = 0 //difference in slopes

//Attitudes: both
reg likePartner i.lostT1##i.messenger, robust
//outreg2 using beh.doc, sideway stats(coef se aster) dec(2)

matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win

test 1.lostT1#1.messenger = 0 //difference in slopes

**********************************************************
* Shoot AND reward the messenger effect - Unrelated fate *
**********************************************************
/*
NOTE: This is not just a perfect replication! 
We specified that messenger does not earn any money for delivering message
We had not specified this in Study 1.
*/

//Dictator: neither
reg dictator_all i.lostT1##i.messenger if Instructions1 == 0, robust

//Spin: only reward the messenger
logit spin_all i.lostT1##i.messenger if Instructions1 == 0

//Prisoner: neither
logit pd_all i.lostT1##i.messenger if Instructions1 == 0

//Prosocial behavior index: marginal reward the messenger
reg behavior_all i.lostT1##i.messenger if Instructions1 == 0, robust
//outreg2 using beh0.doc, sideway stats(coef se aster) dec(2)

matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win
test 1.lostT1#1.messenger = 0 //difference in slopes

//Attitudes: both
reg likePartner i.lostT1##i.messenger if Instructions1 == 0, robust
//outreg2 using beh0.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win

test 1.lostT1#1.messenger = 0 //difference in slopes

***********************************************
* compare effect of common fate vs. unrelated *
***********************************************
//Zero vs. 0.05 shared
//Dictator: all n.s.
reg dictator_all i.Instructions1##i.messenger##i.lostT1 if Instructions1 < 2, robust

//Spin: all n.s.
logit spin_all i.Instructions1##i.messenger##i.lostT1 if Instructions1 < 2

//Prisoner: all n.s.
logit pd_all i.Instructions1##i.messenger##i.lostT1 if Instructions1 < 2

//Prosocial behavior index: n.s.
reg behavior_all i.Instructions1##i.messenger##i.lostT1 if Instructions1 < 2, robust

//Attitudes: all n.s.
reg likePartner i.Instructions1##i.messenger##i.lostT1 if Instructions1 < 2, robust


//Zero vs. 0.50 shared
//Dictator: all n.s.
reg dictator_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 3), robust

//Spin: all n.s.
logit spin_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 3)

//Prisoner: all n.s.
logit pd_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 3)

//Prosocial behavior index: n.s.
reg behavior_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 3), robust

//Attitudes: all n.s.
reg likePartner i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 3), robust



//Zero vs. 0.05 opposite
//Dictator: all n.s.
reg dictator_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 2), robust

//Spin: more STM & more RTM
logit spin_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 2)

//Prisoner: all n.s.
logit pd_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 2)

//Prosocial behavior index: n.s.
reg behavior_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 2), robust

//Attitudes: all n.s.
reg likePartner i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 2), robust


//Zero vs. 0.50 opposite
//Dictator: all n.s.
reg dictator_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 4), robust

//Spin: n.s.
logit spin_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 4)

//Prisoner: all n.s.
logit pd_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 4)

//Prosocial behavior index: n.s.
reg behavior_all i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 4), robust

//Attitudes: all n.s.
reg likePartner i.Instructions1##i.messenger##i.lostT1 if (Instructions1 == 0 | Instructions1 == 4), robust



**********************************************************************
/*
Question: Can we ameliorate the Shoot the Messenger effect (STM)?
*/
//0.05 shared
//Dictator: only reward the messenger
reg dictator_all i.messenger##i.lostT1 if Instructions1 == 1, robust

//Spin: only reward the messenger
logit spin_all i.messenger##i.lostT1 if Instructions1 == 1

//Prisoner: neither
logit pd_all i.messenger##i.lostT1 if Instructions1 == 1

//Prosocial behavior index: only reward the messenger
reg behavior_all i.messenger##i.lostT1 if Instructions1 == 1, robust

//Attitudes: only reward the messenger
reg likePartner i.messenger##i.lostT1 if Instructions1 == 1, robust


//0.50 shared
//Dictator: only reward the messenger
reg dictator_all i.messenger##i.lostT1 if Instructions1 == 3, robust

//Spin: only reward the messenger
logit spin_all i.messenger##i.lostT1 if Instructions1 == 3

//Prisoner: only reward the messenger
logit pd_all i.messenger##i.lostT1 if Instructions1 == 3

//Prosocial behavior index: only reward the messenger
reg behavior_all i.messenger##i.lostT1 if Instructions1 == 3, robust

//Attitudes: both
reg likePartner i.messenger##i.lostT1 if Instructions1 == 3, robust

********************************************************************
/*
Question: Does Shoot the Messenger effect (STM) occur/worsen?
Also, does reward the messenger effect (RTM) disappear?
*/
//0.05 opposite
//Dictator: only reward the messenger
reg dictator_all i.messenger##i.lostT1 if Instructions1 == 2, robust

//Spin: both
logit spin_all i.messenger##i.lostT1 if Instructions1 == 2

//Prisoner: neither
logit pd_all i.messenger##i.lostT1 if Instructions1 == 2

//Prosocial behavior index: both
reg behavior_all i.messenger##i.lostT1 if Instructions1 == 2, robust

//Attitudes: both
reg likePartner i.messenger##i.lostT1 if Instructions1 == 2, robust


//0.50 opposite
//Dictator: only shoot the messenger
reg dictator_all i.messenger##i.lostT1 if Instructions1 == 4, robust

//Spin: neither
logit spin_all i.messenger##i.lostT1 if Instructions1 == 4

//Prisoner: neither
logit pd_all i.messenger##i.lostT1 if Instructions1 == 4

//Prosocial behavior index: neither
reg behavior_all i.messenger##i.lostT1 if Instructions1 == 4, robust

//Attitudes: only shoot the messenger
reg likePartner i.messenger##i.lostT1 if Instructions1 == 4, robust




*******************************************
* All the common fate conditions together *
*******************************************

/*
Goal: ameliorate Shoot the Messenger effect (STM).
If effective, we should see STM effect disappear.
*/
//Dictator: only reward the messenger
reg dictator_all i.messenger##i.lostT1 if (Instructions1 == 1 | Instructions1 == 3), robust

//Spin: only reward the messenger
logit spin_all i.messenger##i.lostT1 if (Instructions1 == 1 | Instructions1 == 3)

//Prisoner: only reward the messenger
logit pd_all i.messenger##i.lostT1 if (Instructions1 == 1 | Instructions1 == 3)

//Prosocial behavior index: only reward the messenger
reg behavior_all i.lostT1##i.messenger if (Instructions1 == 1 | Instructions1 == 3), robust
//outreg2 using beh1.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win
test 1.lostT1#1.messenger = 0 //difference in slopes


//Attitudes: both
reg likePartner i.lostT1##i.messenger if (Instructions1 == 1 | Instructions1 == 3), robust
//outreg2 using beh1.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win
test 1.lostT1#1.messenger = 0 //difference in slopes


*********************************************
* All the opposite fate conditions together *
*********************************************
/*
Goal: check whether Shoot the Messenger effect (STM) occurs/worsens.
Also, check whether reward the messenger effect (RTM) disappears.
If effective, we should see STM effect occur/worsen and RTM disappear.
*/
//0.05 opposite
//Dictator: only shoot the messenger
reg dictator_all i.messenger##i.lostT1 if (Instructions1 == 2 | Instructions1 == 4), robust

//Spin: only reward the messenger
logit spin_all i.messenger##i.lostT1 if (Instructions1 == 2 | Instructions1 == 4)

//Prisoner: neither
logit pd_all i.messenger##i.lostT1 if (Instructions1 == 2 | Instructions1 == 4)

//Prosocial behavior index: only reward the messenger
reg behavior_all i.lostT1##i.messenger if (Instructions1 == 2 | Instructions1 == 4), robust
//outreg2 using beh2.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win
test 1.lostT1#1.messenger = 0 //difference in slopes


//Attitudes: both
reg likePartner i.lostT1##i.messenger if (Instructions1 == 2 | Instructions1 == 4), robust
//outreg2 using beh2.doc, sideway stats(coef se aster) dec(2)
matrix coeff = e(b)
matrix list coeff
test 1.lostT1 +  1.lostT1#1.messenger = 0 //effect of losing when messenger
test 1.lostT1 = 0 //effect of losing when other
test 1.messenger + 1.lostT1#1.messenger = 0 //difference between money to messenger vs. other if lost
test 1.messenger  = 0 //difference of money given to messenger vs. other if win
test 1.lostT1#1.messenger = 0 //difference in slopes

//////////////
reg behavior_all i.messenger##i.lostT1##i.Instructions2, robust
//////////////

*********************************************************************
* Compare all common and opposite fate conditions to unrelated fate *
*********************************************************************
//Dictator: no difference
reg dictator_all i.Instructions2##i.messenger##i.lostT1, robust

//Spin: no difference
logit spin_all i.Instructions2##i.messenger##i.lostT1

//Prisoner: no difference
logit pd_all i.Instructions2##i.messenger##i.lostT1

//Prosocial behavior index: only reward the messenger: : no difference
reg behavior_all i.Instructions2##i.messenger##i.lostT1, robust

//Attitudes: no difference
reg likePartner i.Instructions2##i.messenger##i.lostT1, robust


********************************************************
* Compare all common fate conditions to unrelated fate *
********************************************************
//Dictator: all n.s.
reg dictator_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 < 2, robust

//Spin: all n.s.
logit spin_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 < 2

//Prisoner: all n.s.
logit pd_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 < 2

//Prosocial behavior index: only reward the messenger: : no difference
reg behavior_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 < 2, robust

//Attitudes: all n.s.
reg likePartner i.Instructions2##i.messenger##i.lostT1 if Instructions2 < 2, robust


**********************************************************
* Compare all opposite fate conditions to unrelated fate *
**********************************************************
//Dictator: all n.s.
reg dictator_all i.Instructions2##i.messenger##i.lostT1 if (Instructions2 == 0 | Instructions2 == 2), robust

//Spin: all n.s.
logit spin_all i.Instructions2##i.messenger##i.lostT1 if (Instructions2 == 0 | Instructions2 == 2)

//Prisoner: all n.s.
logit pd_all i.Instructions2##i.messenger##i.lostT1 if (Instructions2 == 0 | Instructions2 == 2)

//Prosocial behavior index: only reward the messenger: : no difference
reg behavior_all i.Instructions2##i.messenger##i.lostT1 if (Instructions2 == 0 | Instructions2 == 2), robust

//Attitudes: all n.s.
reg likePartner i.Instructions2##i.messenger##i.lostT1 if (Instructions2 == 0 | Instructions2 == 2), robust


******************************************************************
* Compare all opposite fate conditions to shared fate conditions *
******************************************************************
//Dictator: all n.s.
reg dictator_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 > 0, robust

//Spin: all n.s.
logit spin_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 > 0

//Prisoner: all n.s.
logit pd_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 > 0

//Prosocial behavior index: only reward the messenger: : no difference
reg behavior_all i.Instructions2##i.messenger##i.lostT1 if Instructions2 > 0, robust

//Attitudes: all n.s.
reg likePartner i.Instructions2##i.messenger##i.lostT1 if Instructions2 > 0, robust

log close
translate study2.smcl study2.pdf

***********************************************************

sort Instructions2 lostT1
by Instructions2: sum dictator_messenger if lostT1 == 0
by Instructions2: sum dictator_other if lostT1 == 0

by Instructions2: sum dictator_messenger if lostT1 == 1
by Instructions2: sum dictator_other if lostT1 == 1

***********************************************************

//by win/lose
sort lostT1
*DG
hist dictator_messenger, by(lostT1) percent ylabel(0(10)50) xlabel (0(10)50)
graph save dM.gph, replace

hist dictator_other, by(lostT1) percent ylabel(0(10)50) xlabel (0(10)50)
graph save dO.gph, replace

graph combine dM.gph dO.gph, ycommon

*STW
hist spin_messenger, by(lostT1) percent ylabel(0(10)50) xlabel (0 "PC" 1 "Human") xtitle("Choose foor wheel spin") title("Messenger")
graph save sM.gph, replace

hist spin_other, by(lostT1) percent ylabel(0(10)50) xlabel (0 "PC" 1 "Human") xtitle("Choose foor wheel spin") title("Other")
graph save sO.gph, replace

graph combine sM.gph sO.gph, ycommon


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

*************
* Table A10 *
*************
//demographics
sum age
sum education
sum ideology
sum pid

sum believeLuck
sum emotionReg
sum justWorld

tab pid
tab race
tab hispanic
tab sex

tab leaners
tab strongPID


******************************************************
* Table A11 - Balance tests using  Conditional logit *
******************************************************
*(1) lostT1
clogit lostT1 age female ideology education employed white hispanic believeLuck emotionReg justWorld, group(condition)
outreg2 using aza.doc, sideway stats(coef se aster) dec(2)


***********************************
* Heterogeneity (nothing to show) *
***********************************
/////////////////////////////////
//all fate conditions combined //
/////////////////////////////////
//using behavioral index
reg behavior_all i.lostT1##i.messenger##c.justWorld, robust
outreg2 using czy.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.emotionReg, robust
outreg2 using czx.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.believeLuck, robust
outreg2 using czu.doc, sideway stats(coef se aster) dec(2)

//using attitudes index
reg likePartner i.lostT1##i.messenger##c.justWorld, robust
outreg2 using cz0.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.emotionReg, robust
outreg2 using cz1.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.believeLuck, robust
outreg2 using cz2.doc, sideway stats(coef se aster) dec(2)

*****************************
* Regressions with controls *
*****************************
reg behavior_all i.lostT1##i.messenger justWorld emotionReg believeLuck female age ideology education  employed white hispNew, robust
outreg2 using cze.doc, sideway stats(coef se aster) dec(2)
reg likePartner i.lostT1##i.messenger justWorld emotionReg believeLuck female age ideology education  employed white hispNew, robust
outreg2 using czf.doc, sideway stats(coef se aster) dec(2)


/////////////////////////////
//unrelated fate condition //
/////////////////////////////
//using behavioral index
reg behavior_all i.lostT1##i.messenger##c.justWorld if Instructions1 == 0, robust
outreg2 using cz39.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.emotionReg if Instructions1 == 0, robust
outreg2 using cz49.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.believeLuck if Instructions1 == 0, robust
outreg2 using cz59.doc, sideway stats(coef se aster) dec(2)

//using attitudes index
reg likePartner i.lostT1##i.messenger##c.justWorld if Instructions1 == 0, robust
outreg2 using cz69.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.emotionReg if Instructions1 == 0, robust
outreg2 using cz79.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.believeLuck if Instructions1 == 0, robust
outreg2 using cz89.doc, sideway stats(coef se aster) dec(2)

///////////////////////////
//shared fate conditions //
///////////////////////////
//using behavioral index
reg behavior_all i.lostT1##i.messenger##c.justWorld if (Instructions1 == 1 | Instructions1 == 3), robust
outreg2 using cz3.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.emotionReg if (Instructions1 == 1 | Instructions1 == 3), robust
outreg2 using cz4.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.believeLuck if (Instructions1 == 1 | Instructions1 == 3), robust
outreg2 using cz5.doc, sideway stats(coef se aster) dec(2)

//using attitudes index
reg likePartner i.lostT1##i.messenger##c.justWorld if (Instructions1 == 1 | Instructions1 == 3), robust
outreg2 using cz6.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.emotionReg if (Instructions1 == 1 | Instructions1 == 3), robust
outreg2 using cz7.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.believeLuck if (Instructions1 == 1 | Instructions1 == 3), robust
outreg2 using cz8.doc, sideway stats(coef se aster) dec(2)


/////////////////////////////
//opposite fate conditions //
/////////////////////////////
//using behavioral index
reg behavior_all i.lostT1##i.messenger##c.justWorld if (Instructions1 == 2 | Instructions1 == 4), robust
outreg2 using cz67.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.emotionReg if (Instructions1 == 2 | Instructions1 == 4), robust
outreg2 using cz77.doc, sideway stats(coef se aster) dec(2)

reg behavior_all i.lostT1##i.messenger##c.believeLuck if (Instructions1 == 2 | Instructions1 == 4), robust
outreg2 using cz87.doc, sideway stats(coef se aster) dec(2)

//using attitudes index
reg likePartner i.lostT1##i.messenger##c.justWorld if (Instructions1 == 2 | Instructions1 == 4), robust
outreg2 using cz9.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.emotionReg if (Instructions1 == 2 | Instructions1 == 4), robust
outreg2 using cz10.doc, sideway stats(coef se aster) dec(2)

reg likePartner i.lostT1##i.messenger##c.believeLuck if (Instructions1 == 2 | Instructions1 == 4), robust
outreg2 using cz11.doc, sideway stats(coef se aster) dec(2)
