reg behavior_all i.messenger##i.lostT1##i.Instructions2 if Instructions2==0, robust //unrelated

reg behavior_all i.messenger##i.lostT1##i.Instructions2 if Instructions2==1, robust //shared

reg behavior_all i.messenger##i.lostT1##i.Instructions2 if Instructions2==2, robust  //opposite

//MESSENGERS
***********************
* Unrelated condition *
***********************
test (_cons + 1.messenger) - (_cons + 1.messenger + 1.lostT1 + 1.messenger#1.lostT1) = 0 //original equation: difference win-lose, unrelated condition
test 1.lostT1 + 1.messenger#1.lostT1 = 0 //difference win-lose, unrelated condition

lincom _cons + 1.messenger //messenger

********************
* Shared condition *
********************
test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) = 0 //original equation: difference win-lose, shared conditions
test 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 = 0 //difference win-lose, shared conditions

**********************
* Opposite condition *
**********************
test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) = 0 //original equation: difference win-lose, opposite conditions
test 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0 //difference win-lose, opposite conditions


//difference W-L unrelated - W-L shared
test (1.lostT1 + 1.messenger#1.lostT1) - (1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) = 0 //original equation
test 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 = 0 //short equation

//difference W-L unrelated - W-L opposite
test (1.lostT1 + 1.messenger#1.lostT1) - (1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) = 0 //original equation
test 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0

//difference W-L shared - W-L opposite
test (1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) = 0
test 1.lostT1#1.Instructions2 - 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#1.Instructions2 - 1.messenger#1.lostT1#2.Instructions2 = 0

//NON-MESSENGERS
***********************
* Unrelated condition *
***********************
test (_cons) - (_cons + 1.lostT1) = 0
test 1.lostT1 = 0 //difference win-lose, unrelated condition

********************
* Shared condition *
********************
test (_cons + 1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2) = 0
test 1.lostT1 + 1.lostT1#1.Instructions2 = 0 //difference win-lose, shared conditions

**********************
* Opposite condition *
**********************
test (_cons + 2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2) = 0
test 1.lostT1 + 1.lostT1#2.Instructions2 = 0 //difference win-lose, opposite conditions


//LOSING: MESSENGERS vs. NON-MESSENGERS
***********************
* Unrelated condition *
***********************
//test (_cons + 1.messenger + 1.lostT1 + 1.messenger#1.lostT1) - (_cons + 1.lostT1) = 0
test 1.messenger + 1.messenger#1.lostT1 = 0

********************
* Shared condition *
********************
//test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (_cons + 1.Instructions2 + 1.lostT1 + 1.lostT1#1.Instructions2) = 0
test 1.messenger + 1.messenger#1.lostT1 + 1.messenger#1.Instructions2  + 1.messenger#1.lostT1#1.Instructions2 = 0

**********************
* Opposite condition *
**********************
//test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) - (_cons + 2.Instructions2 + 1.lostT1 + 1.lostT1#2.Instructions2) = 0
test 1.messenger + 1.messenger#1.lostT1 + 1.messenger#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2 = 0


//WINNING: MESSENGERS vs. NON-MESSENGERS
***********************
* Unrelated condition *
***********************
//test (_cons + 1.messenger) - (_cons) = 0
test 1.messenger = 0

********************
* Shared condition *
********************
//test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.Instructions2) = 0
test 1.messenger + 1.messenger#1.Instructions2 = 0

**********************
* Opposite condition *
**********************
//test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 2.Instructions2) = 0
test 1.messenger + 1.messenger#2.Instructions2 = 0



****************************************************
* DIFFERENCES between MESSENGERS across conditions *
****************************************************
//shared - unrelated
//LOST
test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#1.Instructions2 + 1.messenger#1.lostT1#1.Instructions2) - (_cons + 1.messenger + 1.lostT1 + 1.messenger#1.lostT1) = 0

//WON
test (_cons + 1.messenger + 1.Instructions2 + 1.messenger#1.Instructions2) - (_cons + 1.messenger) = 0

//opposite - unrelated
//LOST
test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2 + 1.lostT1 + 1.messenger#1.lostT1 + 1.lostT1#2.Instructions2 + 1.messenger#1.lostT1#2.Instructions2) - (_cons + 1.messenger + 1.lostT1 + 1.messenger#1.lostT1) = 0

//WON
test (_cons + 1.messenger + 2.Instructions2 + 1.messenger#2.Instructions2) - (_cons + 1.messenger) = 0


//differences between 0.05 and 0.50
//SHARED
//behavior
sort lostT1
by lostT1: sum behavior_all if messenger == 1 & Instructions1 == 1 //0.05 shared
by lostT1: sum behavior_all if messenger == 1 & Instructions1 == 3 //0.50 shared

by lostT1: sum behavior_all if messenger == 0 & Instructions1 == 1 //0.05 shared
by lostT1: sum behavior_all if messenger == 0 & Instructions1 == 3 //0.50 shared


//attitudes
sort lostT1
by lostT1: sum likePartner if messenger == 1 & Instructions1 == 1 //0.05 shared
by lostT1: sum likePartner if messenger == 1 & Instructions1 == 3 //0.50 shared

by lostT1: sum likePartner if messenger == 0 & Instructions1 == 1 //0.05 shared
by lostT1: sum likePartner if messenger == 0 & Instructions1 == 3 //0.50 shared


//OPPOSITE
//behavior
sort lostT1
by lostT1: sum behavior_all if messenger == 1 & Instructions1 == 2 //0.05 shared
by lostT1: sum behavior_all if messenger == 1 & Instructions1 == 4 //0.50 shared

by lostT1: sum behavior_all if messenger == 0 & Instructions1 == 2 //0.05 shared
by lostT1: sum behavior_all if messenger == 0 & Instructions1 == 4 //0.50 shared


//attitudes
sort lostT1
by lostT1: sum likePartner if messenger == 1 & Instructions1 == 2 //0.05 shared
by lostT1: sum likePartner if messenger == 1 & Instructions1 == 4 //0.50 shared

by lostT1: sum likePartner if messenger == 0 & Instructions1 == 2 //0.05 shared
by lostT1: sum likePartner if messenger == 0 & Instructions1 == 4 //0.50 shared
