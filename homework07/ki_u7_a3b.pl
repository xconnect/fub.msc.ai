/**************************************************************************************/
/* Wir verwenden hier die board(X1,X2,X3,X4,X5,X6,X7,X8,X9) Darstellung.              */
/**************************************************************************************/

/* Idee: move(OldState, NewState) */

move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_X > 1, moveLeft([OS_X/OS_Y|OS_Y_Rest], NewState).
move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_X < 3, moveRight([OS_X/OS_Y|OS_Y_Rest], NewState).
move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_Y > 1, moveDown([OS_X/OS_Y|OS_Y_Rest], NewState).
move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_Y < 3, moveUp([OS_X/OS_Y|OS_Y_Rest], NewState).

/* moveDir bewegt jetzt halt das Blank nach links, rechts, obn bzw. unten */
moveLeft ...
moveRight ...
moveUp ...
moveDown ...