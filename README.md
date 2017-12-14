# tic-tac-game - Client and Server
Wild Tic Tac Toe game client and server written in Haskell.

Client and server uses Bencode file format to communicate.
Each move has 3 data parameters:  
- **id** - player identifier  
- **c** - move coordinates - from (0,0) to (2,2)   
- **v** - incidates what move was made - either *x* or *o*.  
All moves except the first move also has:  
- **prev** - stores all previous moves  
  
Bencode game board example:  
>d1:cli0ei2ee2:id1:w4:prevd1:cli2ei1ee2:id5:WgEYE4:prevd1:cli1ei1ee2:id1:w1:v1:xe1:v1:oe1:v1:xe
 
Here **cli0ei2ee** shows move coordinates (0,2)  
**id** w and WgEYE  
**prev** shows previous move. All previous moves are included with current move recursively.  
This is an example how it recursively saves move history if there are 4 total moves on the board:  
<Move 4 data + previous<Move 3 data + previous<Move 2 data + previous<Move 1 data>>>>

