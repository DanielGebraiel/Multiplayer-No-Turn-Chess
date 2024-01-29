;27-12-2022
;This code plays Chezz
;Authors:
;Daniel Adel Aziz      
;David George Adel     
;Habiba Wael Nabil     
;Nardeen Hany Milad    
;Peter Ashraf Moussa   

;-_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- -_- 

.model small
.stack 300
.data
;=====================Chatting Vars======================
VALUE db 0

mes db 'hi$'
cols db 1
colr db 42
var db 0
rows db 0
rowr db 0
checkenter db 0
;=====================InLineChatting Vars======================
ChatCols db 0
ChatRows db 39d
Chatcolr db 3fh
ChatRowr db 39d
Ascii db ?
scanCode2 db 0
FirstLine db 160 dup(0)
;===========================================================
;===========================================================
WKingMovedFlag db 0
BKingMovedFlag db 0
LeftWTabyaMovedFlag db 0
RightWTabyaMovedFlag db 0
LeftBTabyaMovedFlag db 0
RightBTabyaMovedFlag db 0

;===========================================================

ChessBoardWidth EQU 600
ChessBoardHeight EQU 600
ChessPieceWidth EQU 75
ChessPieceHeight EQU 75

DarkCellColor EQU 03h
LightCellColor EQU 0fH

PlayerOneSelectedCellColor equ 05h
PlayerOneHighlightedCellColor equ 52h
PlayerOneHighlightedSelectedCellColor EQU 6fh
PlayerOneOnPlayerTwoHighlightColor equ 2ah

PlayerTwoSelectedCellColor equ 0ch
PlayerTwoHighlightedCellColor equ 0eh
PlayerTwoHighlightedSelectedCellColor EQU 06h
PlayerTwoOnPlayerOneHighlightColor equ 68h

OverLappedHighlightingColor equ 2fh
OverLappedHighlightedSelectedColor equ 2h

BlackORWhite db 1                                 ;0 ForBlack, 1 ForWhite
LeaveGameFlag db 0

RandomCellNumber dw 0                             ;Result of the random cell number generator
RandomNumber dw 0H                                ;Result of the random number generator

ENDX DW 0                                         ;End position of drawing for x-axis
ENDY DW 0                                         ;End position of drawing for y-axis
STARTX DW 0                                       ;Start of drawing for x-axis
OldPositionDrawingX dw 0                          ;To know the position of the old cell when selectring the pieces (x-axis)
OldPositionDrawingY dw 0                          ;To know the position of the old cell when selectring the pieces (y-axis)
OldPositionIndex dw 0                             ;To know the position of index when of the old cell when selecting pieces

PlayerOneSelectedPiecePosition dw 0ffffH,0ffffH   ;To know the position of the currently selected piece of player one         
PlayerTwoSelectedPiecePosition dw 0ffffH,0ffffH   ;To know the position of the currently selected piece of player one

PlayerOneHighlightedCells dw 30 dup(0ffffh)       ;To store all the player's highlighted cellsof each selection
PlayerTwoHighlightedCells dw 30 dup(0ffffh)

PlayerOneHighlightedCellsIndex dw 0
PlayerTwoHighlightedCellsIndex dw 0

WhiteKingPosition dw 04d, 56d
BlackKingPosition dw 04d,0d
OutsideBorders db 0
Notifications db '-------------------Notifications--------------------','$'
ChatLabel db '--------------------------------------------------------------Chat--------------------------------------------------------------$'
PressAnyKey db 'Press Any Key To Exit','$'
HasBonus db 00h                                   ; 2 if black got powerup, 1 if white
;=====================IMAGES======================;
PowerUp db 'PowerUp.bin',0
White_Tabya DB 'WTabya.bin',0        ; 
Black_Tabya DB 'BTabya.bin',0        ; 
White_Fil DB 'WFil.bin',0            ;
Black_Fil DB 'BFil.bin',0            ;
White_Hosan DB 'WHosan.bin',0        ;
Black_Hosan DB 'BHosan.bin',0        ;    
White_Askary DB 'WAskary.bin',0      ;
Black_Askary DB 'BAskary.bin',0      ;
White_King DB 'WKing.bin',0          ;
Black_King DB 'BKing.bin',0          ;
White_Queen DB 'WQueen.bin',0        ;
Black_Queen DB 'BQueen.bin',0        ;
CoolDown DB 'CoolDown.bin',0
QuarterJail db 'segn33.bin',0
HalfJail db 'segn22.bin',0
FullJail db 'segn11.bin',0

;================Status Bar Messages=================;
StatusBarCurrentRow db 4d
EnterBatchMode db 'Entered Batch Mode','$'
LeaveBatchMode db 'Left Batch Mode','$'
Clear db '                       ','$'
deadBAskary db 'Black Askary Died','$'
deadWAskary db 'White Askary Died','$'
deadBhosan db 'Black Hosan Died','$'
deadWhosan db 'White Hosan Died','$'
deadBtabya db 'Black Tabya Died','$'
deadWtabya db 'White Tabya Died','$'
deadBWazeer db 'Black Wazeer Died','$'
deadWWazeer db 'White Wazeer Died','$'
deadBfil db 'Black Fil Died','$'
deadWfil db 'White Fil Died','$'
keshWmalek db 'Kesh White Malek','$'
keshBmalek db 'Kesh Black Malek','$'
chatmodetext db 'This is chatting mode in phase 2 ISA','$'
whiteAskaryWazeer db 'White Askary Etra2a','$'
blackAskaryWazeer db 'Black Askary Etra2a','$'
SentGameInvitation db 'You sent a game invitation','$'
ReceivedGameInvitation db 'You received a game invitation','$'
SentChatInvitation db 'You sent a chat invitation','$'
ReceivedChatInvitation db 'You received a chat invitation','$'

;==========TIMER VARIABLES=============; So that time is calculated relative to them
StartingHour DB ?
StartingMinute DB ?
StartingSecond DB ?
CurrentHour DB 0
CurrentMinute DB 0
CurrentSecond Db 0
firsttimeclock db 0

;;;;;;;;;;;;;;;;;;;;;;;
;;;;Codes for Pieces;;;
;;;;;;;;;;;;;;;;;;;;;;;
; Each piece has 2 digits of code, one for colour and one for type
;0 For black and 1 for White (Left digit)
;1 Askary
;2 Tabya
;3 Hosan
;4 Fil
;5 Queen
;6 King
; For example:  01 is black askary

ChessboardGrid DB 02h, 03h, 04h, 05h, 06h, 04h, 03h, 02h
               DB 01h, 01h, 01h, 01h, 01h, 01h, 01h, 01h
               DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
               DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
               DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
               DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
               DB 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h
               DB 12h, 13h, 14h, 15h, 16h, 14h, 13h, 12h

DefaultChessboardGrid DB 02h, 03h, 04h, 05h, 06h, 04h, 03h, 02h
                      DB 01h, 01h, 01h, 01h, 01h, 01h, 01h, 01h
                      DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
                      DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
                      DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
                      DB 99h, 99h, 99h, 99h, 99h, 99h, 99h, 99h
                      DB 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h
                      DB 12h, 13h, 14h, 15h, 16h, 14h, 13h, 12h               
               
LockedCellPosition DB 64 dup(0ffh)         ;Array to indicate wether this cell is locked or not (1 if locked)
LockedCellTime DB 64 dup(88d)              ;Array to store the time this cell was locked              

FullJailDrawnFlag db 64 dup(0)
HalfJailDrawnFlag db 64 dup(0)
QuarterJailDrawnFlag db 64 dup(0)

BatchModeString db 16 dup('$')
BatchModeIndex dw 00H
BatchModeSize dw 00h
BatchModeFlag db 0h
BatchModeBX DW 00H
BatchModeDI DW 00H
firstTimeFs db 00h

iSPowerupGenerated db 0h
ProbabilityOfGeneration equ 1000d

;===========For Phase 2============;
DataToSend db 0,0
SkipWSend db 0
SkipBSend db 0
IsData db 0
SendOldOrNew db 0
ReadOldOrNew db 0
KingDied db 0
ChattingSent db 0
ChattingReceived db 0
GameSent db 0
GameReceived db 0
NotificationBarCurrentRow db 0
clear1 db '                                        ','$'
;==================================;

first db "For chatting mode press F1$"
second db "For play mode press F2 $"
third db "To exit press ESC $"
ending db 'Thank You ',03h,'$'

getname1 db "Enter Your Name: $"
player1 db 20,?,20 dup('$')
player2 db 20,?,20 dup('$')
PlayerWinningMessage db 'Wins ',02,'$'
WhiteWon db 'White Won','$'
BlackWon db 'Black Won','$'

FileHandle DW ? ; pointer pointing to the beginning of the file

PieceData DB ChessPieceWidth*ChessPieceHeight dup(0)

.code

main proc far
    mov ax,@data
    mov ds,ax

;============================Phase2================================
    ; initinalize COM
    ;Set Divisor Latch Access Bit
    mov dx,3fbh 			; Line Control Register
    mov al,10000000b		;Set Divisor Latch Access Bit
    out dx,al				;Out it
    ;Set LSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f8h			
    mov al,0ch			
    out dx,al

    ;Set MSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f9h
    mov al,00h
    out dx,al

    ;Set port configuration
    mov dx,3fbh
    mov al,00011011b
    out dx,al
;=====================================================================

fscreen:
    cmp firstTimeFs,00H
    je firstTime

    ;mov ah,0
    ;int 16h 
    jmp screen

    firstTime:
    mov firstTimeFs,1h
;new window
    mov ah,0
    mov al,13h
    int 10h 
;moving cursor          
    mov dl,40
    mov dh,5          
    mov ah,2
    mov al,13h
    int 10h

    mov dx,offset getname1
    call displaystring

;taking input from user
    mov ah,0AH 
    mov dx,offset player1
    int 21h 

;; moving cursor          
;     mov dl,40
;     mov dh,7         
;     mov ah,2
;     mov al,13h
;     int 10h
;     mov dx,offset getname2
;     call displaystring

; ;taking input from user
;     mov ah,0AH 
;     mov dx,offset player2
;     int 21h 

screen:
    ;new window
    mov ah,0
    mov al,13h
    int 10h 
    ;moving cursor          
    mov dl,45
    mov dh,11          
    mov ah,2
    mov al,13h
    int 10h
    
    mov dx,offset first
    call displaystring  
                         
    
   ;moving cursor 
    mov dl,45
    mov dh,14          
    mov ah,2
    mov al,13h
    int 10h 
    
    mov dx,offset second
    call displaystring    
    ;moving cursor 
    mov dl,45
    mov dh,17          
    mov ah,2
    mov al,13h
    int 10h  
    mov dx,offset third
    call  displaystring   
           
        
    ;taking input from user
    WaitingLoop:
        call ReceiveInvitation

        cmp ChattingReceived,1
        jne CheckGame
        cmp ChattingSent,1
        je chatmode

        CheckGame:
        cmp GameReceived,1
        jne continueWaitingLoop
        cmp GameSent,1
        je tempgamemode

        continueWaitingLoop:
        mov ah,1h
        int 16h
    jz WaitingLoop
    
    cmp ah, 03bh
    je F1Pressed

    cmp ah, 03ch
    je F2Pressed

    cmp ah, 01h
    je temp1endscreen

    endWaitingLoop:
    mov ah,0h
    int 16h
    jmp WaitingLoop

    F1Pressed:
    cmp ChattingSent,1
    je endWaitingLoop
    mov ChattingSent,1
    mov dx, offset SentChatInvitation
    call UpdateNotificationBar
    pop dx
    call SendInvitation
    jmp endWaitingLoop

    F2Pressed:
    cmp GameSent,1
    je endWaitingLoop
    mov GameSent,1
    mov dx, offset SentGameInvitation
    call UpdateNotificationBar
    pop dx
    call SendInvitation
    jmp endWaitingLoop
    tempgamemode: jmp gamemode
    temp1endscreen: jmp tempendscreen     

    chatmode:
     ;new window
    mov ChattingSent,0
    mov ChattingReceived,0
    ; mov ah,0
    ; mov al,13h
    ; int 10h 
    ; mov ah,02h
    ; mov dh,10d
    ; mov dl,2d
    ; int 10h
    ; mov dx,offset chatmodetext
    ;  call displaystring 
    ; mov ah,02h
    ; mov dh,12d
    ; mov dl,9d
    ; int 10h 
    ; mov dx, offset PressAnyKey
    ; call displaystring
    ; mov ah,0h
    ; int 16h
    mov ax,3H
int 10h
push ax
push dx
mov ah,2
mov dh ,0
mov dl, 40d
push dx
int 10h
SplitScreenloop:
        pop dx
        mov dl,40d
        inc dh
        push dx
        cmp dh, 25d
        jae endSplitScreen
        int 10h
        mov ah,2
        mov dl, '|'
        int 21h
jmp SplitScreenloop

endSplitScreen:
mov ah,2
mov dx,0000h
int 10h
pop dx
pop ax
; sending a value
;Check that Transmitter Holding Register is Empty
		;mov dx , 3FDH		; Line Status Register
        
LABEL1:
  		
        mov ah,1
        int 16h
        mov checkenter,al
        jZ CHKTemp
        cmp ah,50h
        ; je scrolldown
       
        ;jmp cont

        ;      scrolldown:
        ;      mov ah,07h
        ;      mov al,0
        ;      mov bh,07 
        ;      mov cx,0 
        ;      mov dx,184FH
        ;      int 10h 
        ;      mov ah,2h
        ;      int 21h
        ;      jmp LABEL1    
            

       ; cont:
		MOV CL,AL

        MOV AH,0Ch
        INT 21H

;If empty put the VALUE in Transmit data register
  		
        In al , dx 		;Read Line Status
  		AND al , 00100000b
  		JZ CHKTemp 

		mov dx , 3F8H		; Transmit data register
        MOV AL,CL
  		out dx , AL

        cmp al,92d
        jne NotDel
        mov cols,1
        mov colr ,41
        mov rows,00
        mov rowr,0
        jmp fscreen
       
        NotDel: 
        mov ah,2 
        mov dl,AL
        int 21h
        
        cmp scanCode2,50h
        jne conto
        call scroll_Up
        conto:
        cmp scanCode2,48h
        jne Conto2
        call scroll_down
        Conto2:
        cmp checkenter,13d
        jne checkbcksp
        je enterhere

CHKTemp: jmp CHK
tempendscreen: jmp endscreen
LABEL1Temp: jmp LABEL1
checkbcksp:
        cmp checkenter,08h
        je BackSp
        jmp continue4
        
        BackSp:
        cmp cols,0
        jne conty
        dec rows
        mov cols,38
          ;moving cursor 
         mov dl,cols
         mov dh,rows        
         mov ah,2
         int 10h 

            ; mov ah,2 
            ;  mov dl,08h
            ;  int 21h

        conty:
         mov ah,2 
         mov dl,20h
        int 21h
        
        mov ah,2 
        mov dl,08h
        int 21h

         dec cols
         dec cols
         ;moving cursor 
         mov dl,cols
         mov dh,rows        
         mov ah,2
         int 10h 

        ; mov ah, 9
        ; mov dx, offset Space
        ; int 21h
      
        jmp continue4
          LABEL1TempTemp: jmp LABEL1Temp
        
        
       ; cmp checkenter,27d
        ;je terminate
        enterhere:
        mov cols,0
        inc rows

        continue4:        ;moving cursor 
    mov dl,cols
    mov dh,rows        
    mov ah,2
    int 10h 
	cmp cols,38
	je reset
	jne continue
    
	reset:
	mov cols,0
	inc rows

	continue:
	inc cols

; receiving 
;Check that Data Ready
				; Line Status Register
	CHK:
	mov dx , 3FDH	
	    in al , dx 
  		AND al , 1
  		JZ LABEL1TempTemp

        ; cmp checkenter,13d
        ; jne continue3
        ; cmp checkenter,27d
        ; je terminate
        ; mov colr,42
        ; inc rowr
        ; continue3:

        ;moving cursor 
    mov dl,colr
    mov dh,rowr        
    mov ah,2
    int 10h 
	cmp colr,80
	je reset2
	jne continueChatting

	reset2:
	mov colr,41
	inc rowr

	continueChatting:
    inc colr

 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		;mov VALUE , al
        cmp al,92d
        jne notDelAgain
        mov cols,1
        mov colr, 41
        mov rows,0
        mov rowr,0
        jmp fscreen
        notDelAgain:
        cmp al,13d
        jne checkforbs 
        mov colr,41
        inc rowr
        mov dl,colr
        mov dh,rowr        
        mov ah,2
        int 10h 
        inc colr

checkforbs:
cmp al,08h
jne continue5

        cmp colr,38
        jne conte
        dec rowr
        mov colr,83
        ;moving cursor 
         mov dl,colr
         mov dh,rowr       
         mov ah,2
         int 10h 
        conte:
         mov ah,2 
         mov dl,20h
        int 21h
        
        mov ah,2 
        mov dl,08h
        int 21h

        dec colr
        dec colr
        ;moving cursor 
        mov dl,colr
        mov dh,rowr        
        mov ah,2
        int 10h 
      
        continue5:
        mov ah,2 
        mov dl,AL
        int 21h

JMP LABEL1

endscreen:
;new window
    mov ah,0
    mov al,13h
    int 10h 
    mov dh,10d
    mov dl,13d
    mov ah,02H
    int 10h
    mov dx,offset ending
    call displaystring
jmp endGame

    gamemode:
    mov LeaveGameFlag,0
    mov KingDied, 0
    mov ChatCols, 0
    mov ChatRows, 39d
    mov chatcolr, 3fh
    mov ChatRowr, 39d
    mov GameReceived,0
    mov HasBonus, 0
    mov GameSent,0
    mov StatusBarCurrentRow, 4
    call ResetChessBoard
    mov ax,4f02h                        ;Actvaites video mode
    mov bx, 0105h                       ;Makes the screen resolution 800x600
    int 10h
;-------------------------------------------------------------------------------------------------
; Opening and loading and drawing chess board
   
   CALL DrawChessBoard    

jmp skipaya
tempaya:
mov BlackORWhite, 1
jmp fscreen
skipaya:
mov si,0
mov cx,0
mov dx,0

call DrawAllPieces
;---------------------------------------------------------

cmp BlackORWhite, 1
je WhitePlayer
mov bx,0H                            ;putting our pointer on the top left cell
mov di, 0d
call GetCXDXFromBXDI                 ;Getting coordinates from the pointers
mov al, PlayerTwoSelectedCellColor

call DrawChessCell
call DrawChessPiece                  ;Draws chess piece if exists on this cell
mov bp, 0H
mov si, 0d
jmp ContinueGame

temptempaya:jmp tempaya

WhitePlayer:
mov bx,0H
mov di, 56d                          ;Same for white player
call GetCXDXFromBXDI
mov al, PlayerOneSelectedCellColor


call DrawChessCell
call DrawChessPiece

;============Assigning a specific part for ingame notifications========;
ContinueGame:
mov dx,034Bh
mov ah,02h
int 10h
mov dx, offset Notifications
mov ah, 9
int 21h

;==================Chatting label================================
mov dh, 38d
mov dl, 0d
mov ah,02
int 10h
mov dx, offset ChatLabel
mov ah,09
int 21h

;===========Getting system time at the beginning of the game=====;
MOV AH,2CH              ; To get System Time
INT 21H
MOV StartingHour,CH     ; Hour is in CH
MOV StartingMinute,CL   ; Minutes is in CL
MOV StartingSecond,0d   ; Seconds in DH

;==================MainLoop================;
Mainloop:
    call SendData
    call CheckIfDataRecieved                    ;Checks if the other player has sent any data
    call DisplayTime                            ;Updates timer
    call GenerateBonusItem                      ;Generates the time powerup randomly
    call CheckTime                              ;Keeps checking time during every main cycle to unlock locked cells
    call ExecuteBatchMode                       ;Keeps calling batchmode in order not to stop the game if it is being executed
    
    mov ah,1h
    int 16h
    jz Mainloop
    mov Ascii, al

    cmp LeaveGameFlag,1
    je temptempaya
    cmp KingDied, 1
    je ContinueMain
    cmp BatchModeFlag,1                         ;If White entered batch mode, only black can play using keys
    je ContinueMain
    cmp BlackORWhite, 0
    je BlackMoves

    cmp ah, 3Eh                                 ;Exit to main menu if F4 is pressed
    je temptempaya

    cmp ah, 4dh                                 ;if D is pressed white selector moves to the right
    je callWRight

    cmp ah, 4bh                                 ;if A is pressed white selector moves to the left
    je callWLeft

    cmp ah, 50h                                 ;if S is pressed white selector moves down
    je callWDown

    cmp ah, 48h                                 ;if W is pressed white selector moves up
    je callWUp

    cmp ah, 39H                                 ;If Q is pressed an action is taken
    je callWAction

    jmp ContinueMain

    BlackMoves:
    cmp ah, 4dh                                 ;Same for black but using arrow keys and space for action
    je callBRight
    
    cmp ah, 4bh
    je callBLeft

    cmp ah, 48h
    je callBUp

    cmp ah, 50h
    je callBDown

    cmp ah, 39H
    je callBAction

    ContinueMain:
    
    cmp al, 30h                                ;If 0 is pressed, batch mode flag is changed
    je callSwitchFlag

    cmp BatchModeFlag,1                        ;The player is in batch mode so the string containing his commands is updated
    je callEditBatchModeString

    call sendChat

    jmp endmainloop

    ;========================Labels Calling Moving Procs=======================;

    callWRight:
    call WRight
    jmp endmainloop

    callBRight:
    call BRight
    jmp endmainloop

    callWLeft:
    call WLeft
    jmp endmainloop


    callBLeft:
    call BLeft
    jmp endmainloop


    callWUp:
    call WUp
    jmp endmainloop


    callBUp:
    call BUp
    jmp endmainloop


    callWDown:
    call WDown
    jmp endmainloop


    callBDown:
    call BDown
    jmp endmainloop


    callSwitchFlag:
    call SwitchFlag
    jmp endmainloop


    callWAction:
    call WAction
    jmp endmainloop


    callBAction:
    call BAction
    jmp endmainloop
    

    callEditBatchModeString:
    call EditBatchModeString

    endmainloop:
    mov ah,0h
    int 16h
jmp Mainloop
main endp

;=====================================================================================
;//////////////////////////////////////////PROC///////////////////////////////////////
;=====================================================================================

;===================================Moving Procs=======================================

WRight proc
    mov BlackORWhite, 1
    call WMoveRight
    ret
WRight endp

BRight proc
    mov BlackORWhite, 0
    call BMoveRight
ret
BRight endp

WLeft proc
    mov BlackORWhite, 1
    call WMoveLeft
ret
WLeft endp

BLeft proc
    mov BlackORWhite, 0
    call BMoveLeft
ret
BLeft endp

WUp proc
    mov BlackORWhite, 1
    call WMoveUp
    mov ah,0h
ret
WUp endp

BUp proc
    mov BlackORWhite, 0
    call BMoveUp
ret
BUp endp

WDown proc
    mov BlackORWhite, 1
    call WMoveDown
ret
WDown endp

BDown proc
    mov BlackORWhite, 0
    call BMoveDown
ret
BDown endp

WAction proc
    mov BlackORWhite, 1
    call SelectPieceWhite
ret
WAction endp

BAction proc
    mov BlackORWhite, 0
    call SelectPieceBlack
ret
BAction endp

SwitchFlag proc                                         ;Switches batch mode flag 0->1, 1->0
push dx

    cmp BatchModeFlag,0                                 ;If flag was zero, it sets it to 1
    je SetBatchModeFlag
    mov BatchModeFlag, 0                                ;Else it sets it to zero
    mov dx, offset LeaveBatchMode
    call UpdateStatusBar
    call ExecuteBatchMode                               ;Starts executing batch mode
    jmp endswitch
    SetBatchModeFlag:
    mov BatchModeFlag, 1
    mov dx, offset EnterBatchMode
    call UpdateStatusBar

    endswitch:

pop dx
ret
SwitchFlag endp

EditBatchModeString proc                            ;Takes commands from user
    push si

    cmp BatchModeSize, 16d                          ;If maximum length, it ends
    je endEditBatchModeString

    cmp al,31h                                      ;To check that the input is valid between 1->8
    jb endEditBatchModeString
    cmp al,38h
    ja endEditBatchModeString

    mov si, BatchModeSize                           ;Increments the size as it adds each number to the string
    mov BatchModeString[si], al
    inc si
    mov BatchModeSize, si

    endEditBatchModeString:
    pop si
ret
EditBatchModeString endp


;==================================Drawing Procs======================================
openfile proc
    mov ah,3Dh
    mov al,0 ; read only mode
    int 21h

    ;you should check carry flag to make sure it worked correctly
    ; carry = 0 -> successful, file handle -> ax
    ; carry = 1 -> failed, ax-> error code

    mov [FileHandle],ax ; moving the pointer (file handle to the ax)
    ret
openfile endp

readdataPiece proc
    push cx
    push dx
    mov ah,3Fh
    mov bx,[FileHandle]
    mov cx,ChessPieceWidth*ChessPieceHeight ; number of bytes to read 75*75
    ;lea dx,PieceData ; place where the data is added after reading
    int 21h  
    pop dx
    pop cx
    ret
readdataPiece endp

closefile proc
    mov ah,3Eh 
    mov bx,[FileHandle]
    int 21h   ; interrupt for closing file 21/3E
    ret
closefile endp

DrawImageProc proc
    push cx
    push dx

  drawloop1:
    mov al,[bx]             ;moving the current colour in the bx to the al

    cmp al, 0eh             ;Here we compare the colors that we don't want to draw
    je skipDraw
    cmp al, 03h
    je skipDraw
    cmp al, 02h
    je skipDraw
    cmp al, 01h
    je skipDraw
    cmp al, 0dh
    je skipDraw
    cmp al, 10h
    je skipDraw

    int 10h

    skipDraw:
    inc cx         ;Gets next coordinate
    inc bx         ;Gets next colour
    cmp cx,ENDX    ;checking the whole width is not reached
    jne drawloop1
    mov cx,STARTX  ;If a whole row is drawn, x-coordinate is reset and y-coordinate is incremented
    inc dx
    cmp dx,ENDY ; checking the whole height is not reached
    jne drawloop1

    pop dx
    pop cx
    ret
DrawImageProc endp

Load_Piece_to_Draw proc
    push cx
    push dx
    CALL openfile
    lea dx,PieceData ; place where the data is added after reading
    CALL readdataPiece
    lea bx,PieceData ; bl contains index at the current drawn pixel
    pop dx
    pop cx
    ret
Load_Piece_to_Draw endp

Draw_Loaded_Piece proc ; we need to change the cx and dx before moving (x,y coordinates)
    push cx
    push dx
    MOV STARTX,CX      ;For the program to know where to go after finishing one line of drawing (x-axis)
    mov endx,cx
    add endx,ChessPieceWidth    ;These two lines determine the end of the picture
    mov ENDY,dx
    add endy,ChessPieceHeight   ;These two lines determine the end of the picture

    mov ah,0ch ; switching to graphics mode
    ;Drawing Chess Board
    call DrawImageProc
    call closefile
    pop dx
    pop cx
    ret
Draw_Loaded_Piece endp

;---------------------------------------------------------------------------------------------
DrawChessCell proc
    push cx                             ;To keep the values of CX,DX,AX and BX
    push dx
    push ax
    push bx

    mov ah, 0Ch                         ;To draw the pixel   
    mov bx,0h                           ;Page number zero
    mov Startx, cx                      ;To keep the starting point known
    mov Endx, CX                        ;To determine the endpoint in x-axis
    add Endx, ChessPieceWidth
    mov Endy, DX                        ;To determine the endpoint in y-axis
    add Endy, ChessPieceHeight

    Drawloop:                           ;Main loop to draw
    int 10h
    inc cx
    cmp cx, endx                        ;Checks whether CX has reached its end
    je incY                             ;Increments dx if so
    jmp Drawloop                        ;Else continue to draw loop

    incY:
    mov cx,Startx                       ;returns to start
    inc dx                              
    cmp dx, endy                        ;checks whether drawing is done
    je endCell
    jmp Drawloop                        ;if not done jumps back to drawing loop

    endCell:
    pop bx
    pop ax
    pop dx                              ;return values of cx, dx and ax
    pop cx
    ret
DrawChessCell endp
;-----------------------------------------------------------------
;-----------------------------------------------------------------
DrawChessBoard proc
    push cx                              ;To keep the values of cx,dx,ax and si
    push dx
    push ax
    push SI
    mov cx,0d
    mov dx,0d
    mov si,ChessPieceHeight

    ChessBoardLoop:

    DrawFirstColor:
    mov al,LightCellColor                ;Loading al with first color
    call DrawChessCell                   ;Drawing the actual piece
    add cx,ChessPieceWidth               ;Going to the next cell
    cmp cx,ChessBoardWidth               ;Checking whether row has ended
    je incRow                            ;Goes to the cell below

    DrawSecondColor:
    mov al,DarkCellColor                     ;Loading al with second color
    call DrawChessCell                   ;Drawing the second color
    add cx,ChessPieceWidth               ;Going to the next cell
    cmp cx,ChessBoardWidth               ;Checking whether row has ended
    je incRow                            ;Goes to the cell below

    jmp ChessBoardLoop                   ;Continues to draw

    incRow:
    mov cx,0h                            ;Goes to the start of the board
    add dx, ChessPieceHeight             ;Goes down a cell
    cmp dx, ChessBoardHeight             ;Checks if finished
    je endBoard                          ;Jumps to the end of the proc

    cmp dx,si                            ;Checks which color to draw first
    je CheckForRow                       ;Goes to a few lines of code which moves the position where we should use the second color

    jmp DrawFirstColor                   ;If we shouldn't draw the second color we go to first color

    CheckForRow:                         
    add si, 2*ChessPieceHeight           ;So that we start the row after the current one with the same color
    jmp DrawSecondColor

    endBoard:
    pop si
    pop ax
    pop dx                              ;To return the values of cx, dx, ax and SI
    pop cx
    ret
DrawChessBoard endp

DrawAllPieces proc                      ;This function Draws all pieces found on the chess board
    push di
    push cx
    push dx
    push bx

    mov bx,0
    mov di,0
    mov cx,0
    mov dx,0
    mov ah, BlackORWhite
    push ax
    mov BlackORWhite,1
    DrawingLoop:                        ;Loops 64 times to draw all chess pieces
        call DrawChessPiece
        inc di
        cmp di, 64d
        je endloop
        add cx, ChessPieceWidth         ;Every time we add chesspiecewidth to go to nextcell
        cmp cx, ChessBoardWidth
        je NextRow                      ;Here we see if we finished the whole row
    jmp DrawingLoop

    NextRow:
        mov cx,0
        add dx,ChessPieceHeight         ;We reset the cx then (x-axis) then we go to next row
    jmp DrawingLoop

    endloop:
        pop ax
        mov BlackORWhite, ah
        pop bx                              ;Return all used registers
        pop dx
        pop cx
        pop di
    ret
DrawAllPieces endp

;================================DRAWING PIECES PROCS===============================;

DrawChessPiece proc                  

        push bx
        push di

        NormalDrawChessPiece:
            cmp ChessboardGrid[bx][di], 01h  ;Similar to a switch case in c++ we check which piece is this to draw it
            jne Draw1
            call DrawBAskary
        Draw1: 
            cmp ChessboardGrid[bx][di], 02
            jne Draw2
            call DrawBTabya
        Draw2:
            cmp ChessboardGrid[bx][di], 03
            jne Draw3
            call DrawBHosan
        Draw3:
            cmp ChessboardGrid[bx][di], 04
            jne Draw4
            call DrawBFil
        Draw4:
            cmp ChessboardGrid[bx][di], 05
            jne Draw5
            call DrawBQueen
        Draw5:
            cmp ChessboardGrid[bx][di], 14h
            jne Draw6
            call DrawWFil
        Draw6:
            cmp ChessboardGrid[bx][di], 12h
            jne Draw7
            call DrawWTabya
        Draw7:
            cmp ChessboardGrid[bx][di], 15h
            jne Draw8
            call DrawWQueen
        Draw8:
            cmp ChessboardGrid[bx][di], 06h
            jne Draw9
            call DrawBKing
        Draw9:
            cmp ChessboardGrid[bx][di], 16h
            jne Draw10
            call DrawWKing
        Draw10:
            cmp ChessboardGrid[bx][di], 11h
            jne Draw11
            call DrawWAskary
        Draw11:
            cmp ChessboardGrid[bx][di], 13h
            jne Draw12
            call DrawWHosan
        Draw12:
            cmp ChessboardGrid[bx][di], 0BBh
            jne NoDraw
            call DrawPowerUp
        NoDraw:
            pop di
            pop bx

            ret

DrawChessPiece endp

DrawPowerUp proc
    push bx
    push dx
    lea dx, PowerUp
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawPowerUp endp

DrawFullJail proc
    push bx
    push dx
    lea dx, FullJail
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawFullJail endp

DrawHalfJail proc
    push bx
    push dx
    lea dx, HalfJail
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawHalfJail endp

DrawQuarterJail proc
    push bx
    push dx
    lea dx, QuarterJail
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawQuarterJail endp 

DrawWKing proc
    push bx
    push dx
    lea dx, White_King
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawWKing endp 

DrawWAskary proc
    push bx
    push dx
    lea dx, White_Askary
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawWAskary endp 

DrawWFil proc
    push bx
    push dx
    lea dx, White_Fil
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawWFil endp

DrawWHosan proc
    push bx
    push dx
    lea dx, White_Hosan
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawWHosan endp

DrawWTabya proc
    push bx
    push dx
    lea dx, White_Tabya
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawWTabya endp

DrawWQueen proc
    push bx 
    push dx
    lea dx, White_Queen
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawWQueen endp

DrawBKing proc
    push bx
    push dx
    lea dx, Black_King
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawBKing endp

DrawBQueen proc
    push bx
    push dx
    lea dx, Black_Queen
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawBQueen endp

DrawBHosan proc
    push bx
    push dx
    lea dx, Black_Hosan
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawBHosan endp

DrawBTabya proc
    push bx 
    push dx
    lea dx, Black_Tabya
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawBTabya endp

DrawBFil proc
    push bx
    push dx
    lea dx, Black_Fil
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawBFil endp

DrawBAskary proc
    push bx
    push dx
    lea dx, Black_Askary
    Call Load_Piece_to_Draw
    pop dx
    call Draw_Loaded_Piece
    pop bx
    ret
DrawBAskary endp 

;----------------------------------Moving Procs---------------------------------------;

WMoveRight proc             
    call GetCXDXFromBXDI                     ;This function allows the user to select the cell right to him
    mov OldPositionDrawingX, cx              ;Stores the old position both for drawing and knowing where we are
    mov OldPositionDrawingY, dx
    mov OldPositionIndex, bx

    add cx,ChessPieceWidth                  ;Moving right for drawing
    inc bx                                  ;Moving right in index of chess grid
    cmp cx,ChessBoardWidth                  ;Checks if user passed the limit to return him back
    jb skipWRight
    sub cx, ChessPieceWidth                 ;Returns him back
    dec bx
    jmp endWMoveRight

    skipWRight:
    call CheckCellColor                     ;Check for Overlapping
    cmp al, PlayerTwoSelectedCellColor      ; If player 2 selector is on this cell it prevents player 1 from going there
    je WNoMoveRight
    cmp al, PlayerTwoOnPlayerOneHighlightColor
    je WNoMoveRight
    cmp al, PlayerTwoHighlightedSelectedCellColor
    je WNoMoveRight

    xchg cx, OldPositionDrawingX            ;Gets old cell position
    xchg bx, OldPositionIndex
    call CheckCellColor                     ;Checks the color of the cell
    call CheckOldCellToDraw
    call DrawChessCell                      ;Draws it
    call DrawChessPiece                     ;Draws chess piece in that location

    xchg cx, OldPositionDrawingX            ;Gets new cell position
    xchg bx, OldPositionIndex
    call CheckCellColor
    call CheckNewCellToDraw                 ;Draws the selected cell
    call DrawChessCell          
    call DrawChessPiece                     ;Draws piece on top of it

    endWMoveRight:
    ret
    WNoMoveRight:
    sub cx, ChessPieceWidth                 ;Returns him back
    dec bx
    jmp endWMoveRight

WMoveRight endp

BMoveRight proc    
push bx
push di 
    mov bx,bp                               ;As bp and si are placeholders for player 2 but are not used in any operation
    mov di,si
    call GetCXDXFromBXDI                    ;This function allows the user to select the cell right to him
    mov OldPositionDrawingX, cx             ;Stores the old position both for drawing and knowing where we are
    mov OldPositionDrawingY, dx
    mov OldPositionIndex, bx

    add cx,ChessPieceWidth                  ;Moving right for drawing
    inc bx                                  ;Moving right in index of chess grid
    cmp cx,ChessBoardWidth                  ;Checks if user passed the limit to return him back
    jb skipBRight
    sub cx, ChessPieceWidth                 ;Returns him back
    dec bx
    jmp endBMoveRight

    skipBRight:
    call CheckCellColor
    cmp al, PlayerOneSelectedCellColor
    je BNoMoveRight
    cmp al, PlayerOneOnPlayerTwoHighlightColor
    je BNoMoveRight
    cmp al, PlayerOneHighlightedSelectedCellColor
    je BNoMoveRight
    
    xchg cx, OldPositionDrawingX            ;Gets old cell position
    xchg bx, OldPositionIndex
    call CheckCellColor                     ;Checks the color of the cell
    call CheckOldCellToDraw
    call DrawChessCell                      ;Draws it
    call DrawChessPiece                     ;Draws chess piece in that location

    xchg cx, OldPositionDrawingX            ;Gets new cell position
    xchg bx, OldPositionIndex
    call CheckCellColor
    call CheckNewCellToDraw                 ;Draws the selected cell
    call DrawChessCell          
    call DrawChessPiece                     ;Draws piece on top of it

    endBMoveRight:
    mov bp,bx
    mov si,di
    pop di
    pop bx
    ret
    BNoMoveRight:
    sub cx, ChessPieceWidth                 ;Returns him back
    dec bx
    jmp endBMoveRight
    

BMoveRight endp

WMoveLeft proc
    call GetCXDXFromBXDI
    mov OldPositionDrawingX, cx
    mov OldPositionDrawingY, dx
    mov OldPositionIndex, bx

    sub cx, ChessPieceWidth
    dec bx
    cmp cx,0
    jge skipWLeft
    add cx, ChessPieceWidth
    inc bx
    jmp endWmoveLeft

    skipWLeft:
    call CheckCellColor
    cmp al,PlayerTwoSelectedCellColor
    je WNoMoveLeft
    cmp al, PlayerTwoOnPlayerOneHighlightColor
    je WNoMoveLeft
    cmp al, PlayerTwoHighlightedSelectedCellColor
    je WNoMoveLeft

    xchg cx, OldPositionDrawingX
    xchg bx, OldPositionIndex
    call CheckCellColor
    call CheckOldCellToDraw
    call DrawChessCell
    call DrawChessPiece

    xchg cx, OldPositionDrawingX
    xchg bx, OldPositionIndex
    call CheckCellColor
    call CheckNewCellToDraw
    call DrawChessCell
    call DrawChessPiece
    
    endWmoveLeft:
    ret
    WNoMoveLeft:
    add cx, ChessPieceWidth
    inc bx
    jmp endWmoveLeft

WMoveLeft endp

BMoveLeft proc
    push bx
    push di 
    mov bx,bp
    mov di,si
    call GetCXDXFromBXDI     
    mov OldPositionDrawingX, cx
    mov OldPositionDrawingY, dx
    mov OldPositionIndex, bx

    sub cx, ChessPieceWidth
    dec bx
    cmp cx,0
    jge skipBLeft
    add cx, ChessPieceWidth
    inc bx
    jmp endBmoveLeft

    skipBLeft:
    call CheckCellColor
    cmp al,PlayerOneSelectedCellColor
    je BNoMoveLeft
    cmp al, PlayerOneOnPlayerTwoHighlightColor
    je BNoMoveLeft
    cmp al, PlayerOneHighlightedSelectedCellColor
    je BNoMoveLeft

    xchg cx, OldPositionDrawingX
    xchg bx, OldPositionIndex
    call CheckCellColor
    call CheckOldCellToDraw
    call DrawChessCell
    call DrawChessPiece

    xchg cx, OldPositionDrawingX
    xchg bx, OldPositionIndex
    call CheckCellColor
    call CheckNewCellToDraw
    call DrawChessCell
    call DrawChessPiece
    
    endBmoveLeft:
    mov bp,bx
    mov si,di
    pop di
    pop bx
    ret
    BNoMoveLeft:
    add cx, ChessPieceWidth
    inc bx
    jmp endBmoveLeft

BMoveLeft endp

WMoveUp proc
    call GetCXDXFromBXDI
    mov OldPositionDrawingY, dx
    mov OldPositionDrawingX, cx
    mov OldPositionIndex, di

    sub dx, ChessPieceHeight
    sub di,8d
    cmp dx,0
    jge skipWUp
    add dx, ChessPieceHeight
    add di,8d
    jmp endWMoveUp

    skipWUp:
    call CheckCellColor
    cmp al,PlayerTwoSelectedCellColor
    je WNoMoveUp
    cmp al, PlayerTwoOnPlayerOneHighlightColor
    je WNoMoveUp
    cmp al, PlayerTwoHighlightedSelectedCellColor
    je WNoMoveUp

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex
    call CheckCellColor
    call CheckOldCellToDraw
    call DrawChessCell
    call DrawChessPiece

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex
    call CheckCellColor
    call CheckNewCellToDraw
    call DrawChessCell
    call DrawChessPiece

    endWMoveUp:
    ret
    WNoMoveUp:
    add dx, ChessPieceHeight
    add di,8d
    jmp endWMoveUp

WMoveUp endp

BMoveUp proc
    push bx
    push di 
    mov bx,bp
    mov di,si
    call GetCXDXFromBXDI     
    mov OldPositionDrawingY, dx
    mov OldPositionDrawingX, cx
    mov OldPositionIndex, di

    sub dx, ChessPieceHeight
    sub di,8d
    cmp dx,0
    jge skipBUp
    add dx, ChessPieceHeight
    add di,8d
    jmp endBMoveUp

    skipBUp:
    call CheckCellColor
    cmp al, PlayerOneSelectedCellColor
    je BNoMoveUp
    cmp al, PlayerOneOnPlayerTwoHighlightColor
    je BNoMoveUp
    cmp al, PlayerOneHighlightedSelectedCellColor
    je BNoMoveUp

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex
    call CheckCellColor
    call CheckOldCellToDraw
    call DrawChessCell
    call DrawChessPiece

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex
    call CheckCellColor
    call CheckNewCellToDraw
    call DrawChessCell
    call DrawChessPiece

    endBMoveUp:
    mov bp,bx
    mov si,di
    pop di
    pop bx
    ret

    BNoMoveUp:
    add dx, ChessPieceHeight
    add di,8d
    jmp endBMoveUp

BMoveUp endp

WMoveDown proc
    call GetCXDXFromBXDI
    mov OldPositionDrawingY, dx
    mov OldPositionDrawingX, cx
    mov OldPositionIndex, di

    add dx, ChessPieceHeight
    add di,8d
    cmp dx,ChessBoardHeight
    jb skipWDown
    sub dx, ChessPieceHeight
    sub di,8d
    jmp endWMoveDown

    skipWDown:
    call CheckCellColor
    cmp al, PlayerTwoSelectedCellColor
    je WNoMoveDown
    cmp al, PlayerTwoOnPlayerOneHighlightColor
    je WNoMoveDown
    cmp al, PlayerTwoHighlightedSelectedCellColor
    je WNoMoveDown

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex
    call CheckCellColor
    call CheckOldCellToDraw
    call DrawChessCell
    call DrawChessPiece

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex

    call CheckCellColor
    call CheckNewCellToDraw
    call DrawChessCell
    call DrawChessPiece

    endWMoveDown:
    ret
    WNoMoveDown:
    sub dx, ChessPieceHeight
    sub di,8d
    jmp endWMoveDown

WMoveDown endp

BMoveDown proc
    push bx
    push di 
    mov bx,bp
    mov di,si
    call GetCXDXFromBXDI     
    mov OldPositionDrawingY, dx
    mov OldPositionDrawingX, cx
    mov OldPositionIndex, di

    add dx, ChessPieceHeight
    add di,8d
    cmp dx,ChessBoardHeight
    jb skipBDown
    sub dx, ChessPieceHeight
    sub di,8d
    jmp endBMoveDown

    skipBDown:
    call CheckCellColor
    cmp al, PlayerOneSelectedCellColor
    je BNoMoveDown
    cmp al, PlayerOneOnPlayerTwoHighlightColor
    je BNoMoveDown
    cmp al, PlayerOneHighlightedSelectedCellColor
    je BNoMoveDown

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex
    call CheckCellColor
    call CheckOldCellToDraw
    call DrawChessCell
    call DrawChessPiece

    xchg dx, OldPositionDrawingY
    xchg di, OldPositionIndex

    call CheckCellColor
    call CheckNewCellToDraw
    call DrawChessCell
    call DrawChessPiece

    endBMoveDown:
    mov bp,bx
    mov si,di
    pop di
    pop bx
    ret
    BNoMoveDown:
    sub dx, ChessPieceHeight
    sub di,8d
    jmp endBMoveDown

BMoveDown endp

CheckOldCellToDraw proc
    push dx
    push cx
    push di
    push bx

    cmp BlackORWhite, 0
    je BlackCheckOldCellToDraw

    cmp al,PlayerOneHighlightedSelectedCellColor
    jne PlayerOneCheck2
    mov al,PlayerOneHighlightedCellColor
    jmp endCheckOldCellToDraw

    PlayerOneCheck2:
    cmp al, PlayerOneOnPlayerTwoHighlightColor
    jne OverLapCheck
    mov al, PlayerTwoHighlightedCellColor
    jmp endCheckOldCellToDraw

    OverLapCheck:
    cmp al, OverLappedHighlightedSelectedColor
    jne Normal
    mov al, OverLappedHighlightingColor
    jmp endCheckOldCellToDraw

    BlackCheckOldCellToDraw:
    cmp al,PlayerTwoHighlightedSelectedCellColor
    jne PlayerTwoCheck2
    mov al,PlayerTwoHighlightedCellColor
    jmp endCheckOldCellToDraw

    PlayerTwoCheck2:
    cmp al, PlayerTwoOnPlayerOneHighlightColor
    jne OverLapCheck
    mov al, PlayerOneHighlightedCellColor
    jmp endCheckOldCellToDraw

    Normal:                                 ;Determining the color of the cell from the color of the cell next to it
    mov cx, OldPositionDrawingX
    mov dx, OldPositionDrawingY
    call CheckCellColor

    cmp al,LightCellColor
    je SecondColor
    cmp al, DarkCellColor
    je FirstColor
    jne specialCase

    specialCase:                            ;If the cell was highlighted we determine its color using another method
    pop bx
    push bx
    add bx,di                           ;Adds the two indicies to know if they are odd or even
    rcr bx,1                            ;Checks whether odd or even
    jc OddNumber                
    jnc EvenNumber

    EvenNumber:                         ;Check which row is the user in first
    mov ax,di
    mov bl,8
    div bl
    rcr al,1                            ;If the user is in an odd row, we must exchange the colors
    jc SecondColor

    FirstColor:
    mov al, LightCellColor
    jmp endCheckOldCellToDraw

    OddNumber:                          ;Checks which row is the user in first
    mov ax,di
    mov bl,8
    div bl
    rcr al,1                            ;If the user in an odd row, we must exchange colors
    jc FirstColor

    SecondColor:
    mov al,DarkCellColor

    endCheckOldCellToDraw:
    pop bx
    pop di
    pop cx
    pop dx
    ret
CheckOldCellToDraw endp

CheckNormalCellColor proc
    push di
    push bx
    push dx
    push ax

    add bx,di                           ;Adds the two indicies to know if they are odd or even
    rcr bx,1                            ;Checks whether odd or even
    jc OddNumber1                
    jnc EvenNumber1

    EvenNumber1:                         ;Check which row is the user in first
    mov ax,di
    mov bl,8
    div bl
    rcr al,1                            ;If the user is in an odd row, we must exchange the colors
    jc SecondColor1

    FirstColor1:
    pop ax
    mov al, LightCellColor
    jmp endCheckNormalCellColor

    OddNumber1:                          ;Checks which row is the user in first
    mov ax,di
    mov bl,8
    div bl
    rcr al,1                            ;If the user in an odd row, we must exchange colors
    jc FirstColor1

    SecondColor1:
    pop ax
    mov al,DarkCellColor

    endCheckNormalCellColor:
    pop dx
    pop bx
    pop di

ret
CheckNormalCellColor endp

GetCXDXFromBXDI proc                      ;Getting coordinates from row and coloumn <3
    push ax
    push si

    mov ax,bx
    mov si, ChessPieceWidth
    mul si
    mov cx,ax

    mov ax,di
    mov si,8d
    div si
    mov si, ChessPieceHeight
    mul si
    mov dx,ax

    pop si
    pop ax
    ret
GetCXDXFromBXDI endp

SelectPieceWhite proc                                 ;Executed if user pressed Q                           
            call CheckIfWhiteLockedPiece              ;If piece is locked decline action
            jc NotWhite
            call CheckCellColor
            cmp al, PlayerOneHighlightedSelectedCellColor ;if cell is highlighted and selected we move
            jne WColorFurtherCheck
            call WMovePiece
            jmp NotWhite
        WColorFurtherCheck:
            cmp al, OverLappedHighlightedSelectedColor    ;if 2 players highlighted the same cell, we move
            jne White0
            call WMovePiece
            jmp NotWhite
        White0:
            cmp PlayerOneSelectedPiecePosition[0], 0ffffH
            jne WDeselectPiece
        White1:                                              ;Similar to a switch case in c++ we check which piece is this to draw it
            cmp ChessboardGrid[bx][di], 14h
            jne White2
            call DrawWFilMoves
            call Highlight
        White2:
            cmp ChessboardGrid[bx][di], 12h
            jne White3
            call DrawWTabyaMoves
            call Highlight
        White3:
            cmp ChessboardGrid[bx][di], 15h
            jne White4
            call DrawWQueenMoves
            call Highlight
        White4:
            cmp ChessboardGrid[bx][di], 16h
            jne White5
            call DrawWKingMoves
            call Highlight
        White5:
            cmp ChessboardGrid[bx][di], 11h
            jne White6
            call DrawWAskaryMoves
            call Highlight
        White6:
            cmp ChessboardGrid[bx][di], 13h
            jne NotWhite
            call DrawWHosanMoves
            call Highlight
        NotWhite:
            ret
            WDeselectPiece:
            call PlayerOneRemoveHighlightedCells
            jmp White1
SelectPieceWhite endp

SelectPieceBlack proc                                      ;Similar to a switch case in c++ we check which piece is this to draw it
            push bx
            push di

            mov di,si
            mov bx,bp
            call CheckIfBlackLockedPiece
            jc NotBlack
            call CheckCellColor
            cmp al, PlayerTwoHighlightedSelectedCellColor
            jne BColorFurtherCheck
            call BMovePiece
            jmp NotBlack
        BColorFurtherCheck:
            cmp al, OverLappedHighlightedSelectedColor
            jne Black0
            call BMovePiece
            jmp NotBlack
        Black0:
            cmp PlayerTwoSelectedPiecePosition[0], 0ffffH
            jne BDeselectPiece
        Black1:
            cmp ChessboardGrid[bx][di], 04h
            jne Black2
            call DrawBFilMoves
            call Highlight
        Black2:
            cmp ChessboardGrid[bx][di], 02h
            jne Black3
            call DrawBTabyaMoves
            call Highlight
        Black3:
            cmp ChessboardGrid[bx][di], 05h
            jne Black4
            call DrawBQueenMoves
            call Highlight
        Black4:
            cmp ChessboardGrid[bx][di], 06h
            jne Black5
            call DrawBKingMoves
            call Highlight
        Black5:
            cmp ChessboardGrid[bx][di], 01h
            jne Black6
            call DrawBAskaryMoves
            call Highlight
        Black6:
            cmp ChessboardGrid[bx][di], 03h
            jne NotBlack
            call DrawBHosanMoves
            call Highlight
        NotBlack:
            mov bp,bx
            mov si,di
            pop di
            pop bx
            ret
            BDeselectPiece:
            call PlayerTwoRemoveHighlightedCells
            jmp Black1
SelectPieceBlack endp
;==================================================================
DrawWTabyaMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerOneSelectedPiecePosition[0], bx
    mov PlayerOneSelectedPiecePosition[2], di

    mov si, 00h                      ;Reset the counter
 
    AboveTabya:
    sub di,8d
    call checkBorders
    cmp OutsideBorders,1
    je Continue1
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h                     ;Checks if cell empty
    je CountHighUp

    cmp AL, 0bbh                    ;Checks if cell contains powerup
    jne CheckIfBlackAbovePiece      ;If not empty check if white piece
    je CountHighUp

    CountHighUp:
        inc si                       ;One cell counted to be highlighted
        jmp AboveTabya

    CheckIfBlackAbovePiece:
    and AL,0f0h
    cmp AL,00h
    je CountBlackAboveThenEnd         ;if black piece found
    jne Continue1

    CountBlackAboveThenEnd:
        inc si
        jmp Continue1
    Continue1:
    mov al, PlayerOneHighlightedCellColor
    
    CALL HighlightUp
    mov bx,PlayerOneSelectedPiecePosition[0]
    mov di,PlayerOneSelectedPiecePosition[2]
    
    mov si,00h
    BelowTabya:
    add di,8d
    call checkBorders
    cmp OutsideBorders,1
    je Continue2
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je CountHighBelow

    cmp AL, 0bbh         ;Checks if cell contains powerup
    jne CheckIfBlackPieceBelow                ;If not empty check if white piece
    je CountHighBelow

    CountHighBelow:
        inc si      ; One cell counted to be highlighted
        jmp BelowTabya

    CheckIfBlackPieceBelow:
    and AL,0f0h
    cmp AL,00h
    je CountBlackBelowThenEnd                          ;if black piece found
    jne Continue2

    CountBlackBelowThenEnd:
        inc si
        jmp Continue2

    Continue2:
    mov al,PlayerOneHighlightedCellColor
    
    CALL HighlightDown
    mov bx,PlayerOneSelectedPiecePosition[0]
    mov di,PlayerOneSelectedPiecePosition[2]
    
    ;------------------------------------------------
RightTabya:
    inc bx
    call checkBorders
    cmp OutsideBorders,1
    je Continue3
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je CountHighRight

    cmp AL, 0bbh         
    jne CheckIfBlackRightPiece                ;If not empty check if white piece
    je CountHighRight

    CountHighRight:
        inc si      ; One cell counted to be highlighted
        jmp RightTabya

    CheckIfBlackRightPiece:
    and AL,0f0h
    cmp AL,00h
    je CountBlackRightThenEnd                          ;if black piece found
    jne Continue3

    CountBlackRightThenEnd:
        inc si
        jmp Continue3
    Continue3:
    mov al, PlayerOneHighlightedCellColor

    CALL HighlightRight
    mov bx,PlayerOneSelectedPiecePosition[0]
    mov di,PlayerOneSelectedPiecePosition[2]
    
    mov si,00h
    LeftTabya:
    dec bx
    call checkBorders
    cmp OutsideBorders,1
    je endDrawWTabyaMoves
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je CountHighLeft

    cmp AL, 0bbh         
    jne CheckIfBlackPieceLeft                ;If not empty check if white piece
    je CountHighLeft

    CountHighLeft:
        inc si      ; One cell counted to be highlighted
        jmp LeftTabya

    CheckIfBlackPieceLeft:
    and AL,0f0h
    cmp AL,00h
    je CountBlackLeftThenEnd                          ;if black piece found
    jne endDrawWTabyaMoves

    CountBlackLeftThenEnd:
        inc si
    

    ;------------------------------------------------
    
    endDrawWTabyaMoves:
    mov al,PlayerOneHighlightedCellColor
    CALL HighlightLeft
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawWTabyaMoves endp

DrawWQueenMoves proc ;Since the queen is a combination of movements of both fil and tabya
    call DrawWFilMoves
    call DrawWTabyaMoves
ret
DrawWQueenMoves endp

CheckBorders proc         ;Checks if we're outside borders the flag is set to 1

    push bx
    push di

    mov OutsideBorders,0H
     
    cmp di,0h                                 
    JL Outside
    cmp di,56d
    JG Outside
    cmp bx,0
    JL Outside
    cmp bx,7
    JG Outside
    JLE endCheck

    Outside:
    mov OutsideBorders,1h
   

endCheck:
pop di
pop bx

ret
CheckBorders endp    


DrawWFilMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerOneSelectedPiecePosition[0], bx
    mov PlayerOneSelectedPiecePosition[2], di
    mov si,00h                              ;Reset counter

    FilRightUpDiagonalHighlights:

    sub di,8d                               ;Goes above one cell
    inc bx                                  ;Goes right one cell
    call CheckBorders
    
    cmp OutsideBorders,1h
    je HighlightRightUpDiagonalFil 
    
    ;InsideBorders:
    mov AL, ChessboardGrid[bx][di]         

    cmp AL, 99h
    je incrementCounter1                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounter1                     ;If cell is powerup, increment the counter
    jne PieceFoundRU
    
    PieceFoundRU:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black, increment the counter
    je incrementCounter1 
    jne HighlightRightUpDiagonalFil

    incrementCounter1:
    inc si
    cmp AL, 00h                             ;If black highlight
    je HighlightRightUpDiagonalFil
    jne FilRightUpDiagonalHighlights

    HighlightRightUpDiagonalFil:
    mov al, PlayerOneHighlightedCellColor
    call HighlightUpRightDiagonal

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    mov si,00h                              ;Reset counter

    FilLeftUpDiagonalHighlights:

    sub di,8d                               ;Goes above one cell
    dec bx                                  ;Goes LEFT  one cell

    call checkBorders
    cmp OutsideBorders,1h
    je HighlightLeftUpDiagonalFil 

    ;InsideBorders: 
    mov AL, ChessboardGrid[bx][di]         
     
    
    cmp AL, 99h
    je incrementCounter2                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounter2                     
    jne PieceFoundLU
    
    PieceFoundLU:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black, increment the counter
    je incrementCounter2 
    jne HighlightLeftUpDiagonalFil

    incrementCounter2:
    inc si
    cmp AL, 00h                             ;If black highlight
    je HighlightLeftUpDiagonalFil
    jne FilLeftUpDiagonalHighlights

    HighlightLeftUpDiagonalFil:
    mov al, PlayerOneHighlightedCellColor
    call HighlightUpLeftDiagonal

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    mov si,00h                              ;Reset counter

    FilRightDownDiagonalHighlights:

    add di,8d                               ;Goes down one cell
    inc bx                                  ;Goes right one cell

    call checkBorders
    cmp OutsideBorders,1h
    je HighlightRightDownDiagonalFil 

    ;InsideBorders:
    mov AL, ChessboardGrid[bx][di]         

    cmp AL, 99h
    je incrementCounter3                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounter3                     
    jne PieceFoundRD
    
    PieceFoundRD:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black, increment the counter
    je incrementCounter3 
    jne HighlightRightDownDiagonalFil

    incrementCounter3:
    inc si
    cmp AL, 00h                             ;If black highlight
    je HighlightRightDOWNDiagonalFil
    jne FilRightDOWNDiagonalHighlights


    HighlightRightDOWNDiagonalFil:
    mov al, PlayerOneHighlightedCellColor
    call HighlightDownRightDiagonal

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    mov si,00h                              ;Reset counter

    FilLeftDOWNDiagonalHighlights:

    ADD di,8d                               ;Goes DOWN one cell
    dec bx                                  ;Goes LEFT one cell

    call checkBorders
    cmp OutsideBorders,1h
    je HighlightLeftDownDiagonalFil 

    ;InsideBorders:
    mov AL, ChessboardGrid[bx][di]         

    cmp AL, 99h
    je incrementCounter4                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounter4                     
    jne PieceFoundLD
    
    PieceFoundLD:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black, increment the counter
    je incrementCounter4 
    jne HighlightLeftDOWNDiagonalFil

    incrementCounter4:
    inc si
    cmp AL, 00h                             ;If black highlight
    je HighlightLeftDOWNDiagonalFil
    jne FilLeftDOWNDiagonalHighlights

    HighlightLeftDOWNDiagonalFil:
    mov al, PlayerOneHighlightedCellColor
    call HighlightDownLeftDiagonal

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

   
    endDrawWFilMoves:
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawWFilMoves endp

DrawWkingMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerOneSelectedPiecePosition[0], bx
    mov PlayerOneSelectedPiecePosition[2], di
   

    checkAboveWKing:
    sub di,8d           ;we go above 1 cell

    call CheckBorders
    cmp OutsideBorders,1h
    je checkRightUpWKing
    
    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h         ;check if empty
    je HighlightAboveWking ;if empty highlight

    cmp AL, 0bbh         
    je HighlightAboveWking 
    jne piecefoundUWking   ;else check piece 
     
    piecefoundUWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightAboveWking
    jne checkRightUpWKing

    HighlightAboveWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightUp

   
    

    checkRightUpWKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    sub di,8d
    inc bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftUpWKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightRightUpWking

    cmp AL, 0bbh  
    je HighlightRightUpWking
    jne piecefoundURWking
     
    piecefoundURWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightRightUpWking
    jne checkLeftUpWKing

    HighlightRightUpWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightUpRightDiagonal
    
    
    
    
    checkLeftUpWKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    sub di,8d
    dec bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftWKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightLeftUpWking

    cmp AL, 0bbh 
    je HighlightLeftUpWking
    jne piecefoundULWking
     
    piecefoundULWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightLeftUpWking
    jne checkLeftWKing

    HighlightLeftUpWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightUpLeftDiagonal

    checkLeftWKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    dec bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkRightWKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightLeftWking

    cmp AL, 0bbh  
    je HighlightLeftWking
    jne piecefoundLWking
     
    piecefoundLWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightLeftWking
    jne checkRightWKing

    HighlightLeftWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightLeft

    checkRightWKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    inc bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkBelowKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightRightWking

    cmp AL, 0bbh  
    je HighlightRightWking
    jne piecefoundRWking
     
    piecefoundRWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightRightWking
    jne checkBelowKing

    HighlightRightWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightRight

    checkBelowKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    add di,8d           ;we go below 1 cell
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkRightDownWKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h         ;check if empty
    je HighlightBelowWking ;if empty highlight

    cmp AL, 0bbh        
    je HighlightBelowWking 
    jne piecefoundBWking   ;else check piece 
     
    piecefoundBWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightBelowWking
    jne checkRightDownWKing

    HighlightBelowWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightDown

    checkRightDownWKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    add di,8d
    inc bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftDownWKing
    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightRightDownWking

    cmp AL, 0bbh  
    je HighlightRightDownWking
    jne piecefoundDRWking
     
    piecefoundDRWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightRightDownWking
    jne checkLeftDownWKing

    HighlightRightDownWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightDownRightDiagonal

    checkLeftDownWKing:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    
    add di,8d
    dec bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftWTabyeet

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightLeftDownWking

    cmp AL, 0bbh  
    je HighlightLeftDownWking
    jne piecefoundDLwking
     
    piecefoundDLWking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    je HighlightLeftDownWking
    jne checkLeftWTabyeet

    HighlightLeftDownWking:
    mov si,1h
    mov al, PlayerOneHighlightedCellColor
    call HighlightDownLeftDiagonal
    
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    ;==========================================================================================
    checkLeftWTabyeet:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    mov si,0

    cmp LeftWTabyaMovedFlag,1
    je checkRightWTabyeet
    cmp WKingMovedFlag,1
    je checkRightWTabyeet

    LoopLeftWTabyeet:
    dec bx
    call CheckBorders
    cmp OutsideBorders,1h
    je BreakLWTLoop

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je incrementLWT

    cmp AL, 0bbh  
    je incrementLWT
    jne BreakLWTLoop

    incrementLWT:
    inc si
    jmp LoopLeftWTabyeet

   BreakLWTLoop:
   cmp si,3
   jne checkRightWTabyeet
   cmp ChessboardGrid[bx][di], 12h
   jne checkRightWTabyeet

    HighlightLeftWTabyeet:
    dec si
    mov al, PlayerOneHighlightedCellColor
    call HighlightLeft
    
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]


    checkRightWTabyeet:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    mov si,0

    cmp RightWTabyaMovedFlag,1
    je endwkinghighlights
    cmp WKingMovedFlag,1
    je endwkinghighlights

    LoopRightWTabyeet:
    inc bx
    call CheckBorders
    cmp OutsideBorders,1h
    je BreakRWTLoop

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je incrementRWT

    cmp AL, 0bbh  
    je incrementRWT
    jne BreakRWTLoop

    incrementRWT:
    inc si
    jmp LoopRightWTabyeet

   BreakRWTLoop:
   cmp si,2
   jne endwkinghighlights
   cmp ChessboardGrid[bx][di], 12h
   jne endwkinghighlights

    HighlightRightWTabyeet:
    mov al, PlayerOneHighlightedCellColor
    call HighlightRight
    
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
   endwkinghighlights:

    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawWKingMoves endp

DrawWAskaryMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerOneSelectedPiecePosition[0], bx
    mov PlayerOneSelectedPiecePosition[2], di

    mov si,00h                              ;Reset counter
    sub di,8d                               ;Goes above one cell
    inc bx  ;Goes right one cell
    call checkBorders                                
    cmp OutsideBorders,1h
    je LeftDiagonalWAskary

    mov AL, ChessboardGrid[bx][di]         
    and AL, 0f0h                            ;To check if it is black

    cmp AL, 00h                             ;If black highlight the right above cell
    je HighlightRightDiagonal
    

    LeftDiagonalWAskary:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    
    dec bx
    sub di,8d
    call CheckBorders
    cmp OutsideBorders,1h
    je AboveWAskary

    mov AL, ChessboardGrid[bx][di]
    and AL, 0f0h                    
    cmp AL, 00h
    je HighlightLeftDiagonal
   

    AboveWAskary:
    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]

    sub di,8d
    call CheckBorders
    cmp OutsideBorders,1h
    je endDrawWAskaryMoves
    cmp ChessboardGrid[bx][di], 99h         ;Checks if cell empty
                                             ;If not empty return
    je HighlightAbove
    cmp ChessboardGrid[bx][di], 0BBh         ;Checks if cell has powerup
    jne endDrawWAskaryMoves                 
    je HighlightAbove

    HighlightRightDiagonal:
        inc si
        mov al, PlayerOneHighlightedCellColor
        call HighlightUpRightDiagonal
    jmp LeftDiagonalWAskary

    HighlightLeftDiagonal:
        inc si
        mov al, PlayerOneHighlightedCellColor
        call HighlightUpLeftDiagonal

    jmp AboveWAskary

    HighlightAbove:
        inc si
        add di,8d
        cmp di, 48d
        jne SkipIncrement
        sub di,16d
        mov al, ChessboardGrid[bx][di]
        cmp al, 099h
        je IncrementSI
        cmp al, 0bbh
        jne SkipIncrement
        IncrementSI:
        inc si
        SkipIncrement:
        mov al, PlayerOneHighlightedCellColor
        call HighlightUp

    endDrawWAskaryMoves:
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawWAskaryMoves endp

DrawBAskaryMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerTwoSelectedPiecePosition[0], bx
    mov PlayerTwoSelectedPiecePosition[2], di

    mov si,00h                              ;Reset counter
    add di,8d                               ;Goes above one cell
    inc bx  ;Goes right one cell
    call checkBorders                                
    cmp OutsideBorders,1h
    je LeftDiagonalBAskary

    mov AL, ChessboardGrid[bx][di]         
    and AL, 0f0h                            ;To check if it is black

    cmp AL, 10h                             ;If black highlight the right above cell
    je BHighlightRightDiagonal
    

    LeftDiagonalBAskary:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]
    
    dec bx
    add di,8d
    call CheckBorders
    cmp OutsideBorders,1h
    je AboveBAskary

    mov AL, ChessboardGrid[bx][di]
    and AL, 0f0h                    
    cmp AL, 10h
    je BHighlightLeftDiagonal
   

    AboveBAskary:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    add di,8d
    call CheckBorders
    cmp OutsideBorders,1h
    je endDrawBAskaryMoves
    cmp ChessboardGrid[bx][di], 99h         ;Checks if cell empty                 
    je BHighlightAbove
    cmp ChessboardGrid[bx][di], 0BBh         ;Checks if cell has powerup
    jne endDrawBAskaryMoves                 ;If not empty return
    je BHighlightAbove

    BHighlightRightDiagonal:
        inc si
        mov al, PlayerTwoHighlightedCellColor
        call HighlightDownRightDiagonal

    jmp LeftDiagonalBAskary

    BHighlightLeftDiagonal:
        inc si
        mov al, PlayerTwoHighlightedCellColor
        call HighlightDownLeftDiagonal

    jmp AboveBAskary

    BHighlightAbove:
        inc si
        sub di,8d
        cmp di, 8d
        jne BSkipIncrement
        add di,16d
        mov al, ChessboardGrid[bx][di]
        cmp al, 099h
        je IncrementBSI
        cmp al, 0bbh
        jne BSkipIncrement
        IncrementBSI:
        inc si
        BSkipIncrement:
        mov al, PlayerTwoHighlightedCellColor
        call HighlightDown

    endDrawBAskaryMoves:
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawBAskaryMoves endp
;==================================================================
DrawWHosanMoves proc

    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerOneSelectedPiecePosition[0], bx
    mov PlayerOneSelectedPiecePosition[2], di

        call GetCXDXFromBXDI
        sub di, 16d                                 ;Checks the above left position
        dec bx
        sub dx, 2*ChessPieceHeight
        sub cx, ChessPieceWidth
        

        cmp di, 0h                                  ;Checks if it exceeded the top border
        jl  AboveRightHosanCell
        cmp bx, 0h                                  ;Checks if it exceeded the left border
        jl AboveRightHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je AboveRightHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    AboveRightHosanCell:
        add bx,2H                                   ;Cheks the above right position
        add cx, 2*ChessPieceWidth

        cmp di,0h
        jl RightAboveHosanCell
        cmp bx, 7h
        ja RightAboveHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je RightAboveHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    RightAboveHosanCell:
        inc bx
        add di,8d
        add cx, ChessPieceWidth
        add dx, ChessPieceHeight

        cmp di,0h
        jl LeftAboveHosanCell
        cmp bx,7h
        ja LeftAboveHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je LeftAboveHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    LeftAboveHosanCell:
        sub bx, 4d
        sub cx, 4*ChessPieceWidth

        cmp di,0h
        jl LeftBelowHosanCell
        cmp bx, 0h
        jl LeftBelowHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je LeftBelowHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    LeftBelowHosanCell:
        add di, 16d
        add dx, 2*ChessPieceHeight

        cmp di, 56d
        ja BelowLeftHosanCell
        cmp bx, 0h
        jl BelowLeftHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je BelowLeftHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    BelowLeftHosanCell:
        add di, 8d
        inc bx
        add cx, ChessPieceWidth
        add dx, ChessPieceHeight

        cmp di,56d
        ja BelowRightHosanCell
        cmp bx,0h
        jl BelowRightHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je BelowRightHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    BelowRightHosanCell:
        add bx, 2d
        add cx, 2*ChessPieceWidth

        cmp di, 56d
        ja RightBelowHosanCell
        cmp bx,7d
        ja RightBelowHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece        
        je RightBelowHosanCell

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        call GetCXDXFromBXDI
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    RightBelowHosanCell:
        inc bx
        sub di,8d
        add cx, ChessPieceWidth
        sub dx, ChessPieceHeight

        cmp di,56d
        ja endDrawWHosanMoves
        cmp bx, 7d
        ja endDrawWHosanMoves

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 10H                                 ;Checks for white piece
        je endDrawWHosanMoves

        mov al, PlayerOneHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerOneStoreHighlightedIndex

    endDrawWHosanMoves:
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawWHosanMoves endp

DrawBHosanMoves proc

    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerTwoSelectedPiecePosition[0], bx
    mov PlayerTwoSelectedPiecePosition[2], di

        call GetCXDXFromBXDI
        sub di, 16d                                 ;Checks the above left position
        dec bx
        sub dx, 2*ChessPieceHeight
        sub cx, ChessPieceWidth
        

        cmp di, 0h                                  ;Checks if it exceeded the top border
        jl  BAboveRightHosanCell
        cmp bx, 0h                                  ;Checks if it exceeded the left border
        jl BAboveRightHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BAboveRightHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call StorePlayerIndex

    BAboveRightHosanCell:
        add bx,2H                                   ;Cheks the above right position
        add cx, 2*ChessPieceWidth

        cmp di,0h
        jl BRightAboveHosanCell
        cmp bx, 7h
        ja BRightAboveHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BRightAboveHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    BRightAboveHosanCell:
        inc bx
        add di,8d
        add cx, ChessPieceWidth
        add dx, ChessPieceHeight

        cmp di,0h
        jl BLeftAboveHosanCell
        cmp bx,7h
        ja BLeftAboveHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BLeftAboveHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    BLeftAboveHosanCell:
        sub bx, 4d
        sub cx, 4*ChessPieceWidth

        cmp di,0h
        jl BLeftBelowHosanCell
        cmp bx, 0h
        jl BLeftBelowHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BLeftBelowHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    BLeftBelowHosanCell:
        add di, 16d
        add dx, 2*ChessPieceHeight

        cmp di, 56d
        ja BBelowLeftHosanCell
        cmp bx, 0h
        jl BBelowLeftHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BBelowLeftHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    BBelowLeftHosanCell:
        add di, 8d
        inc bx
        add cx, ChessPieceWidth
        add dx, ChessPieceHeight

        cmp di,56d
        ja BBelowRightHosanCell
        cmp bx,0h
        jl BBelowRightHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BBelowRightHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    BBelowRightHosanCell:
        add bx, 2d
        add cx, 2*ChessPieceWidth

        cmp di, 56d
        ja BRightBelowHosanCell
        cmp bx,7d
        ja BRightBelowHosanCell

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece        
        je BRightBelowHosanCell

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
        call GetCXDXFromBXDI
        ;call DrawChessCell
        ;call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    BRightBelowHosanCell:
        inc bx
        sub di,8d
        add cx, ChessPieceWidth
        sub dx, ChessPieceHeight

        cmp di,56d
        ja endBDrawWHosanMoves
        cmp bx, 7d
        ja endBDrawWHosanMoves

        mov AH, ChessboardGrid[bx][di]              ;Gets the piece in this position
        and AH, 0f0h
        cmp AH, 00H                                 ;Checks for Black piece
        je endBDrawWHosanMoves

        mov al, PlayerTwoHighlightedCellColor       ;If we went here therefore we can draw it
       ; call DrawChessCell
       ; call DrawChessPiece
        call PlayerTwoStoreHighlightedIndex

    endBDrawWHosanMoves:
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawBHosanMoves endp

;--------------------------------------------------------

DrawBTabyaMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerTwoSelectedPiecePosition[0], bx
    mov PlayerTwoSelectedPiecePosition[2], di

    mov si, 00h                      ;Reset the counter
 
    AboveBTabya:
    sub di,8d
    call CheckBorders
    cmp OutsideBorders,1
    je BContinue1
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je BCountHighUp
    cmp AL, 0bbh         
    jne CheckIfWhiteAbovePiece                ;If not empty check if white piece
    je BCountHighUp

    BCountHighUp:
        inc si      ; One cell counted to be highlighted
        jmp AboveBTabya

    CheckIfWhiteAbovePiece:
    and AL,0f0h
    cmp AL,10h
    je CountWhiteAboveThenEnd                          ;if black piece found
    jne BContinue1

    CountWhiteAboveThenEnd:
        inc si
        jmp BContinue1
    BContinue1:

    mov al, PlayerTwoHighlightedCellColor
    CALL HighlightUp
    mov bx,PlayerTwoSelectedPiecePosition[0]
    mov di,PlayerTwoSelectedPiecePosition[2]
    
    mov si,00h
    BBelowTabya:
    add di,8d
    call checkBorders
    cmp OutsideBorders,1
    je BContinue2
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je BCountHighBelow
    cmp AL, 0bbh         
    jne CheckIfWhitePieceBelow                ;If not empty check if white piece
    je BCountHighBelow

    BCountHighBelow:
        inc si      ; One cell counted to be highlighted
        jmp BBelowTabya

    CheckIfWhitePieceBelow:
    and AL,0f0h
    cmp AL,10h
    je CountWhiteBelowThenEnd                          ;if black piece found
    jne BContinue2

    CountWhiteBelowThenEnd:
        inc si
        jmp BContinue2

    BContinue2:
    mov al,PlayerTwoHighlightedCellColor
    cmp si,00h
    
    CALL HighlightDown
    mov bx,PlayerTwoSelectedPiecePosition[0]
    mov di,PlayerTwoSelectedPiecePosition[2]

        ;------------------------------------------------
BRightTabya:
    inc bx
    call checkBorders
    cmp OutsideBorders,1
    je BContinue3
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je BCountHighRight
    cmp AL, 0bbh        
    jne CheckIfWhiteRightPiece                ;If not empty check if white piece
    je BCountHighRight

    BCountHighRight:
        inc si      ; One cell counted to be highlighted
        jmp BRightTabya

    CheckIfWhiteRightPiece:
    and AL,0f0h
    cmp AL,10h
    je CountWhiteRightThenEnd                          ;if black piece found
    jne BContinue3

    CountWhiteRightThenEnd:
        inc si
        jmp BContinue3
    BContinue3:
    mov al, PlayerTwoHighlightedCellColor
    ;cmp si,00h
    
    CALL HighlightRight
    mov bx,PlayerTwoSelectedPiecePosition[0]
    mov di,PlayerTwoSelectedPiecePosition[2]
    
    mov si,00h
    BLeftTabya:
    dec bx
    call checkBorders
    cmp OutsideBorders,1
    je endDrawBTabyaMoves
    mov AL, ChessboardGrid[bx][di]
    cmp AL, 99h         ;Checks if cell empty
    je BCountHighLeft
    cmp AL, 0bbh       
    jne CheckIfWhitePieceLeft                ;If not empty check if white piece
    je BCountHighLeft

    BCountHighLeft:
        inc si      ; One cell counted to be highlighted
        jmp BLeftTabya

    CheckIfWhitePieceLeft:
    and AL,0f0h
    cmp AL,10h
    je CountWhiteLeftThenEnd                          ;if black piece found
    jne endDrawBTabyaMoves

    CountWhiteLeftThenEnd:
        inc si
    

    ;------------------------------------------------
    
    endDrawBTabyaMoves:
    mov al,PlayerTwoHighlightedCellColor
    CALL HighlightLeft
    mov bx,PlayerTwoSelectedPiecePosition[0]
    mov di,PlayerTwoSelectedPiecePosition[2]
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawBTabyaMoves endp
;--------------------------------------------------------

DrawBFilMoves proc 
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerTwoSelectedPiecePosition[0], bx
    mov PlayerTwoSelectedPiecePosition[2], di
    mov si,00h                              ;Reset counter

    BFilRightUpDiagonalHighlights:

    sub di,8d                               ;Goes above one cell
    inc bx                                  ;Goes right one cell
    call CheckBorders
    
    cmp OutsideBorders,1h
    je HighlightRightUpDiagonalBFil 
    
    ;InsideBorders:
    mov AL, ChessboardGrid[bx][di]         

    cmp AL, 99h
    je incrementCounterB1                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounterB1                    
    jne PieceFoundRUBFil
    
    PieceFoundRUBFil:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If white, increment the counter
    jne incrementCounterB1 
    je HighlightRightUpDiagonalBFil

    incrementCounterB1:
    inc si
    cmp AL, 99h                             ;If empty check again
    je BFilRightUpDiagonalHighlights

    cmp AL, 0bbh                             ;If empty check again
    jne HighlightRightUpDiagonalBFil
    je BFilRightUpDiagonalHighlights

    HighlightRightUpDiagonalBFil:
    mov al, PlayerTwoHighlightedCellColor
    call HighlightUpRightDiagonal

    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    mov si,00h                              ;Reset counter

    BFilLeftUpDiagonalHighlights:

    sub di,8d                               ;Goes above one cell
    dec bx                                  ;Goes LEFT  one cell

    call checkBorders
    cmp OutsideBorders,1h
    je HighlightLeftUpDiagonalBFil 

    ;InsideBorders: 
    mov AL, ChessboardGrid[bx][di]         
     
    
    cmp AL, 99h
    je incrementCounterB2                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounterB2                     
    jne PieceFoundLUBFil
    
    PieceFoundLUBFil:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If White, increment the counter
    jne incrementCounterB2 
    je HighlightLeftUpDiagonalBFil

    incrementCounterB2:
    inc si
    cmp AL, 99h                             ;If empty check again 
    je BFilLeftUpDiagonalHighlights

    cmp AL, 0bbh                             ;If empty check again 
    jne HighlightLeftUpDiagonalBFil
    je BFilLeftUpDiagonalHighlights

    HighlightLeftUpDiagonalBFil:
    mov al, PlayerTwoHighlightedCellColor
    call HighlightUpLeftDiagonal

    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    mov si,00h                              ;Reset counter

    BFilRightDownDiagonalHighlights:

    add di,8d                               ;Goes down one cell
    inc bx                                  ;Goes right one cell

    call checkBorders
    cmp OutsideBorders,1h
    je HighlightRightDownDiagonalBFil 

    ;InsideBorders:
    mov AL, ChessboardGrid[bx][di]         

    cmp AL, 99h
    je incrementCounterB3                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounterB3                     
    jne PieceFoundRDBFil
    
    PieceFoundRDBFil:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If white, increment the counter
    jne incrementCounterB3 
    je HighlightRightDownDiagonalBFil

    incrementCounterB3:
    inc si
    cmp AL, 99h                             ;If empty check again
    je BFilRightDOWNDiagonalHighlights

    cmp AL, 0bbh                             ;If empty check again
    jne HighlightRightDOWNDiagonalBFil
    je BFilRightDOWNDiagonalHighlights


    HighlightRightDOWNDiagonalBFil:
    mov al, PlayerTwoHighlightedCellColor
    call HighlightDownRightDiagonal

    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]
    mov si,00h                              ;Reset counter

    BFilLeftDOWNDiagonalHighlights:

    ADD di,8d                               ;Goes DOWN one cell
    dec bx                                  ;Goes LEFT one cell

    call checkBorders
    cmp OutsideBorders,1h
    je HighlightLeftDownDiagonalBFil 

    ;InsideBorders:
    mov AL, ChessboardGrid[bx][di]         

    cmp AL, 99h
    je incrementCounterB4                     ;If cell is empty, increment the counter

    cmp AL, 0bbh
    je incrementCounterB4                     
    jne PieceFoundLDBfil
    
    PieceFoundLDBFil:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If white, increment the counter
    jne incrementCounterB4 
    je HighlightLeftDOWNDiagonalBFil

    incrementCounterB4:
    inc si
    cmp AL, 99h                             ;If empty check again
    je BFilLeftDOWNDiagonalHighlights

    cmp AL, 0bbh                             ;If empty check again
    jne HighlightLeftDOWNDiagonalBFil
    je BFilLeftDOWNDiagonalHighlights

    HighlightLeftDOWNDiagonalBFil:
    mov al, PlayerTwoHighlightedCellColor
    call HighlightDownLeftDiagonal

    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

   
    endDrawBFilMoves:
    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawBFilMoves endp

DrawBQueenMoves proc
    call DrawBTabyaMoves
    call DrawBFilMoves
ret
DrawBQueenMoves endp

DrawBkingMoves proc
    push bx
    push di
    push ax
    push cx
    push dx
    push si

    mov PlayerTwoSelectedPiecePosition[0], bx
    mov PlayerTwoSelectedPiecePosition[2], di

    checkAboveBKing:
    sub di,8d           ;we go above 1 cell

    call CheckBorders
    cmp OutsideBorders,1h
    je checkRightUpBKing
    
    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h         ;check if empty
    je HighlightAboveBking ;if empty highlight

    cmp AL, 0bbh         
    je HighlightAboveBking 
    jne piecefoundUBking   ;else check piece 
     
    piecefoundUBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightAboveBking
    je checkRightUpBKing

    HighlightAboveBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightUp


    checkRightUpBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    sub di,8d
    inc bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftUpBKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightRightUpBking

    cmp AL, 0bbh  ;check if empty
    je HighlightRightUpBking
    jne piecefoundURBking
     
    piecefoundURBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightRightUpBking
    je checkLeftUpBKing

    HighlightRightUpBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightUpRightDiagonal
    

    checkLeftUpBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    sub di,8d
    dec bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftBKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightLeftUpBking

    cmp AL, 0bbh  
    je HighlightLeftUpBking
    jne piecefoundULBking
     
    piecefoundULBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightLeftUpBking
    je checkLeftBKing

    HighlightLeftUpBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightUpLeftDiagonal

    checkLeftBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    dec bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkRightBKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightLeftBking

    cmp AL, 0bbh  
    je HighlightLeftBking
    jne piecefoundLBking
     
    piecefoundLBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightLeftBking
    je checkRightBKing

    HighlightLeftBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightLeft

    checkRightBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    inc bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkBelowBKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightRightBking

    cmp AL, 0bbh  
    je HighlightRightBking
    jne piecefoundRBking
     
    piecefoundRBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightRightBking
    je checkBelowBKing

    HighlightRightBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightRight

    checkBelowBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    add di,8d           ;we go below 1 cell
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkRightDownBKing

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h         ;check if empty
    je HighlightBelowBking ;if empty highlight

    cmp AL, 0bbh        
    je HighlightBelowBking 
    jne piecefoundBBking   ;else check piece 
     
    piecefoundBBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightBelowBking
    je checkRightDownBKing

    HighlightBelowBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightDown

    checkRightDownBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    add di,8d
    inc bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftDownBKing
    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightRightDownBking

    cmp AL, 0bbh  
    je HighlightRightDownBking
    jne piecefoundDRBking
     
    piecefoundDRBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightRightDownBking
    je checkLeftDownBKing

    HighlightRightDownBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightDownRightDiagonal

    checkLeftDownBKing:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]
    
    add di,8d
    dec bx
    call CheckBorders
    
    cmp OutsideBorders,1h
    je checkLeftBTabyeet

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je HighlightLeftDownBking

    cmp AL, 0bbh  
    je HighlightLeftDownBking
    jne piecefoundDLBking
     
    piecefoundDLBking:
    and AL, 0f0h                            ;To check if it is black
    cmp AL, 00h                             ;If black
    jne HighlightLeftDownBking
    je checkLeftBTabyeet

    HighlightLeftDownBking:
    mov si,1h
    mov al, PlayerTwoHighlightedCellColor
    call HighlightDownLeftDiagonal
    
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    ;==========================================================================================
    checkLeftBTabyeet:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]
    mov si,0

    cmp LeftBTabyaMovedFlag,1
    je checkRightBTabyeet
    cmp BKingMovedFlag,1
    je checkRightBTabyeet

    LoopLeftBTabyeet:
    dec bx
    call CheckBorders
    cmp OutsideBorders,1h
    je BreakLBTLoop

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je incrementLBT

    cmp AL, 0bbh  
    je incrementLBT
    jne BreakLBTLoop

    incrementLBT:
    inc si
    jmp LoopLeftBTabyeet

   BreakLBTLoop:
   cmp si,3
   jne checkRightBTabyeet
   cmp ChessboardGrid[bx][di], 02h
   jne checkRightBTabyeet

    HighlightLeftBTabyeet:
    dec si
    mov al, PlayerTwoHighlightedCellColor
    call HighlightLeft
    
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]


    checkRightBTabyeet:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]
    mov si,0

    cmp RightBTabyaMovedFlag,1
    je endBkinghighlights
    cmp BKingMovedFlag,1
    je endBkinghighlights

    LoopRightBTabyeet:
    inc bx
    call CheckBorders
    cmp OutsideBorders,1h
    je BreakRBTLoop

    mov al,ChessboardGrid[bx][di] 

    cmp AL, 99h  ;check if empty
    je incrementRBT

    cmp AL, 0bbh  
    je incrementRBT
    jne BreakRBTLoop

    incrementRBT:
    inc si
    jmp LoopRightBTabyeet

   BreakRBTLoop:
   cmp si,2
   jne endBkinghighlights
   cmp ChessboardGrid[bx][di], 02h
   jne endBkinghighlights

    HighlightRightBTabyeet:
    mov al, PlayerTwoHighlightedCellColor
    call HighlightRight
    
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

   endBkinghighlights:

    pop si
    pop dx
    pop cx
    pop ax
    pop di
    pop bx

ret
DrawBKingMoves endp


CheckCellColor proc ;Calls interrupt that gets pixel colour
    push bx
    mov bh,0H
    mov ah,0dh
    int 10h
    pop bx
ret
CheckCellColor endp

CheckNewCellToDraw proc

    cmp BlackORWhite, 0
    je BlackNewCellToDraw

    cmp al, PlayerOneHighlightedCellColor
    je HighlightSelectedW

    cmp al, PlayerTwoHighlightedCellColor
    je PlayerOneOnPlayerTwo

    cmp al, OverLappedHighlightingColor
    je OverlappedSelected

    mov al, PlayerOneSelectedCellColor
    jmp endCheckNewCellToDraw

    HighlightSelectedW:
    mov al, PlayerOneHighlightedSelectedCellColor
    jmp endCheckNewCellToDraw

    PlayerOneOnPlayerTwo:
    mov al, PlayerOneOnPlayerTwoHighlightColor
    jmp endCheckNewCellToDraw

    OverlappedSelected:
    mov al, OverLappedHighlightedSelectedColor
    jmp endCheckNewCellToDraw

    BlackNewCellToDraw:
    cmp al, PlayerTwoHighlightedCellColor
    je HighlightSelectedB

    cmp al, PlayerOneHighlightedCellColor
    je PlayerTwoOnPlayerOne

    cmp al, OverLappedHighlightingColor
    je OverlappedSelected

    mov al, PlayerTwoSelectedCellColor
    jmp endCheckNewCellToDraw

    HighlightSelectedB:
    mov al, PlayerTwoHighlightedSelectedCellColor
    jmp endCheckNewCellToDraw

    PlayerTwoOnPlayerOne:
    mov al, PlayerTwoOnPlayerOneHighlightColor

    endCheckNewCellToDraw:
ret
CheckNewCellToDraw endp
;===================================HighLighting Procs===========================================

ChooseHighlightingColor proc

    call CheckCellColor

    cmp BlackORWhite, 1
    je WhiteHighlightChecks

    cmp al, PlayerOneHighlightedCellColor
    jne BlackHighlightCheck1
    mov al, OverLappedHighlightingColor
    jmp endChooseHighlightingColor

    BlackHighlightCheck1:
    cmp al, PlayerOneSelectedCellColor
    jne PlayerTwoNomralHighlightingColor
    mov al, PlayerOneOnPlayerTwoHighlightColor
    jmp endChooseHighlightingColor

    PlayerTwoNomralHighlightingColor:
    mov al, PlayerTwoHighlightedCellColor
    jmp endChooseHighlightingColor

    WhiteHighlightChecks:
    cmp al, PlayerTwoHighlightedCellColor
    jne WhiteHighlightCheck1
    mov al, OverLappedHighlightingColor
    jmp endChooseHighlightingColor

    WhiteHighlightCheck1:
    cmp al, PlayerTwoSelectedCellColor
    jne PlayerOneNormalHighlightingColor
    mov al, PlayerTwoOnPlayerOneHighlightColor
    jmp endChooseHighlightingColor

    PlayerOneNormalHighlightingColor:
    mov al, PlayerOneHighlightedCellColor

    endChooseHighlightingColor:
ret
ChooseHighlightingColor endp

StorePlayerIndex proc     ;If white piece store the index of the first player's array
    cmp BlackORWhite,1
    jne blackIndex
    je skip
    blackIndex:
    call PlayerTwoStoreHighlightedIndex
    jmp endstoreplayerindex
    skip:
    call PlayerOneStoreHighlightedIndex
    endstoreplayerindex:
ret
StorePlayerIndex endp

Highlight proc        ;Responsible for highlighting cells stored in the array of each player
    push si
    push bx
    push dx
    push cx
    push di
    
    mov si,0h
    cmp BlackORWhite,1h
    jne BHighlight

    WHighlight:
    mov al, PlayerOneHighlightedCellColor
    cmp si,PlayerOneHighlightedCellsIndex
    je endHighlight
    mov bx,PlayerOneHighlightedCells[si]
    call GetBXDIFromBX                     ;Gets rows and cols from cell number
    call GetCXDXFromBXDI
    call DrawChessCell
    call DrawChessPiece
    add si,2d
    jmp WHighlight

    BHighlight:
    mov al, PlayerTwoHighlightedCellColor
    cmp si,PlayerTwoHighlightedCellsIndex
    je endHighlight
    mov bx,PlayerTwoHighlightedCells[si]
    call GetBXDIFromBX
    call GetCXDXFromBXDI
    call DrawChessCell
    call DrawChessPiece
    add si,2d
    jmp BHighlight

    endhighlight:
    pop di
    pop cx
    pop dx
    pop bx
    pop si
ret
Highlight endp

HighlightUpRightDiagonal proc
    push cx
    push dx

    cmp si,00h
    je endHighlightUpRightDiagonal

    cmp BlackORWhite,1
    jne blackCase

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue

    blackCase:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightUpRightDiagonalLoop:
        inc bx
        sub di,8d
        call StorePlayerIndex
        add cx, ChessPieceWidth
        sub dx, ChessPieceHeight
        call ChooseHighlightingColor
        dec si
    jnz HighlightUpRightDiagonalLoop
    
    pop di
    pop bx

    endHighlightUpRightDiagonal:
 
    pop dx
    pop cx

ret
HighlightUpRightDiagonal endp

HighlightUpLeftDiagonal proc
    push cx
    push dx

    cmp si, 00h
    je endHighlightUpLeftDiagonal

    cmp BlackORWhite,1
    jne blackCase1

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue1

    blackCase1:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue1:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightUpLeftDiagonalLoop:
        dec bx
        sub di,8d
        call StorePlayerIndex
        sub cx, ChessPieceWidth
        sub dx, ChessPieceHeight
        call ChooseHighlightingColor
        dec si
    jnz HighlightUpLeftDiagonalLoop
    
    pop di
    pop bx

    endHighlightUpLeftDiagonal:
    
    pop dx
    pop cx
ret
HighlightUpLeftDiagonal endp

HighlightUp proc
    push cx
    push dx

    cmp si,0h
    je endHighlightUp

    cmp BlackORWhite,1
    jne blackCase2

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue2

    blackCase2:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    labelContinue2:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightUpLoop:
        sub di,8d
        call StorePlayerIndex
        sub dx, ChessPieceHeight
        call ChooseHighlightingColor
        dec si
    jnz HighlightUpLoop
    
    pop di
    pop bx

    endHighlightUp:
    
    pop dx
    pop cx
ret
HighlightUp endp

HighlightDown proc
    push cx
    push dx

    cmp si,0h
    je endHighlightDown

    cmp BlackORWhite,1
    jne blackCase3

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue3

    blackCase3:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue3:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightDownLoop:
        add di,8d
        call StorePlayerIndex
        add dx, ChessPieceHeight
        call ChooseHighlightingColor
        dec si
    jnz HighlightDownLoop
    
    pop di
    pop bx

    endHighlightDown:
    
    pop dx
    pop cx
ret
HighlightDown endp

HighlightRight proc
    push cx
    push dx

    cmp si,0h
    je endHighlightRight

    cmp BlackORWhite,1
    jne blackCase4

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue4

    blackCase4:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue4:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightRightLoop:
        inc bx
        call StorePlayerIndex
        add cx, ChessPieceWidth
        call ChooseHighlightingColor
        dec si
    jnz HighlightRightLoop
    
    pop di
    pop bx

    endHighlightRight:
    pop dx
    pop cx
ret
HighlightRight endp

HighlightLeft proc
    push cx
    push dx

    cmp si,0h
    je endHighlightLeft

    cmp BlackORWhite,1
    jne blackCase7

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue7

    blackCase7:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue7:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightLeftLoop:
        dec bx
        call StorePlayerIndex
        sub cx, ChessPieceWidth
        call ChooseHighlightingColor
        dec si
    jnz HighlightLeftLoop
    
    pop di
    pop bx

    endHighlightLeft:
    pop dx
    pop cx
ret
HighlightLeft endp

HighlightDownLeftDiagonal proc
    push cx
    push dx

    cmp si,0h
    je endHighlightDownLeftDiagonal

    cmp BlackORWhite,1
    jne blackCase5

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue5

    blackCase5:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue5:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightDownLeftDiagonalLoop:
        dec bx
        add di,8d
        call StorePlayerIndex
        sub cx, ChessPieceWidth
        add dx, ChessPieceHeight
        call ChooseHighlightingColor
        dec si
    jnz HighlightDownLeftDiagonalLoop
    
    pop di
    pop bx

    endHighlightDownLeftDiagonal:
    pop dx
    pop cx
ret
HighlightDownLeftDiagonal endp

HighlightDownRightDiagonal proc
    push cx
    push dx

    cmp si,0h
    je endHighlightDownRightDiagonal

    cmp BlackORWhite,1
    jne blackCase6

    mov bx, PlayerOneSelectedPiecePosition[0]
    mov di, PlayerOneSelectedPiecePosition[2]
    jmp labelContinue6

    blackCase6:
    mov bx, PlayerTwoSelectedPiecePosition[0]
    mov di, PlayerTwoSelectedPiecePosition[2]

    

    labelContinue6:
    push bx
    push di
    call GetCXDXFromBXDI

    HighlightDownRightDiagonalLoop:
        inc bx
        add di,8d
        call StorePlayerIndex
        add cx, ChessPieceWidth
        add dx, ChessPieceHeight
        call ChooseHighlightingColor
        dec si
    jnz HighlightDownRightDiagonalLoop
    
    pop di
    pop bx

    endHighlightDownRightDiagonal:
    pop dx
    pop cx

ret
HighlightDownRightDiagonal endp


;================================================================================================
WMovePiece proc

    push ax
    push bx
    push di
    push dx
    push cx
    
    cmp SkipWSend, 1
    je SkipWhiteSend
;=======================To Send Data to Other Player======================
    push cx
    push ax
    mov CX, PlayerOneSelectedPiecePosition[0]
    add cx, PlayerOneSelectedPiecePosition[2]
    mov ax, bx
    add ax, di

    mov ah, cl
    mov DataToSend[0], ah
    mov DataToSend[1], al
    mov isData, 1
    pop ax
    pop cx
;============================================================================
    SkipWhiteSend:
    mov dl,ChessboardGrid[bx][di]
    call WhatPieceDied

    cmp dl,0BBh               ;If cell contained powerup the flag is set to 1 (White has it)
    jne  SkipWPowerUp
    mov HasBonus,1

    SkipWPowerUp:
    xchg bx, PlayerOneSelectedPiecePosition[0]
    xchg di, PlayerOneSelectedPiecePosition[2]

    ;=====================================================================

    add bx,di
    cmp bx,56d
    je MarkLeftWTabyaMoved
    cmp bx,63d
    je MarkRightWTabyaMoved
    cmp bx,60d
    je MarkWKingMoved
    jne endWTabyeet

    MarkLeftWTabyaMoved:
    mov LeftWTabyaMovedFlag,1
    jmp endWTabyeet
    MarkRightWTabyaMoved:
    mov RightWTabyaMovedFlag,1
    jmp endWTabyeet
    MarkWKingMoved:
    mov WKingMovedFlag,1
   
    xchg bx, PlayerOneSelectedPiecePosition[0]          ;get new pos
    xchg di, PlayerOneSelectedPiecePosition[2]
    add bx,di
    push bx
    push di
    push cx
    push dx
    
    cmp bx,58d
    je LeftWTabyeet
    cmp bx,62d
    je RightWTabyeet
    jne endWTabyeetMoves

    LeftWTabyeet:
    mov ChessboardGrid[0][56],99h
    mov ChessboardGrid[3][56],12h
    mov bx,00
    mov di,56d
    call GetCXDXFromBXDI
    mov al,DarkCellColor
    call DrawChessCell
    call DrawChessPiece
    jmp endWTabyeetMoves
  
    RightWTabyeet:
    mov ChessboardGrid[7][56],99h
    mov ChessboardGrid[5][56],12h
    mov bx,7
    mov di,56d
    call GetCXDXFromBXDI
    mov al,LightCellColor
    call DrawChessCell
    call DrawChessPiece

    endWTabyeetMoves:
    pop dx
    pop cx
    pop di
    pop bx
    call GetBXDIFromBX
    xchg bx, PlayerOneSelectedPiecePosition[0]
    xchg di, PlayerOneSelectedPiecePosition[2]

    endWTabyeet:
    call GetBXDIFromBX

;======================================================================

    mov AH, ChessboardGrid[bx][di]        ;stores the piece moving in AH 
    mov ChessboardGrid[bx][di], 99h       ;Empties the original cell

    call GetCXDXFromBXDI
    call CheckNormalCellColor
    call DrawChessCell

    xchg bx, PlayerOneSelectedPiecePosition[0]
    xchg di, PlayerOneSelectedPiecePosition[2]
    mov ChessboardGrid[bx][di], AH         ;puts the moved piece in the new cell
    cmp ah,11h
    jne  notWaskaryPiece
    cmp di,00d
    jne notWaskaryPiece
    mov ChessboardGrid[bx][di],15h
    push dx
    mov dx,offset whiteAskaryWazeer
    call UpdateStatusBar
    pop dx

notWaskaryPiece:
    cmp ah, 06h
    jne NotBlackKing
    mov BlackKingPosition[0], bx
    mov BlackKingPosition[2], di

    NotBlackKing:

    cmp ah, 16h
    jne NotWhiteKing
    mov WhiteKingPosition[0], bx
    mov WhiteKingPosition[2], di

    NotWhiteKing:

    call GetCXDXFromBXDI
    mov PlayerOneSelectedPiecePosition[0], 0ffffH
    mov PlayerOneSelectedPiecePosition[2], 0ffffH


    call PlayerOneRemoveHighlightedCells            ;Removing all highlighted cells

    mov al, PlayerOneSelectedCellColor
    call DrawChessCell
    call DrawChessPiece
    push dx
    mov dh,ChessboardGrid[bx][di]
    call check                                       ;If there is a kesh
    pop dx
    
    ;--------------White Wins----------;
    cmp BlackKingPosition[0],bx
    je CheckDi
    jmp skipp1
    CheckDi:
    cmp BlackKingPosition[2],di
    je EndGameWhite                                 ;If the cell contained the black king then white won
    skipp1:
    
    call StoreCoolDown                              ;Stores time if this cell

    endWMovePiece:
    pop cx
    pop dx
    pop di
    pop bx
    pop ax

ret
    EndGameWhite:
    ;call EndGameWhiteWins
    mov KingDied,1
    mov LeaveGameFlag,1
    mov dx, offset PressAnyKey
    call UpdateStatusBar
    mov dx,0153h
    mov ah,02h
    int 10h
    mov dx, offset WhiteWon
    call displaystring
jmp endWMovePiece
WMovePiece endp


EndGameWhiteWinsTemp: jmp EndGameWhiteWins           ;Kobry


BMovePiece proc                      ;similar to wmovepiece

    push ax
    push bx
    push di
    push bp
    push si
    push dx

    cmp SkipBSend, 1
    je SkipBlackSend
;=======================To Send Data to Other Player======================
    push cx
    push ax
    mov CX, PlayerTwoSelectedPiecePosition[0]
    add cx, PlayerTwoSelectedPiecePosition[2]
    mov ax, bx
    add ax, di

    mov ah, cl
    mov DataToSend[0], ah
    mov DataToSend[1], al
    mov isData, 1
    pop ax
    pop cx
;============================================================================

    SkipBlackSend:
    mov dl,ChessboardGrid[bx][di]
    call WhatPieceDied

    cmp dl,0BBh
    jne  SkipBPowerUp
    mov HasBonus,2

    SkipBPowerUp:

    xchg bx, PlayerTwoSelectedPiecePosition[0]
    xchg di, PlayerTwoSelectedPiecePosition[2]

    ;=====================================================================

    add bx,di
    cmp bx,0d
    je MarkLeftBTabyaMoved
    cmp bx,7d
    je MarkRightBTabyaMoved
    cmp bx,4d
    je MarkBKingMoved
    jne endBTabyeet

    MarkLeftBTabyaMoved:
    mov LeftBTabyaMovedFlag,1
    jmp endBTabyeet
    MarkRightBTabyaMoved:
    mov RightBTabyaMovedFlag,1
    jmp endBTabyeet
    MarkBKingMoved:
    mov BKingMovedFlag,1
   
    xchg bx, PlayerTwoSelectedPiecePosition[0]
    xchg di, PlayerTwoSelectedPiecePosition[2]
    add bx,di
    push bx
    push di
    push cx
    push dx
    
    cmp bx,2d
    je LeftBTabyeet
    cmp bx,6d
    je RightBTabyeet
    jne endBTabyeetMoves

    LeftBTabyeet:
    mov ChessboardGrid[0][0],99h
    mov ChessboardGrid[3][0],02h
    mov bx,00
    mov di,0d
    call GetCXDXFromBXDI
    mov al,LightCellColor
    call DrawChessCell
    call DrawChessPiece
    jmp endBTabyeetMoves
  
    RightBTabyeet:
    mov ChessboardGrid[7][0],99h
    mov ChessboardGrid[5][0],02h
    mov bx,7
    mov di,0
    call GetCXDXFromBXDI
    mov al,DarkCellColor
    call DrawChessCell
    call DrawChessPiece

    endBTabyeetMoves:
    pop dx
    pop cx
    pop di
    pop bx
    call GetBXDIFromBX
    xchg bx, PlayerTwoSelectedPiecePosition[0]
    xchg di, PlayerTwoSelectedPiecePosition[2]

    endBTabyeet:
    call GetBXDIFromBX

;======================================================================

    mov AH, ChessboardGrid[bx][di]
    mov ChessboardGrid[bx][di], 99h

    call GetCXDXFromBXDI
    call CheckNormalCellColor
    call DrawChessCell

    xchg bx, PlayerTwoSelectedPiecePosition[0]
    xchg di, PlayerTwoSelectedPiecePosition[2]
    mov ChessboardGrid[bx][di], AH
    cmp ah,01h
    jne  notBaskaryPiece
    cmp di,56d
    jne notBaskaryPiece
    mov ChessboardGrid[bx][di],05h
    push dx
    mov dx,offset blackAskaryWazeer
    call UpdateStatusBar
    pop dx

notBaskaryPiece:

    cmp ah, 06h
    jne NotBlackKKing
    mov BlackKingPosition[0], bx
    mov BlackKingPosition[2], di
    NotBlackKKing:

    cmp ah, 16h
    jne NotWhiteKKing
    mov WhiteKingPosition[0], bx
    mov WhiteKingPosition[2], di
    NotWhiteKKing:

    call GetCXDXFromBXDI
    mov PlayerTwoSelectedPiecePosition[0], 0ffffH
    mov PlayerTwoSelectedPiecePosition[2], 0ffffH

    call PlayerTwoRemoveHighlightedCells

    mov al, PlayerTwoSelectedCellColor
    call DrawChessCell
    call DrawChessPiece
    push dx
    mov dh,ChessboardGrid[bx][di]
    call check
    pop dx
;-------------Black Wins-----------;
    cmp WhiteKingPosition[0],bx
    je CheckDi2
    jmp skipp2
    CheckDi2:
    cmp WhiteKingPosition[2],di
    je EndGameBlack
    skipp2:

    call StoreCoolDown

    endBMovePiece:
    pop dx
    pop si
    pop bp
    pop di
    pop bx
    pop ax

ret
    EndGameBlack:
    mov KingDied,1
    mov LeaveGameFlag,1
    mov dx, offset PressAnyKey
    call UpdateStatusBar
    mov dx,0153h
    mov ah,02h
    int 10h
    mov dx, offset BlackWon
    call displaystring
jmp endBMovePiece
BMovePiece endp


PlayerOneStoreHighlightedIndex proc
    push bx
    push di

    add bx,di
    mov di, PlayerOneHighlightedCellsIndex
    mov PlayerOneHighlightedCells[di], bx
    add di,2
    mov PlayerOneHighlightedCellsIndex, di

    pop di
    pop bx
ret
PlayerOneStoreHighlightedIndex endp

PlayerOneRemoveHighlightedCells proc
    push bx
    push di
    push si
    push cx
    push dx

    mov di, 0
    mov si, 0h
    cmp PlayerOneHighlightedCellsIndex, 0h
    je endPlayerOneRemoveHighlightedCells

    PlayerOneRemoveLoop:
        mov bx, PlayerOneHighlightedCells[si]
        call GetBXDIFromBX
        call GetCXDXFromBXDI
        call CheckNormalCellColor
        call DrawChessCell
        call DrawChessPiece
        add si,2
        cmp si, PlayerOneHighlightedCellsIndex
        je endPlayerOneRemoveHighlightedCells
    jmp PlayerOneRemoveLoop

    endPlayerOneRemoveHighlightedCells:

    mov PlayerOneHighlightedCellsIndex, 0h
    pop dx
    pop cx
    pop si
    pop di
    pop bx
ret
PlayerOneRemoveHighlightedCells endp

GetBXDIFromBX proc
    push ax
    push cx
    push dx

    mov dx, 0
    mov ax, bx
    mov cx, 8d
    div cx
    mov bx,dx
    mov dx,00h
    mul cx
    mov di, ax

    pop dx
    pop cx
    pop ax
ret
GetBXDIFromBX endp

;-------------------------------------------------------------
PlayerTwoStoreHighlightedIndex proc
    push bx
    push di

    add bx,di
    mov di, PlayerTwoHighlightedCellsIndex
    mov PlayerTwoHighlightedCells[di], bx
    add di,2
    mov PlayerTwoHighlightedCellsIndex, di

    pop di
    pop bx
ret
PlayerTwoStoreHighlightedIndex endp

PlayerTwoRemoveHighlightedCells proc
    push bx
    push di
    push si
    push cx
    push dx

    mov di, 0
    mov si, 0h
    cmp PlayerTwoHighlightedCellsIndex, 0h
    je endPlayerTwoRemoveHighlightedCells

    PlayerTwoRemoveLoop:
        mov bx, PlayerTwoHighlightedCells[si]
        call GetBXDIFromBX
        call GetCXDXFromBXDI
        call CheckNormalCellColor
        call DrawChessCell
        call DrawChessPiece
        add si,2
        cmp si, PlayerTwoHighlightedCellsIndex
        je endPlayerTwoRemoveHighlightedCells
    jmp PlayerTwoRemoveLoop

    endPlayerTwoRemoveHighlightedCells:

    mov PlayerTwoHighlightedCellsIndex, 0h
    pop dx
    pop cx
    pop si
    pop di
    pop bx
ret
PlayerTwoRemoveHighlightedCells endp

;-------------------------------------------------------------
GetTime proc
    push ax 
    push cx

    mov ah,2ch    ; getting system time (seconds in dh)
    int 21h

    cmp dh,3d
    ja EndGetTime
    add dh,59d
    EndGetTime:

    pop cx
    pop ax
ret

GetTime endp

CheckIfJailExists proc

    push cx
    push dx
    push bx
    push di
    push ax

    call GetBXDIFromBX
    call GetCXDXFromBXDI
    inc cx
    call CheckCellColor
    cmp al, 00h
    jne NoJail
    stc
    jmp endCheckIfJailExists
    Nojail:
    clc

    endCheckIfJailExists:
    pop ax
    pop di
    pop bx
    pop dx
    pop cx

ret
CheckIfJailExists endp

CheckTime proc

    push si
    push bx
    push cx
    push dx
    push ax
    push di
    push bp

    xor bx,bx      ; reset bx



    call GetTime

    CheckTimeLoop:
        push dx
        cmp LockedCellPosition[bx], 0ffh
        je ContinueCheckTimeTemp
        sub dh, LockedCellTime[bx]
        cmp dh,3d
        jb StillLocked
        ;Unlock piece 
        mov QuarterJailDrawnFlag[bx],0
        mov si,bx
        call GetBXDIFromBX
        call GetCXDXFromBXDI
        call CheckNormalCellColor
        call DrawChessCell
        call DrawChessPiece
        mov bx, si
        mov LockedCellPosition[bx],0ffh
        mov LockedCellTime[bx],88d
        jmp ContinueCheckTime

        StillLocked:

        cmp dh,1d
        jb BelowOneSec
        cmp dh,2d
        jb BelowTwoSec

        call CheckIfJailExists
        jnc Bypass1
        cmp QuarterJailDrawnFlag[bx], 1
        je ContinueCheckTimeTemp
        ByPass1:
        mov HalfJailDrawnFlag[bx],0
        mov si,bx
        call GetBXDIFromBX
        call GetCXDXFromBXDI

        call DrawHalfJail
        call DrawFullJail
        call DrawQuarterJail
        mov bx, si
        mov QuarterJailDrawnFlag[bx],1
        jmp ContinueCheckTime

        CheckTimeLoopTemp:jmp CheckTimeLoop             ;Kobry
        EndcheckTimeTemp:jmp EndcheckTime               ;Kobry
        ContinueCheckTimeTemp:jmp ContinueCheckTime     ;Kobry

        BelowTwoSec:
        call CheckIfJailExists
        jnc Bypass2
        cmp HalfJailDrawnFlag[bx], 1
        je ContinueCheckTime
        Bypass2:
        mov FullJailDrawnFlag[bx],0
        mov si, bx
        call GetBXDIFromBX
        call GetCXDXFromBXDI

        call DrawFullJail
        call DrawHalfJail
        mov bx, si
        mov HalfJailDrawnFlag[bx], 1
        jmp ContinueCheckTime

        BelowOneSec:
        call CheckIfJailExists
        jnc Bypass3
        cmp FullJailDrawnFlag[bx], 1
        je ContinueCheckTime
        Bypass3:
        mov si,bx
        call GetBXDIFromBX
        call GetCXDXFromBXDI
        call DrawFullJail
        mov bx,si
        mov FullJailDrawnFlag[bx], 1

        ContinueCheckTime:
        pop dx
        inc bx
        cmp bx, 63d
    jne CheckTimeLoopTemp
EndcheckTime:
    pop bp
    pop di
    pop ax
    pop dx
    pop cx
    pop bx
    pop si
ret
CheckTime endp



StoreCoolDown proc
    push ax
    push bx
    push di
    push dx

        call GetTime
        mov al, ChessboardGrid[bx][di]            ;Now al contains the chess piece
        add bx,di
        mov LockedCellPosition[bx],1              ;Locking this cell position
        and al,0f0h                                ;Checking if piece is black
        cmp al,00h
        je blackpiece

        cmp HasBonus,1
        je AddBonus
        jmp endstore
       
        blackpiece:
        cmp HasBonus,2                              ;check if Black has powerup
        je AddBonus
        jmp endstore

        AddBonus:
        dec dh                            ;Reduce time in seconds with 1 seconds


    endstore:
    mov LockedCellTime[bx],dh
    pop dx
    pop di
    pop bx
    pop ax
ret
StoreCoolDown endp

CheckIfLockedPiece proc
    push bx
    push di
    push cx

    add bx,di
    cmp LockedCellPosition[bx],1
    je IsLockedPosition
    clc
    jmp endCheckIfLockedPiece

    IsLockedPosition:
    stc
    endCheckIfLockedPiece:

    pop cx
    pop di
    pop bx
ret
CheckIfLockedPiece endp

CheckIfWhiteLockedPiece proc
    push bx
    push di
    push cx

    add bx,di
    cmp LockedCellPosition[bx],1
    je WIsLockedPosition
    clc
    jmp endCheckIfLockedPiece

    WIsLockedPosition:
    mov ah, ChessboardGrid[bx]
    and ah, 0f0h
    cmp ah, 00h
    je endCheckIfWLockedPiece
    stc

    endCheckIfWLockedPiece:

    pop cx
    pop di
    pop bx
ret
CheckIfWhiteLockedPiece endp

CheckIfBlackLockedPiece proc
    push bx
    push di
    push cx

    add bx,di
    cmp LockedCellPosition[bx],1
    je BIsLockedPosition
    clc
    jmp endCheckIfBLockedPiece

    BIsLockedPosition:
    mov ah, ChessboardGrid[bx]
    and ah, 0f0h
    cmp ah, 10h
    je endCheckIfBLockedPiece
    stc

    endCheckIfBLockedPiece:

    pop cx
    pop di
    pop bx
ret
CheckIfBlackLockedPiece endp
;-------------------------------------------------------------
GenerateRandomCellNumber proc
    push ax
    push dx
    push cx

    mov ah,0h                           ;Gets System time
    int 1ah                             ;Gets number of clock ticks after midnight

    mov ax,dx                           ;We will divide it by 64 and get the remainder as out random number
    mov dx,00H
    mov cx,64d
    div cx

    mov RandomCellNumber, Dx

    pop cx
    pop dx
    pop ax
ret
GenerateRandomCellNumber endp

GenerateRandomNumber proc
    push ax
    push dx
    push cx

    mov ah,0h                           ;Gets System time
    int 1ah                             ;Gets number of clicks after midnight

    mov ax,dx                           ;We will divide it by Probability of generation and get the remainder as out random number
    mov dx,00H
    mov cx, ProbabilityOfGeneration
    div cx

    mov RandomNumber, DX

    pop cx
    pop dx
    pop ax
ret
GenerateRandomNumber endp

;=============================================================
GenerateBonusItem proc
    push cx
    push dx
    push bx
    push di
    push ax

    cmp iSPowerupGenerated,1                        ;To check whether the bonus item is already generated
    je endGenerateBonusItem

    call GenerateRandomNumber                       ;To generate bonus item randomly
    cmp RandomNumber, 6d                            ;Random number chosen by us
    jne endGenerateBonusItem                        ;If not our chosen number end this iteration

    FindEmptyCell:
    call GenerateRandomCellNumber                   ;If equal find an empty cell
    mov bx, RandomCellNumber
    cmp ChessboardGrid[bx], 099h
    jne FindEmptyCell
    mov ChessboardGrid[bx], 0bbh                    ;If empty cell found move the powerup there

    call GetBXDIFromBX                              ;Get row and column from the cellnum
    call GetCXDXFromBXDI                            ;Get drawing coordinates from row and column
    call DrawChessPiece                             ;Draw updated cell
    mov iSPowerupGenerated, 1                       ;In order not to generate more powerups

    endGenerateBonusItem:
    pop ax
    pop di
    pop bx
    pop dx
    pop cx
ret
GenerateBonusItem endp
;=============================================================
ExecuteBatchMode proc

    cmp BlackORWhite, 1
    je WhiteBatchMode

    call BExecuteBatchMode
    jmp endExecuteBatchMode

    WhiteBatchMode:
    call WExecuteBatchMode

    endExecuteBatchMode:
ret
ExecuteBatchMode endp

WExecuteBatchMode proc
    push ax
    push cx
    push si
    push bx
    push di


    cmp BatchModeFlag, 1                       ;Checks if the user is still editting the string
    je endWExecuteBatchModeTemp                 ;If he is then end batchmode

    cmp BatchModeSize, 0                       ;Checks if there is input in the batchmode string
    je endWExecuteBatchModeTemp                 ;If not then jmp to end

    cmp BatchModeIndex, 0                      ;Checks if this is the first command               
    je WFirstCommand                            ;If it is then jump to the other check

    mov cx, 04h                          
    mov ax, BatchModeIndex                     ;Checks if the index is divisble by four to know whether we are in the middle of a command or a new one                                  ;If divisble by four then it is a new command if not then we are in the middle of one
    div cl
    cmp ah,0
    jne WContinueChecking
    je WNewcommand

    WFirstCommand:                       
    mov ch,00H
    mov ax, BatchModeSize                      ;Checks if the entire batchmode input is divisible by 4 to make sure it is complete
    mov cl,4
    div cl
    cmp ah,0
    jne WResetBatchStringTemp                   ;if not divisible by four then reset the string as it is an invalid command

    ;===========After Check===========;
    WNewcommand:
    mov si, BatchModeIndex

    mov bl, BatchModeString[si]
    inc si
    sub bl, 31h                                 ;30 To change from ascii to decimal then 1 because we start from index zero
    mov al, bl
    mov cl, 8
    mul cl
    mov di, ax                                  ;Transformed the row number to di
    mov BatchModeDI, di

    mov bl, BatchModeString[si]
    inc si
    sub bl,31h
    mov bh,00H                                  ;Transformed the column number to bx
    mov BatchModeBX, bx

    call checkBorders                           ;Checks whether the user input is in the grid
    cmp OutsideBorders, 1
    je WNotCorrectStartPositionTemp              ;If not then remove a random piece and go to the next command

    mov ah, ChessboardGrid[bx][di]
    and ah, 0f0h
    cmp ah, 10h                                 ;Check if White Piece
    jne WNotCorrectStartPositionTemp

    mov BatchModeIndex, si

    WContinueChecking:                           ;This is to check whether the current position is a locked piece
    mov bx, BatchModeBX
    mov di, BatchModeDI
    mov si, BatchModeIndex
    call CheckIfLockedPiece
    jc endWExecuteBatchModeTemp                  ;If it is then end this iteration of batchmode
    jmp WBridge

    endWExecuteBatchModeTemp: jmp endWExecuteBatchMode              ;kobry
    WResetBatchStringTemp: jmp WResetBatchString                    ;kobry
    WNotCorrectStartPositionTemp: jmp WNotCorrectStartPosition      ;kobry

    WBridge:
    call SelectPieceWhite                       ;If it is not then select it
    mov bl, BatchModeString[si]                 ;Gets the row and column of the cell
    inc si
    sub bl, 31h                                 ;30 To change from ascii to decimal then 1 because we start from index zero
    mov al, bl
    mov cl, 8
    mul cl
    mov di, ax                                  ;Transformed the row number to di

    mov bl, BatchModeString[si]
    inc si
    sub bl, 31h
    mov bh,00H                                  ;Transformed the column number to bx

    call checkBorders                           ;Checks whether the user has entered a correct position
    cmp OutsideBorders, 1
    je WNotCorrectEndPosition                    ;If not then remove the current piece and remove the highlight
    call GetCXDXFromBXDI                        ;Gets drawing coordinates
    call CheckCellColor                         ;Checks if this this a possible poistion for the piece to go to
    cmp al, PlayerOneHighlightedCellColor
    jne WNotCorrectEndPosition                   ;If not then remove the current piece then the highlight
    call WMovePiece                             ;If it is then move it

    mov BatchModeIndex, si
    cmp si, BatchModeSize                       ;Checks whether we reached the end of the command
    je WResetBatchString                         ;If we did then we reset it
    jmp endWExecuteBatchMode                     ;If not then we go to the next iteration

    WNotCorrectEndPosition:
    mov bx, PlayerOneSelectedPiecePosition[0]   ;Gets the inital position of the piece
    mov di, PlayerOneSelectedPiecePosition[2]
    mov dl, ChessboardGrid[bx][di]
    call WhatPieceDied
    mov ChessboardGrid[bx][di], 099h            ;Moves an empty cell to it
    call GetCXDXFromBXDI                        ;Gets drawing coordinates
    call CheckNormalCellColor                   ;Checks the normal cell color
    call DrawChessCell                          
    call PlayerOneRemoveHighlightedCells        ;Removes highlights

    cmp si, BatchModeSize                       ;Checks if this is the end of the string
    je WResetBatchString                         ;If it is then reset it

    jmp endWExecuteBatchMode                     ;If not then end this iteration

    WNotCorrectStartPosition:                
    call RemoveRandomWhitePiece                 
    add si,2                                    ;Goes to the next command
    cmp si, BatchModeSize
    je WResetBatchString
    mov BatchModeIndex, si                      ;Stores the index
    jmp endWExecuteBatchMode                     ;Ends this iteration of batch mode

    WResetBatchString:
    mov BatchModeSize, 0
    mov BatchModeIndex, 0
    jmp endWExecuteBatchMode

    endWExecuteBatchMode:
    pop di
    pop bx
    pop si
    pop cx
    pop ax
ret
WExecuteBatchMode endp

BExecuteBatchMode proc
    push bp
    push ax
    push cx
    push si
    push bx
    push di

    mov bx, bp
    mov di, si

    cmp BatchModeFlag, 1                       ;Checks if the user is still editting the string
    je endBExecuteBatchModeTemp                 ;If he is then end batchmode

    cmp BatchModeSize, 0                       ;Checks if there is input in the batchmode string
    je endBExecuteBatchModeTemp                 ;If not then jmp to end

    cmp BatchModeIndex, 0                      ;Checks if this is the first command               
    je BFirstCommand                            ;If it is then jump to the other check

    mov cx, 04h                          
    mov ax, BatchModeIndex                     ;Checks if the index is divisble by four to know whether we are in the middle of a command or a new one                                  ;If divisble by four then it is a new command if not then we are in the middle of one
    div cl
    cmp ah,0
    jne BContinueChecking
    je BNewcommand

    BFirstCommand:                       
    mov ch,00H
    mov ax, BatchModeSize                      ;Checks if the entire batchmode input is divisible by 4 to make sure it is complete
    mov cl,4
    div cl
    cmp ah,0
    jne BResetBatchStringTemp                   ;if not divisible by four then reset the string as it is an invalid command

    ;===========After Check===========;
    BNewcommand:
    mov si, BatchModeIndex

    mov bl, BatchModeString[si]
    inc si
    sub bl, 31h                                 ;30 To change from ascii to decimal then 1 because we start from index zero
    mov al, bl
    mov cl, 8
    mul cl
    mov di, ax                                  ;Transformed the row number to di
    mov BatchModeDI, di

    mov bl, BatchModeString[si]
    inc si
    sub bl,31h
    mov bh,00H                                  ;Transformed the column number to bx
    mov BatchModeBX, bx

    call checkBorders                           ;Checks whether the user input is in the grid
    cmp OutsideBorders, 1
    je BNotCorrectStartPositionTemp              ;If not then remove a random piece and go to the next command

    mov ah, ChessboardGrid[bx][di]
    and ah, 0f0h
    cmp ah, 00h                                 ;Check if Black Piece
    jne BNotCorrectStartPositionTemp

    mov BatchModeIndex, si

    BContinueChecking:                           ;This is to check whether the current position is a locked piece
    mov bx, BatchModeBX
    mov di, BatchModeDI
    mov si, BatchModeIndex
    call CheckIfLockedPiece
    jc endBExecuteBatchModeTemp                  ;If it is then end this iteration of batchmode
    jmp BBridge

    endBExecuteBatchModeTemp: jmp endBExecuteBatchMode              ;kobry
    BResetBatchStringTemp: jmp BResetBatchString                    ;kobry
    BNotCorrectStartPositionTemp: jmp BNotCorrectStartPosition      ;kobry

    BBridge:
    mov bp, bx
    push si                                     ;To keep the value of SI
    mov si, di
    call SelectPieceBlack                       ;If it is not then select it
    pop si
    mov bl, BatchModeString[si]                 ;Gets the row and column of the cell
    inc si
    sub bl, 31h                                 ;30 To change from ascii to decimal then 1 because we start from index zero
    mov al, bl
    mov cl, 8
    mul cl
    mov di, ax                                  ;Transformed the row number to di

    mov bl, BatchModeString[si]
    inc si
    sub bl, 31h
    mov bh,00H                                  ;Transformed the column number to bx

    call checkBorders                           ;Checks whether the user has entered a correct position
    cmp OutsideBorders, 1
    je BNotCorrectEndPosition                    ;If not then remove the current piece and remove the highlight
    call GetCXDXFromBXDI                        ;Gets drawing coordinates
    call CheckCellColor                         ;Checks if this this a possible poistion for the piece to go to
    cmp al, PlayerTwoHighlightedCellColor
    jne BNotCorrectEndPosition                   ;If not then remove the current piece then the highlight
    call BMovePiece                             ;If it is then move it

    mov BatchModeIndex, si
    cmp si, BatchModeSize                       ;Checks whether we reached the end of the command
    je BResetBatchString                         ;If we did then we reset it
    jmp endBExecuteBatchMode                     ;If not then we go to the next iteration

    BNotCorrectEndPosition:
    mov bx, PlayerTwoSelectedPiecePosition[0]   ;Gets the inital position of the piece
    mov di, PlayerTwoSelectedPiecePosition[2]
    mov dl, ChessboardGrid[bx][di]
    call WhatPieceDied
    mov ChessboardGrid[bx][di], 099h            ;Moves an empty cell to it
    call GetCXDXFromBXDI                        ;Gets drawing coordinates
    call CheckNormalCellColor                   ;Checks the normal cell color
    call DrawChessCell                          
    call PlayerTwoRemoveHighlightedCells        ;Removes highlights

    cmp si, BatchModeSize                       ;Checks if this is the end of the string
    je BResetBatchString                         ;If it is then reset it

    jmp endBExecuteBatchMode                     ;If not then end this iteration

    BNotCorrectStartPosition:                
    call RemoveRandomBlackPiece                 
    add si,2                                    ;Goes to the next command
    cmp si, BatchModeSize
    je BResetBatchString
    mov BatchModeIndex, si                      ;Stores the index
    jmp endBExecuteBatchMode                     ;Ends this iteration of batch mode

    BResetBatchString:
    mov BatchModeSize, 0
    mov BatchModeIndex, 0
    jmp endBExecuteBatchMode

    endBExecuteBatchMode:
    pop di
    pop bx
    pop si
    pop cx
    pop ax
    pop bp
ret
BExecuteBatchMode endp
;=============================================================
RemoveRandomWhitePiece proc
    push bx
    push ax
    push dx

    GenerateAgain:
    mov bx,00H
    call GenerateRandomCellNumber                       ;We generate a random cell number

    mov bx,RandomCellNumber                             
    mov ah, ChessboardGrid[bx]
    and ah, 0f0h                                        ;We check if the cell contains a whitepiece
    cmp ah, 10H
    jne GenerateAgain                                   ;If not then genereate a random number again
    mov ah, ChessboardGrid[bx]                          
    cmp ah, 16h                                         ;If there is a white piece then check if it is the king
    je GenerateAgain                                    ;If it is then generate a random number again
    mov ChessboardGrid[bx], 099h                        ;Else then move the empty cell to the grid
    mov dl,ah
    call whatpiecedied                                 ;To update the user with the killed piece

    call GetBXDIFromBX                                  ;Get the row and column of the cell
    call GetCXDXFromBXDI                                ;Get drawing coordinates
    call CheckNormalCellColor                           ;Checks the normal cell color
    call DrawChessCell                                  ;Draws over the cell to update it and make it empty

    pop dx
    pop ax
    pop bx
ret
RemoveRandomWhitePiece endp

RemoveRandomBlackPiece proc
    push bx
    push ax
    push dx

    BGenerateAgain:
    mov bx,00H
    call GenerateRandomCellNumber                       ;We generate a random cell number

    mov bx,RandomCellNumber                             
    mov ah, ChessboardGrid[bx]
    and ah, 0f0h                                        ;We check if the cell contains a whitepiece
    cmp ah, 00H
    jne BGenerateAgain                                   ;If not then genereate a random number again
    mov ah, ChessboardGrid[bx]                          
    cmp ah, 06h                                         ;If there is a white piece then check if it is the king
    je BGenerateAgain                                    ;If it is then generate a random number again
    mov ChessboardGrid[bx], 099h                        ;Else then move the empty cell to the grid
    mov dl,ah
    call whatpiecedied                                  ;To update the user with the killed piece

    call GetBXDIFromBX                                  ;Get the row and column of the cell
    call GetCXDXFromBXDI                                ;Get drawing coordinates
    call CheckNormalCellColor                           ;Checks the normal cell color
    call DrawChessCell                                  ;Draws over the cell to update it and make it empty

    pop dx
    pop ax
    pop bx
ret
RemoveRandomBlackPiece endp

;=============================================================
;========================Status Bar Procs=====================

UpdateStatusBar proc         ;Responsible for displaying any message on the status bar
push ax

Update:
cmp StatusBarCurrentRow,35d      ;This means that the status bar is full so we need to clear it first
ja callClearStatusBar
push dx                          ;To save the message
mov dh,StatusBarCurrentRow       ;Moves cursor to the first empty row
mov dl,4Ch
mov ah,02h
int 10h
pop dx                           ;Retrieves the message
mov ah, 9
int 21h
inc StatusBarCurrentRow
jmp endupdate

callClearStatusBar:
call ClearStatusBar

endUpdate:
pop ax
ret
UpdateStatusBar endp

ClearStatusBar proc       ;Clears the satus bar and moves the index to the top again
push dx
    mov dx,offset clear
    mov StatusBarCurrentRow,4
    loop1:
    call UpdateStatusBar
    cmp StatusBarCurrentRow,35d
    ja endclear
    jmp loop1

    endclear:
    mov StatusBarCurrentRow,4
    pop dx
    call UpdateStatusBar
ret
ClearStatusBar endp
;=====================================================

WhatPieceDied proc

    cmp dl,01
    je BlackAskary
    cmp dl,02h
    je BlackTabya
    cmp dl,03h
    je BlacKHosan
    cmp dl,04h
    je BlackFil
    cmp dl,05
    je BlackQueen

    cmp dl,11h
    je WhiteAskary
    cmp dl,12h
    je WhiteTabya
    cmp dl,13h
    je WhiteHosan
    cmp dl,14h
    je WhiteFil
    cmp dl,15h
    je WhiteQueen
    jmp endwhatpiece

    BlackAskary:
    mov dx,offset deadBAskary
    call UpdateStatusBar
    jmp endwhatpiece

    WhiteAskary:
    mov dx,offset deadWAskary
    call UpdateStatusBar
    jmp endwhatpiece

    BlackTabya:
    mov dx,offset deadBtabya
    call UpdateStatusBar
    jmp endwhatpiece

    WhiteTabya:
    mov dx,offset deadWtabya
    call UpdateStatusBar
    jmp endwhatpiece

    BlacKHosan:
    mov dx,offset deadBhosan
    call UpdateStatusBar
    jmp endwhatpiece

    WhiteHosan:
    mov dx,offset deadWhosan
    call UpdateStatusBar
    jmp endwhatpiece

    BlackFil:
    mov dx,offset deadBfil
    call UpdateStatusBar
    jmp endwhatpiece

    WhiteFil:
    mov dx,offset deadWfil
    call UpdateStatusBar
    jmp endwhatpiece

    BlackQueen:
    mov dx,offset deadBWazeer
    call UpdateStatusBar
    jmp endwhatpiece

    WhiteQueen:
    mov dx,offset deadWWazeer
    call UpdateStatusBar
    jmp endwhatpiece

endwhatpiece:
ret
WhatPieceDied endp
;==========================================

check proc
    push si
    push di
    push bx
    push dx
    push cx
    
    cmp BlackORWhite,1h
    je WhitePiece1

        ;BlackPiece
            cmp dh, 04h            ;piece in DH
            jne BlackCheck2
            call DrawBFilMoves
            jmp startCheck
            BlackCheck2:
            cmp dh, 02h
            jne BlackCheck3
            call DrawBTabyaMoves
            jmp startCheck
            BlackCheck3:
            cmp dh, 05h
            jne BlackCheck4
            call DrawBQueenMoves
            jmp startCheck
            BlackCheck4:
            cmp dh, 06h
            jne BlackCheck5
            call DrawBKingMoves
            jmp startCheck
            BlackCheck5:
            cmp dh, 01h
            jne BlackCheck6
            call DrawBAskaryMoves
            jmp startCheck
            BlackCheck6:
            cmp dh, 03h
            call DrawBHosanMoves
            jne startCheck

        WhitePiece1:
            cmp dh, 14h
            jne WhiteCheck2
            call DrawWFilMoves
            jmp startCheck
            WhiteCheck2:
            cmp dh, 12h
            jne WhiteCheck3
            call DrawWTabyaMoves
            jmp startCheck
            WhiteCheck3:
            cmp dh, 15h
            jne WhiteCheck4
            call DrawWQueenMoves
            jmp startCheck
            WhiteCheck4:
            cmp dh, 16h
            jne WhiteCheck5
            call DrawWKingMoves
            jmp startCheck
            WhiteCheck5:
            cmp dh, 11h
            jne WhiteCheck6
            call DrawWAskaryMoves
            jmp startCheck
            whiteCheck6:
            cmp dh, 13h
            call DrawWHosanMoves

    startCheck:
    mov si,0h
  
    cmp BlackORWhite,1h
    jne CheckBlack

    checkWhite:
    cmp si,PlayerOneHighlightedCellsIndex
    je StopCheckWhite
    mov bx,PlayerOneHighlightedCells[si]
    call GetBXDIFromBX
    mov dl,ChessboardGrid[bx][di]
    cmp dl,06h                 ;If a king exists in any of the cells in the array
    je BlackCheckLabel
    add si,2h
    jmp checkWhite

    CheckBlack:
    cmp si,PlayerTwoHighlightedCellsIndex
    je StopCheckblack
    mov bx,PlayerTwoHighlightedCells[si]
    call GetBXDIFromBX
    mov dl,ChessboardGrid[bx][di]
    cmp dl,16h
    je WhiteCheckLabel
    add si,2h
    jmp CheckBlack

    WhiteCheckLabel:
    mov dx , offset keshWmalek
    call UpdateStatusBar
    jmp StopCheckWhite

    BlackCheckLabel:
    mov dx , offset keshBmalek
    call UpdateStatusBar
    jmp StopCheckblack

    StopCheckWhite:
    mov PlayerOneHighlightedCellsIndex,0h
    jmp StopCheck
    StopCheckblack:
    mov PlayerTwoHighlightedCellsIndex,0h

    StopCheck:
    pop cx
    pop dx
    pop bx
    pop di
    pop si
ret
check endp

DisplayTime proc

    push ax
    push bx
    push cx
    push dx
    push di


    ;cmp firsttimeclock,0
    ;jne Seconds
    MOV AH,2CH    ; To get System Time
    INT 21H
    ;cmp dh,StartingSecond
    ;mov dh,0d
    ;mov CurrentSecond,dh
    ;sub StartingSecond,dh
    ;mov dh,StartingSecond
    ;mov CurrentSecond,dh
    ;mov StartingSecond,0
    ;mov firsttimeclock,1
    ;Seconds Part
    
     
    cmp firsttimeclock,0
    jne seconds
    mov CurrentSecond,0
    mov firsttimeclock,1
    jmp skipincmin

    
    Seconds:
    cmp dh,StartingSecond
    je skipincmin
    add CurrentSecond,1d
    cmp CurrentSecond,60d
    je incMin
    jmp skipincmin
    incMin:
    inc CurrentMinute
    mov CurrentSecond,0d
    skipincmin:
    mov ch,CurrentSecond
    mov StartingSecond,ch
    mov ah,02h
    mov dx,0260h
    int 10h

    mov al, CurrentMinute
    mov ah, 0h
    mov bl, 10d
    div bl
    push ax
    mov dl, al
    mov ah,2
    add dl,30h
    int 21h
    pop ax
    mov dl,ah
    mov ah,02
    add dl,30h
    int 21h  
   
    mov dl, ':'
    int 21h
    cmp firsttimeclock,0
    jne dontresetseconds
    mov CurrentSecond,00d
dontresetseconds:
    mov al, CurrentSecond
    mov ah, 0h
    mov bl, 10d
    div bl
    push ax
    mov dl, al
    mov ah,2
    add dl,30h
    int 21h
    pop ax
    mov dl,ah
    mov ah,02
    add dl,30h
    int 21h

pop di
pop dx
pop cx
pop bx
pop ax
ret 
DisplayTime endp

;====================For Phase 2=======================
UpdateNotificationBar proc         ;Responsible for displaying any message on the status bar
push ax

UpdateNotification:
cmp NotificationBarCurrentRow,4d      ;This means that the status bar is full so we need to clear it first
ja callClearNotificationBar
push dx                          ;To save the message
mov dh,NotificationBarCurrentRow       ;Moves cursor to the first empty row
mov dl,00h
mov ah,02h
int 10h
pop dx                           ;Retrieves the message
mov ah, 9
int 21h
inc NotificationBarCurrentRow
jmp endupdatenotification

callClearNotificationBar:
call ClearNotificationBar

endUpdatenotification:
pop ax
ret
UpdateNotificationBar endp

ClearNotificationBar proc       ;Clears the satus bar and moves the index to the top again
push dx
    mov dx,offset clear1
    mov NotificationBarCurrentRow,0
    loop11:
    call UpdateNotificationBar
    cmp StatusBarCurrentRow,4d
    ja endclear1
    jmp loop11

    endclear1:
    mov NotificationBarCurrentRow,0
    pop dx
    call UpdateNotificationBar
ret
ClearNotificationBar endp

SendInvitation proc
    mov dx , 3FDH		; Line Status Register
    AGAIN:  
            In al , dx 			;Read Line Status
            AND al , 00100000b
            JZ AGAIN

    ;If empty put the VALUE in Transmit data register
            mov dx , 3F8H		; Transmit data register
            mov al,  ah
            out dx , al 

            cmp al,3ch
            jne EndSendInvitation 
            cmp GameReceived,0
            je EndReceiveInvitation
            mov BlackORWhite,0

            EndSendInvitation:
ret
SendInvitation endp

ReceiveInvitation proc
                            ;Check that Data Ready
        mov dx , 3FDH		; Line Status Register
        in al , dx 
        AND al , 1
        jz EndReceiveInvitation
                            ;If Ready read the VALUE in Receive data register
        mov dx , 03F8H
        in al , dx 
        cmp al,3bh
        je callChattingReceived

        cmp al,3ch
        je callGamingReceived

        EndReceiveInvitation:
ret

callChattingReceived: 
mov ChattingReceived,1
push dx
mov dx, offset ReceivedChatInvitation
call UpdateNotificationBar
pop dx
jmp EndReceiveInvitation

callGamingReceived:
mov GameReceived,1
push dx
mov dx, offset ReceivedGameInvitation
call UpdateNotificationBar
pop dx
jmp EndReceiveInvitation

ReceiveInvitation endp

sendChat Proc
    push dx
    push ax
    push cx

    cmp Ascii, 63d
    jbe endSendChat
    

    ;Sending:
    ;Check that Transmitter Holding Register is Empty
    mov dx , 3FDH		; Line Status Register
    ReadyToSendChat:  
        In al , dx 			;Read Line Status
        AND al , 00100000b
    JZ ReadyToSendChat
    ;If empty put the VALUE in Transmit data register
        mov dx , 3F8H		; Transmit data register
        mov al, Ascii
        out dx , al

        cmp ascii, 92d
        je LeaveGame
    ;moving cursor 
        mov dl,ChatCols
        mov dh,ChatRows      
        mov ah,2
        int 10h 

        cmp Ascii, 91d
        je NewRow
        cmp Ascii,93d
        jne NotSpace
        mov al, 0
        NotSpace:
        mov ah,2 
        mov dl,al
        int 21h
        inc ChatCols
        cmp ChatCols,3ch
        je NewRow
        jne endSendChat

        LeaveGame:
        mov LeaveGameFlag,1
        mov dx, offset PressAnyKey
        call UpdateStatusBar
        jmp endSendChat

        NewRow:
        inc ChatRows
        mov ChatCols,0
        cmp chatrows,48d
        jb endSendChat
        call clearChatBar



    endSendChat:
    pop cx
    pop ax
    pop dx
ret
sendChat endp

ClearChatBar proc       ;Clears the chat bar and moves the index to the top again
push dx
push ax
 
    mov ChatRows,39d
    loop111:
    mov dh,chatrows
    mov dl,ChatCols
    mov ah,2
    int 10h
       mov dx,offset clear1
   call displaystring
   inc chatrows
    cmp chatrows,48d
    ja endclearchat
    jmp loop111

    endclearchat:
    mov ChatRows,39d
    pop ax
    pop dx
    call sendChat
ret
ClearChatBar endp

SendData proc
    push dx
    push ax
    push cx

    cmp IsData, 0           ;Check if there is data to send
    je endSendData

    cmp SendOldOrNew, 0     ;0 If old, 1 if new
    jne NewPos

    mov cl, DataToSend[0]   ;Sending old position
    mov SendOldOrNew, 1     ;Changing flag to send new position
    jmp Sending

    NewPos:             
    mov cl, DataToSend[1]   ;Sending new position
    mov SendOldOrNew, 0     ;Changing flag to send old position next
    mov IsData, 0           ;Moving isdata with zero to indicate there is no data to send

    Sending:
    ;Check that Transmitter Holding Register is Empty
    mov dx , 3FDH		; Line Status Register
    ReadyToSend:  
        In al , dx 			;Read Line Status
        AND al , 00100000b
    JZ ReadyToSend
    ;If empty put the VALUE in Transmit data register
        mov dx , 3F8H		; Transmit data register
        mov al, cl
        out dx , al

    endSendData:
    pop cx
    pop ax
    pop dx 
ret
SendData endp


ReadBData proc
    push dx
    push ax
    push bp
    push si
    push bx
    push di

    ;mov dx , 03F8H              ;Line status register
    ;in al , dx                  ;We read the data (cell number)
    mov bh, 0                   
    mov bl, al
    call GetBXDIFromBX          ;We change the cell number to row and column

    cmp ReadOldOrNew, 0         ;Check if this is and old or new position
    je BOldPos

    mov BlackORWhite, 0
    mov SkipBSend, 1
    call BMovePiece
    mov SkipBSend, 0
    mov BlackORWhite, 1
    mov ReadOldOrNew, 0
    jmp endReadBdata

    BOldPos:
    mov PlayerTwoSelectedPiecePosition[0], bx
    mov PlayerTwoSelectedPiecePosition[2], di
    mov ReadOldOrNew, 1
    mov bp, bx
    mov si, di
    mov BlackORWhite, 0
    call SelectPieceBlack
    mov BlackORWhite, 1
    jmp endReadBdata

    ; mov bx, ax                  ;We moved the old position in bx
    ; mov al, bh                  ;Here we split the position into the two register bx and cx
    ; mov ah,0                    ;CX has the old position while bx has the new one
    ; mov bh,0

    ; xchg ax, bx                 ;We put in bx the old position

    ; call GetBXDIFromBX
    ; mov bp, bx
    ; mov si, di

    ; mov BlackORWhite, 0
    ; call SelectPieceBlack

    ; mov BlackORWhite, 0
    ; xchg bx, ax                ;We put in bx the new one
    ; call GetBXDIFromBX

    ; mov BlackORWhite, 0
    ; mov SkipBSend, 1
    ; call BMovePiece
    ; mov SkipBSend, 0
    ; mov BlackORWhite, 1 

    endReadBdata:
    pop di
    pop bx
    pop si
    pop bp
    pop ax
    pop dx
    ret
ReadBData endp

ReadWData proc
    push dx
    push ax
    push bx
    push di

    ;mov dx , 03F8H              ;Line status register
    ;in al , dx                  ;We read the data (cell number)
    mov bh, 0                   
    mov bl, al
    call GetBXDIFromBX          ;We change the cell number to row and column

    cmp ReadOldOrNew, 0         ;Check if this is and old or new position
    je WOldPos

    mov BlackORWhite, 1
    mov SkipWSend, 1
    call WMovePiece
    mov SkipWSend, 0
    mov BlackORWhite, 0
    mov ReadOldOrNew, 0
    jmp endReadWdata

    WOldPos:
    mov PlayerOneSelectedPiecePosition[0], bx
    mov PlayerOneSelectedPiecePosition[2], di
    mov ReadOldOrNew, 1
    mov BlackORWhite, 1
    call SelectPieceWhite
    mov BlackORWhite, 0
    jmp endReadWdata

    ; push ax
    ; push dx
    ; push cx
    ; mov cl, ah
    ; mov ah,2
    ; mov dl, cl
    ; int 21h
    ; mov dl, al
    ; int 21H
    ; pop cx
    ; pop dx
    ; pop ax

    ; mov bx, ax                  ;We moved the old position in bx
    ; mov al, bh                  ;Here we split the position into the two register bx and cx
    ; mov ah,0                    ;CX has the old position while bx has the new one
    ; mov bh,0

    ; xchg ax, bx                 ;We put in bx the old position

    ; call GetBXDIFromBX

    ; mov BlackORWhite, 1
    ; call SelectPieceWhite

    ; mov BlackORWhite,1 
    ; mov bx, ax               ;We put in bx the new one
    ; call GetBXDIFromBX

    ; mov BlackORWhite, 1
    ; mov SkipWSend, 1
    ; call WMovePiece
    ; mov SkipWSend, 0
    ; mov BlackORWhite, 0 
    endReadWdata:
    pop di
    pop bx
    pop ax
    pop dx
    ret
ReadWData endp

CheckIfDataRecieved proc
    push dx
    push ax

    ;Check that Data Ready
    mov dx , 3FDH		; Line Status Register
    in al , dx 
    AND al , 1          ;Check if there is data and if there isn't return
    jz endCheckIfDataReceived

    mov dx, 03F8H
    in al, dx
    cmp al, 63d
    ja RecieveChatLabel

    cmp BlackORWhite, 1
    je BlackReadData
    jne WhiteReadData

    RecieveChatLabel:
    call RecieveChat
    jmp endCheckIfDataReceived

    BlackReadData:
    call ReadBData
    jmp endCheckIfDataReceived

    WhiteReadData:
    call ReadWdata

    endCheckIfDataReceived:
    pop ax
    pop dx
ret
CheckIfDataRecieved endp

RecieveChat proc

    push dx
    push ax
        mov dl,ChatColr
        mov dh,ChatRowr      
        mov ah,2
        int 10h 

        cmp al, 92d
        je LeaveGame2
        cmp al,91d
        je NewRowR
        cmp al, 93d
        jne NotSpaceR
        mov al,0
        NotSpaceR:
        mov ah,2 
        mov dl,al
        int 21h
        inc ChatColr
        cmp ChatColr,7bh
        je NewRowR
        jne endReceiveChat

        LeaveGame2:
        mov LeaveGameFlag,1
        mov dx, offset PressAnyKey
        call UpdateStatusBar
        jmp endReceiveChat

        NewRowR:
        inc ChatRowr
        mov Chatcolr,3fh
        cmp ChatRowr,48d
        jb endReceiveChat
        call ClearRChatBar


        endReceiveChat:
    pop ax
    pop dx
ret
RecieveChat endp

ClearRChatBar proc       ;Clears the chat bar and moves the index to the top again
push dx
push ax
  
    mov ChatRowr,39d
    mov chatcolr,3fh
    loop1111:
    mov dh,chatrowr
    mov dl,ChatColr
    mov ah,2
    int 10h
    mov dx,offset clear1
    
    call displaystring
    inc chatrowr
    cmp chatrowr,48d
    ja endclearrchat
    jmp loop1111

    endclearrchat:
    mov ChatRowr,39d
    mov chatcolr,3fh
    mov dh,chatrowr ;i think this mafar2etsh f ay haga
    mov dl,ChatColr
    mov ah,2
    int 10h
   
    pop ax
    pop dx
    call recieveChat
ret
ClearRChatBar endp

ResetChessBoard proc
    push si
    push ax
    mov si,0

    ResetLoop:
    mov al, DefaultChessboardGrid[si]
    mov ChessboardGrid[si], al
    inc si
    cmp si,64d
    jne ResetLoop

    pop ax
    pop si
ret
ResetChessBoard endp


displaystring proc
    mov ah,9h
    int 21h
    ret
displaystring  endp
;                 scrolling                       ;
;-------------------------------------------------;

save_firstline PROC
    push ds
    mov ax, ds
    mov es, ax
    lea di, firstline
    mov ax, 0b800h
    mov ds, ax
    mov ax, 0
    mov si, ax
    mov cx, 80
    rep movsw
    pop ds
    ret
save_firstline ENDP
restore_firstline PROC
    lea si, firstline
    mov ax, 0b800h
    mov es, ax
    mov ax, 0
    mov di, ax
    mov cx, 80
    rep movsw
    ret
restore_firstline ENDP
scroll_Up PROC
    mov ah, 6               
    mov al, 1               
    mov bh, 0               
    mov ch, 0               
    mov cl, 0               
    mov dh, 25              
    mov dl, 80              
    int 10h
    call restore_firstline
    ret
scroll_Up ENDP
scroll_Down PROC
    mov ah, 7               
    mov al, 1               
    mov bh, 0               
    mov ch, 0               
    mov cl, 0               
    mov dh, 25              
    mov dl, 80              
    int 10h
    call restore_firstline
    ret
scroll_Down ENDP
EndGameWhiteWins:
mov dx,0153h
mov ah,02h
int 10h
mov dx, offset WhiteWon
call displaystring
jmp PlayerWon
;;-----------------------------
EndGameBlackWins:
mov dx,0153h
mov ah,02h
int 10h
mov dx, offset BlackWon
call displaystring


PlayerWon:
mov dx, offset PressAnyKey
call UpdateStatusBar
MOV AH,0H     
INT 16H
;jmp endscreen
endGame:
;;-----------------------------
end main