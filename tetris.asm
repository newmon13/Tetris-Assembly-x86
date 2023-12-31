.model small
.stack 100h

drawShape MACRO pattern, brickColor
Local drawLoop
    push cx
    push dx
    push bx
    push si
    push di

    mov bx, offset pattern
    mov cx, [bx] ;load the number of squares
    add bx, 2    ;skipping first dw
	
    ;loop that will draw every single square
    drawLoop:
        push cx              ; save CX
        mov cx, [bx]         ; load X start (x1)
		add cx, 40			 ; start x of piece
        mov dx, [bx + 2]     ; load Y start (y1)
		add dx, 0			 ; start y of piece 
        mov si, [bx + 4]     ; load X end (x2)
		add si, 40			 ;
        mov di, [bx + 6]     ; load Y end (y2)
		add di, 0
        drawRect cx,dx,si,di,brickColor      ; draw the square
        add bx, 8            ; move to the next square coordinates
        pop cx               ; restore CX
        loop drawLoop

    pop di
    pop si
    pop bx
    pop dx
    pop cx
ENDM

drawRect MACRO x1, y1, x2, y2, brickColor
	Local rectXLoop
	Local rectYLoop
    push cx
    push dx
    push bx
    push si
    push di

    mov cx, x1 ; X start position
    mov dx, y1 ; Y start position
    mov bx, x2   ; X end position
    mov si, y2   ; Y end position
    mov ah, 0Ch		; BIOS function to set pixel

    rectYLoop:
        push cx
        push dx
        rectXLoop:
            mov al, brickColor
            int 10h
            inc cx		; next column
            cmp cx, bx	; check if end column reached
            jl rectXLoop

        pop dx
        pop cx
        inc dx		; Next row
        cmp dx, si	; Check if end row reached
        jl rectYLoop

    pop di
    pop si
    pop bx
    pop dx
    pop cx
ENDM


drawHorizontalLine MACRO y, x1, x2
LOCAL drawLoop

    mov ax, 0A000h ; segment address for video memory in mode 13h
    mov es, ax     ; set extra segment to video memory segment

    ; calculating offset (y * 320) + x1
    mov ax, y
    mov bx, 320
    mul bx
    add ax, x1
    mov di, ax

    mov al, 0Fh    ; for now only white line

    ;number of iterations
    mov cx, x2
    sub cx, x1
    inc cx ;last pixel

    drawLoop:
        mov es:[di], al ;saving color to the specyfic position in memory
        inc di; moving to the next  pixel
        loop drawLoop   ;dec cx

ENDM

drawVerticalLine MACRO x, y1, y2
LOCAL drawLoop

	mov ax, 0A000h
    mov es, ax


    ;offset = (y1 * 320) + x
    mov ax, y1
    mov bx, 320
    mul bx
    add ax, x
    mov di, ax


    mov al, 0Fh

    mov cx, y2
    sub cx, y1
    inc cx


    drawLoop:
        mov es:[di], al
        add di, 320
        loop drawLoop  
ENDM



writeText MACRO

	mov si,@data	;moves to si the location in memory of the data segment
	mov ah,13h	;service to print string in graphic mode
	mov al,0	;sub-service 0 all the characters will be in the same color(bl) and cursor position is not updated after the string is written
	mov bh,0	;page number=always zero
	mov bl,0Fh	;color of the text (white foreground and black background)
	
	;     0000             1111
	;|_ Background _| |_ Foreground _|
	;

	mov cx,5;length of string
	;resoultion of the screen is 320x200
	mov dh,63;y coordinate
	mov dl,170;x coordinate
	mov es,si;moves to es the location in memory of the data segment
	mov bp,offset message;mov bp the offset of the string
	int 10h

ENDM


; Macro for checking boundary collisions
; Assumes currentFallingShape is an array of word-sized coordinates (x, y)
; Assumes game boundaries are fixed

CHECK_COLLISION MACRO
    LOCAL noCollision
	LOCAL checkLoop
	LOCAL collision
	
	push ax
	push bx
	push cx
	
	mov eax, 10h


	
    ; Load the base address of the current falling shape
    mov bx, OFFSET currentFallingShape
    mov cx, bx
	add bx,2; skipping first value 

    ; Assuming 4 squares per shape, each represented by 2 words (x, y)

    checkLoop:
        ; Load coordinates of a square (x, y)
        mov ax, [bx]     ; x coordinate
        mov dx, [bx+2]   ; y coordinate

        ; Check if the square is outside the game boundaries
        cmp ax, 10   ; Check left boundary
        jle collision
        cmp ax, 190  ; Check right boundary
        jge collision
        cmp dx, 100 ; Check bottom boundary
        jge collision

        ; Move to the next square
        add bx, 8 ; Move to the next set of coordinates
        loop checkLoop
        jmp noCollision

    collision:
        ; Collision detected, set a flag or handle it accordingly
        ; For example, set AX to 1 to indicate collision
        mov ax, 1
        jmp done

    noCollision:
        ; No collision, set AX to 0
        mov ax, 0

    done:
		pop cx
		pop bx
		pop ax
    ENDM

.data
color dw 0Ch
TIME_AUX db 0

blockHorizontalShape4	dw 4
					dw 10, 10, 20, 20   ; First square coordinates (x1, y1, x2, y2)
					dw 20, 10, 30, 20   ; Second square
					dw 30, 10, 40, 20   ; Third square
					dw 40, 10, 50, 20   ; Fourth square
					
					
					
blockVerticalShape4	dw 4
					dw 10, 10, 20, 20   ; First square coordinates (x1, y1, x2, y2)
					dw 10, 20, 20, 30   ; Second square
					dw 10, 30, 20, 40   ; Third square
					dw 10, 40, 20, 50   ; Fourth square
					
					
					
block4 dw 					

blockSnakeShape	    dw 4
					dw 20, 10, 30, 20   ; Third square
					dw 10, 20, 20, 30   ; First square coordinates (x1, y1, x2, y2)
					dw 10, 10, 20, 20   ; Second square
					dw 20, 0, 30, 10	; Fourth square
					
blockReversedTShape  dw 4                    ; Number of squares in the shape
					 dw 10, 0, 20, 10         ; First square coordinates (x1, y1, x2, y2)
					 dw 10, 10, 20, 20        ; Second square
					 dw 0, 10, 10, 20         ; Third square
					 dw 20, 10, 30, 20        ; Fourth square
				  
				  
blockTShape        dw 4                    ; Number of squares in the shape
                   dw 10, 10, 20, 30         ; First square coordinates (x1, y1, x2, y2)
                   dw 10, 10, 20, 20        ; Second square
                   dw 0, 10, 10, 20         ; Third square
                   dw 20, 10, 30, 20 

				  
currentFallingShape dw 9 dup(?)

videoBuffer DB 64000 DUP(?)
message db 'Hello', '$'




.code

;------CONSTANTS--------

BRICK_VELOCITY equ 1
BRICK_H_MOVE equ BRICK_VELOCITY * 10

;------CONSTANTS--------
main proc
    mov ax, @data
    mov ds, ax

    ; Set VGA mode 13h
    mov ax, 13h
    int 10h
	
	drawShape blockVerticalShape4, 01h
;----------------------------------	
	mov cx, 17              ; Number of words to copy
    lea si, blockVerticalShape4     ; Source index to start of blockTShape
    lea di, currentFallingShape ; Destination index to start of currentFallingShape
	
	 copyLoop:
        mov ax, [si]       ; Load a word from blockTShape
        mov [di], ax       ; Store the word in currentFallingShape
        add si, 2          ; Move to the next word in blockTShape
        add di, 2          ; Move to the next word in currentFallingShape
        loop copyLoop      ; Decrement cx and repeat if cx is not zero



	;test rotate vertical -> horizontal
	;writeText
	call drawBoard

;----------------------------------
	checkTime:
		mov ah, 2ch ; get system time ch = hou, cl = minute, dh = second, dl = 1/100s
		int 21h
		cmp dl, TIME_AUX
		je checkTime
		;if different move brick
		mov TIME_AUX, dl
;-----------------------------
	drawShape currentFallingShape 00h
	call updateFallingShape
	call moveFallingShapeH
	drawShape currentFallingShape 01h

;-----------------------------
		jmp checkTime
	
    ; Keep the screen visible
    jmp $

    ; Return to text mode and exit
    mov ax, 4C00h
    int 21h

main endp



updateFallingShape proc near
    push ax
    push bx
    push cx
    push si

    lea si, currentFallingShape
    mov cx, [si] ; Load the number of squares
    add si, 2    ; Skip the count word

    updateLoop:
        add [si+2], BRICK_VELOCITY ; Increment Y1 (second word of the coordinate pair)
        add [si+6], BRICK_VELOCITY ; Increment Y2 (fourth word of the coordinate pair)
        add si, 8      ; Move to the next square coordinates
    loop updateLoop
    pop si
    pop cx
    pop bx
    pop ax
    ret
updateFallingShape endp
drawBoard proc near


	;frame for game
	drawVerticalLine 10, 10, 190
	drawHorizontalLine 190, 10, 100
	drawVerticalLine 100, 10, 190
	
	;display for next shape
	
	drawHorizontalLine 10, 150, 200
	drawVerticalLine 150, 10, 50
	drawHorizontalLine 50, 150, 200
	drawVerticalLine 200, 10, 50
	
	ret

drawBoard endp
moveFallingShapeH proc near

    push ax
    push bx
    push cx
    push si

    lea si, currentFallingShape ; Load address of currentFallingShape
    mov cx, [si]                ; Load the number of squares
    add si, 2                   ; Skip the count word

    ; Check for a key press
    mov ah, 01h
    int 16h
    jz NoKeyPress               ; Jump if no key is pressed

    ; Get the key press
    mov ah, 00h
    int 16h
	
	cmp al, 61h ;a
	JE moveLeft

    cmp al, 64h ;d                 ; Compare with 'd'
    je moveRight              ; Jump if not 'd'

             ; Continue loop if BX is not zero
			 
	cmp al, 72h ;r - rotate
	je rotate

NoKeyPress:
    pop si
    pop cx
    pop bx
    pop ax
    ret
	

moveRight:
cmp ax, 1
je NoKeyPress
    updateLoopR:
        add [si], BRICK_H_MOVE ; Increment x1 (second word of the coordinate pair)
        add [si+4], BRICK_H_MOVE ; Increment x2 (fourth word of the coordinate pair)
        add si, 8      ; Move to the next square coordinates
    loop updateLoopR
	jmp NoKeyPress
	
moveLeft:
CHECK_COLLISION
cmp ax, 1
je NoKeyPress
mov bx, BRICK_H_MOVE
    updateLoopL:
	
        sub [si], bx; Subtract x1 (second word of the coordinate pair)
        sub [si+4], bx ; Subtract x2 (fourth word of the coordinate pair)
        add si, 8      ; Move to the next square coordinates
    loop updateLoopL
	jmp NoKeyPress	

rotate:
call rotateFallingShape	
		
moveFallingShapeH endp

rotateFallingShape proc near
	push ax
	push bx
	push cx
	push si
	push di


	mov cx, 17              ; Number of words to copy
	lea si, blockHorizontalShape4     ; Source index to start of blockTShape
	lea di, currentFallingShape ; Destination index to start of currentFallingShape
	rotateLoop:
		mov ax, [si]       ; Load a word from blockTShape
		mov [di], ax       ; Store the word in currentFallingShape
		add si, 2          ; Move to the next word in blockTShape
		add di, 2          ; Move to the next word in currentFallingShape
		loop rotateLoop      ; Decrement cx and repeat if cx is not zero

	pop di
	pop si
	pop cx
	pop bx
	pop ax
	ret


rotateFallingShape endp





end main