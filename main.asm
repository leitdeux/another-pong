;===========================================================================
; Pong
;
; Features:
; 2 players
; scoreboard
; sound effects (collision, score)
;===========================================================================

	processor 6502

    .include "includes/vcs.h"
    .include "includes/macro.h"
    .include "includes/timer.h"

;===========================================================================
; Declare variables starting from memory address $80
;===========================================================================
    seg.u Variables
    org $80

;-----------------------------------------------------------------------
; Variables
;-----------------------------------------------------------------------
ScanlineCount .byte         ; number of visible scanlines

Player0X .byte              ; P0 x-position
Player1X .byte              ; P1 x-position
Player0Y .byte              ; P0 y-position
Player1Y .byte              ; P1 y-position

BallX .byte                 ; ball x-position
BallY .byte                 ; ball y-position
BallXVel .byte              ; ball x velocity (signed)
BallYVel .byte              ; ball y velocity (signed)
BallXFrac .byte             ; ball x fractional value
                            ; BallXVel is added to BallXFrac so that the Ball
                            ; can move in incremenets less than 1px per frame

GoalFrameCount .byte        ; the current number of frames elapsed since a goal was made
GoalFrameTarget .byte       ; max # of frames until reaching a goal

Player0Score .byte          ; score of P0, 2 digits stored as binary-coded decimal (BCD)
Player1Score .byte          ; score of P1, 2 digits (BCD)
ScoreTemp .byte             ; temporary value used in score parsing

Player0ScoreSprite .byte    ; score sprite of P0
Player1ScoreSprite .byte    ; score sprite of P1

OnesDigitOffset .word       ; score digit pointers
TensDigitOffset .word

;-----------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------
SCOREBOARD_HEIGHT = 14
SCANLINE_COUNT = 96 - SCOREBOARD_HEIGHT         ; 2-line kernel; 192 / 2 = 96
SCREEN_X_MIN = 0            ; optical left edge of screen
SCREEN_X_MAX = 160          ; optical right edge of screen
SCREEN_Y_MAX = 80           ; optical top of the screen
SCREEN_Y_MIN = 4            ; optical bottom of the screen

PLAYER_HEIGHT = 16
PLAYER_0_X_INIT = 8         ; initial P0 x-position (in pixels)
PLAYER_1_X_INIT = 152       ; initial P1 x-position
PLAYER_Y_INIT = 35          ; initial Player y-position
PLAYER_Y_MAX = 65           ; max player y-position
PLAYER_SPRITE = %00000111

BALL_HEIGHT = 4
BALL_X_INIT = 88            ; initial Ball x-position (offset)
BALL_Y_INIT = 110           ; initial Ball y-position
BALL_SIZE = %00101100       ; set 3-pixel width and highest draw priority

CENTER_TO_GOAL_FRAMES = 77  ; # of frames from screen center to a goal
PLAYER_TO_GOAL_FRAMES = 143 ; # of frames from one player to a goal

SCORE_DIGITS_HEIGHT = 5     ; the height of player score digits

COLOR_WHITE = $0e
COLOR_BLUE = $a0
COLOR_PURPLE = $8e
COLOR_YELLOW = $1a
COLOR_GREEN = $c0

;===========================================================================
; Init code segment
;===========================================================================
    seg Code
    org $f000

Start:
    CLEAN_START

;-----------------------------------------------------------------------
; Initialize variables
;-----------------------------------------------------------------------
    lda #PLAYER_Y_INIT      ; 2, set default y-position
    sta Player0Y            ; 3
    sta Player1Y            ; 3

    lda #PLAYER_0_X_INIT    ; 2, set player default x-position
    sta Player0X            ; 3
    lda #PLAYER_1_X_INIT    ; 2
    sta Player1X            ; 3

    lda #BALL_Y_INIT        ; 2, set default ball y-position
    sta BallY               ; 3

    lda #BALL_X_INIT        ; 2, set ball default x-position
    sta BallX               ; 3

    lda #1                  ; 2, set default ball velocity
    sta BallYVel            ; 3
    lda #$30                ; 2, set ball x-velocity to 48
    sta BallXVel

    lda #0                  ; 2, init goal frame count
    sta GoalFrameCount
    lda #CENTER_TO_GOAL_FRAMES ; 2, set current target frame count until goal
    sta GoalFrameTarget

;-----------------------------------------------------------------------
; Initialize fine x-position offsets for all motion objects (144)
;-----------------------------------------------------------------------
    sta WSYNC               ; 3, clear horiz. motion registers
    sta HMCLR               ; 3

    lda BallX               ; 3, handle Ball x-position
    ldx #4                  ; 2
    jsr HandleObjXPosition  ; 39

    lda #BALL_X_INIT        ; 2, handle M0 x-position
    ldx #2                  ; 2
    jsr HandleObjXPosition  ; 39

    jsr ParseScoreDigits    ; setup score digits

    sta WSYNC               ; 3, sync scanline before setting horizontal registers
    sta HMOVE               ; 3, apply *all* horizontal motion registers (HM__)

NextFrame:
    lsr SWCHB               ; 5?, handle game reset
    bcc Start               ; 2

    lda #SCANLINE_COUNT     ; 2, reset visible scanline count
    sta ScanlineCount       ; 3

;===========================================================================
; Output VSync, VBlank
;===========================================================================
    lda #2                  ; 2, enable VBLANK, VSYNC
    sta VBLANK              ; 3
    sta VSYNC               ; 3

    sta WSYNC               ; 3, generate 3 lines of VSYNC
    sta WSYNC               ; 3
    sta WSYNC               ; 3

    lda #0                  ; 2, disable VSYNC
    sta VSYNC               ; 3

    ldx #37                 ; 2
OutputVBlank:
    sta WSYNC               ; 3
    dex                     ; 2
    bne OutputVBlank        ; 2
    stx VBLANK              ; 3, disable VBLANK

    jsr HandleScoreDraw     ; draw scoreboard

    ; set initial player positions after scoreboard draw
    lda Player0X            ; 3, handle P0 x-position
    ldx #0                  ; 2
    jsr HandleObjXPosition  ; 39

    lda Player1X            ; 3, handle P1 x-position
    ldx #1                  ; 2
    jsr HandleObjXPosition  ; 39

    lda #0                  ; 2, set background color
    sta COLUBK              ; 3
    lda #COLOR_YELLOW       ; 2, set ball color
    sta COLUPF              ; 3
    lda #COLOR_WHITE        ; 2, set player colors
    sta COLUP0              ; 3
    sta COLUP1              ; 3

;===========================================================================
; Visible scanlines
;===========================================================================
DrawGame:
    ; FIXME: -- improve drawing routines so that setup consistently occurs during h-blank period
    lda ScanlineCount       ; 3, load visible scanline count
VisibleScanlines:           ; 78
    ldx #0                  ; 2, draw P0
    jsr HandlePlayerDraw    ; 34
    ;sta WSYNC               ; 3, scanline (1 of 2) we sync after because we want
                            ; P0 and P1 to be drawn on the same line
    lda ScanlineCount       ; 3, restore scanline value for next player draw
    ldx #1                  ; 2, draw P1
    jsr HandlePlayerDraw    ; 34
.HandleMissileDraw:         ; draws the centered "tennis net"
    ldx #0                  ; 2, value for disabling M0 graphics
    lda #4                  ; 2, the vertical width of M0 when AND-ed with ScanlineCount
    bit ScanlineCount
    beq .NoMissileDraw      ; 2, if the result of AND is not 0, do not draw M0
    ldx #2                  ; 2, enable M0 graphics
.NoMissileDraw:
    stx ENAM0               ; 3, enable or disable M0 graphics
    lda ScanlineCount       ; 3, load current scanline
.HandleBallDraw:
    ldx #BALL_SIZE          ; 2, load ball width in X
    ldy #%00000010          ; 2, load ball-graphics-enable value in Y
    sec                     ; 2, set carry flag before subtraction
    sbc BallY               ; 3, subtract ball y-position from current scanline count
    cmp #BALL_HEIGHT        ; 2, is ball found in scanline?
    bcc .DrawBall           ; 2

    ldx #0                  ; 2, draw nothing
    ldy #0                  ; 2
.DrawBall:
    sta WSYNC               ; 3, draw scanline (2 of 2)
    stx CTRLPF              ; 3, set ball draw width
    sty ENABL               ; 3, enable ball draw

    dec ScanlineCount       ; 5, decrement visible scanline count
    lda ScanlineCount       ; 3, load updated scanline count
    bne VisibleScanlines    ; 2, branch if all scanlines have been drawn

;===========================================================================
; Output overscan
; - ensure we spend only 29 scanlines worth of work in here
; - at the end of it all, we call again WSYNC and HMOVE to apply motion updates
;===========================================================================
    ;lda #2                  ; 2, enable VBLANK
    ;sta VBLANK              ; 3
    lda #COLOR_BLUE
    sta COLUBK
OutputOverscan:
    ; TIMER_SETUP 29
    TIMER_SETUP 40           ; use timer instead of iterating over each scanline

;===========================================================================
; Handle Ball collisions with screen (update ball y-velocity)
; X - ball y-position
; Y - audio control
; A - ball y-velocity (+1 or -1)
;===========================================================================
HandleScreenCollision:
    ldx BallY               ; load ball y-position in X
    ldy #%00001011
    lda #%11111000
    sta AUDF0
    lda #$ff                ; 2, load negative y-velocity (-1) in A
    cpx #SCREEN_Y_MAX       ; is ball at top of screen?
    bcs .UpdateBallYVel
    lda #1                  ; 2, load positive y-velocity (1) in A
    cpx #SCREEN_Y_MIN       ; is ball at bottom of screen?
    bcc .UpdateBallYVel
    ldy #0
    lda BallYVel            ; load current y-velocity value
.UpdateBallYVel:
    sta BallYVel            ; set ball y-velocity to either -1, 1 or its current value
    cpy #0
    bne .PlayAudio
.PlayAudio:
    sty AUDC0
    sty AUDV0

;===========================================================================
; Handle Ball collision with "goal" (screen left, right)
; - compare the current frame count with the target frame count
; - if GoalFrameCount >= GoalFrameTarget, then consider the ball has reached
; the "goal" point; i.e. a player has scored a point
; - reset the fine x-position of the ball
; - reset the frame count and frame target
; - reverse the direction of the ball (x-velocity)
; - update the score of the player that earned a point
;===========================================================================
HandleGoalCollision:
    lda GoalFrameCount      ; 3, load current frame count in A
    cmp GoalFrameTarget     ; 3
    bcc .NoGoal             ; 2, is frame count < max frames?
    lda #BALL_Y_INIT        ; 2, reset ball y-position
    sta BallY
    lda BallX               ; 3, reset ball x-position
    ldx #4                  ; 2
    jsr HandleObjXPosition  ; 39
    lda #0                  ; reset frame count
    sta GoalFrameCount
    lda #CENTER_TO_GOAL_FRAMES ; reset target # of frames until goal should be checked
    sta GoalFrameTarget
    ldy #$30                ; if x-velocity is negative, set positive and update P1 score
    ldx #1
    lda BallXVel
    bmi .ReflectXVel
    ldy #$d0                ; if x-velocity is positive, set negative and update P0 score
    ldx #0
.ReflectXVel:
    sty BallXVel
    sed                     ; 2, enable BCD
    lda Player0Score,x      ; 4
    clc
    adc #1                  ; 2, increment score
    sta Player0Score,x      ; 4
    cld                     ; 2, disable BCD
    jmp .CommitBallUpdate
.NoGoal:
    inc GoalFrameCount      ; 5, increment frame count

;===========================================================================
; Handle Ball collisions with players (update ball x-velocity)
; - if collides with P1, set x-velocity to negative
; - if collides with P0, set x-velocity to positive
;===========================================================================
HandlePlayerCollision:
    ldy #0
    lda BallXVel            ; 3, load current x-velocity in A
    bmi .HandleBallMoveLeft ; 2, is ball moving left?
    lda #%01000000          ; 2, has ball collided with P1?
    bit CXP1FB              ; 3
    bvc .NoPlayerCollision  ; 2
    ldy #%00011101
    lda #$d0                ; 2, update x-velocity to negative value
    sta BallXVel            ; 3
    lda #0                  ; 2, reset goal frame counter and update target frames
    sta GoalFrameCount
    lda #PLAYER_TO_GOAL_FRAMES
    sta GoalFrameTarget
.HandleBallMoveLeft:
    lda #%01000000          ; 2, has ball collided with P0?
    bit CXP0FB              ; 3
    bvc .NoPlayerCollision  ; 2
    ldy #%00011101
    lda #$30                ; 2, update x-velocity to positive value
    sta BallXVel            ; 3
    lda #0                  ; 2, reset goal frame counter
    sta GoalFrameCount
    lda #PLAYER_TO_GOAL_FRAMES
    sta GoalFrameTarget
.NoPlayerCollision:
    sty AUDC0
    sty AUDV0
    sta CXCLR               ; 3, clear collision registers

;===========================================================================
; Update Ball x,y position via velocity
;===========================================================================
UpdateBallPosition:
    sta WSYNC               ; 3, necessary?
    sta HMCLR               ; 3

    lda BallY               ; 3, update y-position
    clc                     ; 2, clear carry
    adc BallYVel            ; 3, increment y-position by y-velocity
    sta BallY               ; 3
    lda BallXVel            ; 3, update x-position
    bmi .MoveBallLeft       ; 2, if x-velocity < 0, move Ball left

    clc                     ; 2, clear carry
    adc BallXFrac           ; 3, increment x-position by x-velocity
    sta BallXFrac           ; 3, BallXFrac += BallXVel
    lda #$f0                ; 2, set ball motion register to move right 1 clock
    sta HMBL                ; 3
    bne .CommitBallUpdate   ; 2

.MoveBallLeft:
    clc                     ; 2, set carry flag
    adc BallXFrac           ; seems to work...? looks strange, however
    sta BallXFrac           ; 3, update x-velocity value
    lda #$10                ; 2, set ball motion register to move left by 1 clock
    sta HMBL                ; 3
.CommitBallUpdate:
    sta WSYNC               ; 3
    sta HMOVE               ; 3, commit all horizontal motion updates

;===========================================================================
; Handle player input for next frame (88)
;===========================================================================
HandlePlayerInputs:
    lda #%00010000          ; 2, handle P0 input
    ldx Player0Y            ; 3, load P0 y-position
    jsr HandlePlayerInput   ; 36
    stx Player0Y            ; 3, update P0 y-position

    lda #%00000001          ; 2, handle P1 input
    ldx Player1Y            ; 3, load P1 y-position
    jsr HandlePlayerInput   ; 36
    stx Player1Y            ; 3, update P1 y-position

    jsr ParseScoreDigits    ; update score digits

;-----------------------------------------------------------------------
; Overscan timer expires
;-----------------------------------------------------------------------
    TIMER_WAIT              ; wait until 29 scanlines, and then strobe
                            ; one more scanline for 30 total in overscan

;===========================================================================
; Draw next frame
;===========================================================================
    jmp NextFrame

;===========================================================================
; Subroutines
;===========================================================================

;-----------------------------------------------------------------------
; HandlePlayerDraw (28)
; A is current scanline count
; X is player to draw (0 = player-0, 1 = player-1)
; Y is player sprite value (0 if not drawn)
;-----------------------------------------------------------------------
HandlePlayerDraw subroutine
    ldy #PLAYER_SPRITE      ; 2, load player sprite in Y
    sec                     ; 2, set carry for subtract
    sbc Player0Y,x          ; 4, subtract the player-y from A
    cmp #PLAYER_HEIGHT      ; 2, is sprite found in scanline?
    bcc .DrawPlayer         ; 2

    ldy #0                  ; 2, reset value in Y (don't draw player)
.DrawPlayer:
    clc                     ; 2, clear carry flag
    tya                     ; 2, transfer sprite value in Y to A
    sta GRP0,x              ; 4, store player-0 bitmap
    rts                     ; 6

;-----------------------------------------------------------------------
; HandlePlayerInput (30)
; A is the bitmask which is AND-ed to detect input change
; X is P0 or P1 y-position
;-----------------------------------------------------------------------
HandlePlayerInput subroutine
    bit SWCHA               ; 3
    bne .HandleDownInput    ; 2

    cpx #PLAYER_Y_MAX       ; 2, is player y-position at top of screen?
    beq .HandleDownInput    ; 2

    inx                     ; 2, increment y-position in X
.HandleDownInput:
    asl                     ; 2, bit-shift left to test the down input
    bit SWCHA               ; 3
    bne .HandleNoInput      ; 2

    cpx #SCREEN_Y_MIN       ; 2, is y-position at bottom of screen?
    beq .HandleNoInput      ; 2

    dex                     ; 2, decrement y-position in X
.HandleNoInput:
    rts                     ; 6

;-----------------------------------------------------------------------
; Handle motion object x-position with fine offset (33)
; A is desired x-position in pixels
; X is type of object (0 = P0, 1 = P1, 2 = M0, 3 = M1, 4 = Ball)
;-----------------------------------------------------------------------
HandleObjXPosition subroutine
    sta WSYNC               ; 3, we want to do this work during the h-blank period
    sec                     ; 2, ensure carry flag is set for subtraction
.DivideBy15Clocks:
    sbc #15                 ; 2, subtract 15 TIA clocks (5 CPU cycles) from A
    bcs .DivideBy15Clocks   ; 2, loop until negative
    eor #7                  ; 2, adjust remainder between -8 and +7 using XOR
    asl                     ; 2 bit-shift left 4 times, as HM__ uses left-most nibble
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    sta RESP0,x             ; 4, set coarse x-position for object x
    sta HMP0,x              ; 4, set fine x-position for object x
    rts                     ; 6

;-----------------------------------------------------------------------
; Handle player scoreboard draw
;
;-----------------------------------------------------------------------
HandleScoreDraw subroutine
    lda #COLOR_BLUE
    sta COLUBK
    lda #0                  ; clear all TIA registers
    sta GRP0
    sta GRP1
    sta HMCLR
    nop
    nop
    nop
    sta RESP0
    sta HMP0
    nop
    nop
    nop
    sta RESP1
    sta HMP1

    lda #%00000101
    sta NUSIZ0
    sta NUSIZ1

    lda #COLOR_PURPLE       ; set color of PF
    sta COLUP0
    sta COLUP1

    ldx #SCORE_DIGITS_HEIGHT
DrawScoreboard:
.DrawPlayer0Score:
    ldy TensDigitOffset     ; load ten's digit offset for score
    lda Digits,y            ; load value in Digits table
    and #$f0                ; isolate hi-nibble and store in P0 score
    sta Player0ScoreSprite
    ldy OnesDigitOffset     ; load one's digit
    lda Digits,y
    and #$0f                ; isolate lo-nibble and merge
    ora Player0ScoreSprite  ; merge one's and ten's digit values
    sta Player0ScoreSprite  ; store result in P0 score sprite
    sta WSYNC
    sta GRP0                 ; display sprite middle PF register
.DrawPlayer1Score:
    ldy TensDigitOffset+1   ; load ten's digit
    lda Digits,y
    and #$f0
    sta Player1ScoreSprite
    ldy OnesDigitOffset+1   ; load one's digit
    lda Digits,y
    and #$0f
    ora Player1ScoreSprite  ; merge one's and ten's digits
    sta Player1ScoreSprite  ; store result in P1 score sprite
    sta GRP1
    ldy Player0ScoreSprite  ; preload sprite for next scanline
    sta WSYNC
    sty GRP0                ; update PF for P0 score display
    inc TensDigitOffset     ; increment all digits for next line of data
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1
    dex
    bne DrawScoreboard
    sta WSYNC
    lda #0
    sta GRP0
    sta GRP1
    sta NUSIZ0
    sta NUSIZ1
    rts

;------------------------------------------------------------------------
; Parse score digits (for later handling in BCD mode)
; X - Score (0 = P0, 1 = P1)
; Subroutine for scoreboard digits used in score for P0 and P1
; Calculates the digit offsets for both scores.
; Convert high and low "nibbles" of Score vars into
; the offsets of Digits look-up table so values are displayed.
;
; For the low bits, we need to multiply by DIGIT_HEIGHT to get correct digit
; row in table. We can use left bit-shifts to perform multiplications by 2:
; For any number N, the value N * 5 = (N * 2 * 2) + N
; e.g. n = 2
; 8 + 2 = 10 <-- the row of the digit in the table
;
; For the high bits, since it's already * 16 (since it's already hexadecimal),
; we need to divide by 16 and then multiply by DIGIT_HEIGHT.
; - use right bit-shifts to perform division by 2
; - for any number N, the value of (N / 16) * 5 = (N / 2 / 2) + (N / 2 / 2 / 2 / 2)
; e.g. n = 2
; (2 / 4) + (2 / 16) = 10 / 16 = (5 / 8) = .625
;-----------------------------------------------------------------------
ParseScoreDigits subroutine
    ldx #1                  ; loop counter (P1 score is calculated first)
.ParseDigits
    lda Player0Score,x      ; load current score value
    and #$0f                ; isolate one's digit (lo-nibble)
    sta ScoreTemp           ; store value in temp var
    asl                     ; get the one's digit (n * 4)
    asl
    adc ScoreTemp           ; (+ n)
    sta OnesDigitOffset,x
    lda Player0Score,x
    and #$f0                ; isolate the ten's digit (hi-nibble)
    lsr                     ; n / 4
    lsr
    sta ScoreTemp
    lsr                     ; n / 16
    lsr
    adc ScoreTemp           ; add temp value to A (n / 4 + n / 16)
    sta TensDigitOffset,x
    dex                     ; load next score
    bpl .ParseDigits
    rts

;===========================================================================
; Data Tables
;===========================================================================
    align $100              ; ensure data doesn't cross page boundary

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

;===========================================================================
; Complete ROM
;===========================================================================
    org $fffc       ; move to memory address $fffc
    .word Start      ; write 2 bytes with program reset address
    .word Start      ; write 2 butes with interruption vector
