;===========================================================================
; Another Pong - by Elliott Fiedler (@leitdeux)
; main.asm
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

;===========================================================================
; Game loop, subroutines and data tables 
;===========================================================================
    .include "game.asm"
    .include "helpers.asm"
    .include "data.asm"

;===========================================================================
; Complete ROM
;===========================================================================
    org $fffc       ; move to memory address $fffc
    .word Start      ; write 2 bytes with program reset address
    .word Start      ; write 2 butes with interruption vector
