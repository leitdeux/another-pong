	processor 6502

;===========================================================================
; Include required files
;===========================================================================
    .include "include/vcs.h"
    .include "include/macro.h"
    .include "include/timer.h"
;===========================================================================
; Pong
;
; Features:
; 2 players
; ball
; center line
; collisions
; movement
; scoreboard
; sound effects (collision, score)
;
; Stretch goals:
; - title screen
; - increase ball speed as scores increase

; TODO:
; 1. -- draw players
; DONE - draw player 0
; DONE - draw player 1
; DONE - set y-position
; DONE - set x-position
; DONE - generalize player drawing
; 2. -- move players
; DONE - p0 down, up
; DONE - p1 down, up
; DONE - generalize player input
; 3. -- draw ball
; DONE - set x-position (fine)
; DONE - draw at y-position
; 3a. -- refactor player x-pos offset
; DONE - create general subroutine for settings x-pos offset
; DONE - use it directly after vblank
; 4. -- move ball
; DONE - horizontal, vertical
; DONE - velocity
; 5. -- handle ball collisions with screen top, bottom
; DONE - top
; DONE - bottom
; 6. -- handle collisions between p0,p1 and ball
; DONE - p0
; DONE - p1
; 7. -- handle collision with screen left, right
; DONE - reset ball position
; DONE - reset ball velocity
; DONE - update p0,p1 scores on collision
; 8. -- draw tennis net in center of screen
; DONE - enable M0
; DONE - draw with vertical gaps in between
; 9. -- implement scoreboard
; - implement with playfield?
; - implement using sprites?
; 10. -- sound effects
; - first/next ball
; - player collision
; - screen collision
; - goal collision
; 11. -- implement rng
; - set, reset ball velocity with random value
; 12. -- delay next ball for a few frames after goal
;===========================================================================

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

Player0Score .byte          ; score of P0
Player1Score .byte          ; score of P1

;-----------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------
SCANLINE_COUNT = 96         ; 2-line kernel; 192 / 2 = 96
SCREEN_X_MIN = 0            ; optical left edge of screen
SCREEN_X_MAX = 160          ; optical right edge of screen
SCREEN_Y_MAX = 94           ; optical top of the screen
SCREEN_Y_MIN = 4            ; optical bottom of the screen

PLAYER_HEIGHT = 16
PLAYER_0_X_INIT = 8         ; initial P0 x-position (in pixels)
PLAYER_1_X_INIT = 152       ; initial P1 x-position
PLAYER_Y_INIT = 45          ; initial Player y-position
PLAYER_Y_MAX = 80           ; max player y-position
PLAYER_SPRITE = %00000111

BALL_HEIGHT = 4
BALL_X_INIT = 88            ; initial Ball x-position (offset)
BALL_Y_INIT = 100           ; initial Ball y-position
BALL_SIZE = %00101000       ; 3 pixels

CENTER_TO_GOAL_FRAMES = 76  ; # of frames from screen center to a goal
PLAYER_TO_GOAL_FRAMES = 142 ; # of frames from one player to a goal

COLOR_WHITE = $0e
COLOR_BLUE = $a0
COLOR_PURPLE = $8e
COLOR_YELLOW = $1a

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

    lda #COLOR_WHITE        ; 2, set player colors
    sta COLUP0              ; 3
    sta COLUP1              ; 3
    lda #COLOR_YELLOW       ; 2, set ball color
    sta COLUPF              ; 3

;-----------------------------------------------------------------------
; Initialize fine x-position offsets for all motion objects (144)
;-----------------------------------------------------------------------
    sta WSYNC               ; 3, clear horiz. motion registers
    sta HMCLR               ; 3

    lda Player0X            ; 3, handle P0 x-position
    ldx #0                  ; 2
    jsr HandleObjXPosition  ; 39

    lda Player1X            ; 3, handle P1 x-position
    ldx #1                  ; 2
    jsr HandleObjXPosition  ; 39

    lda BallX               ; 3, handle Ball x-position
    ldx #4                  ; 2
    jsr HandleObjXPosition  ; 39

    lda #BALL_X_INIT        ; 2, handle M0 x-position
    ldx #2                  ; 2
    jsr HandleObjXPosition  ; 39

    sta WSYNC               ; 3, sync scanline before setting horizontal registers
    sta HMOVE               ; 3, apply *all* horizontal motion registers (HM__)

NextFrame:
    lsr SWCHB               ; 5?, handle game reset
    bcc Start               ; 2

    lda #SCANLINE_COUNT     ; 2, reset visible scanline count
    sta ScanlineCount       ; 3

    lda #COLOR_BLUE         ; 2, set background color
    sta COLUBK              ; 3

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

    lda #0                  ; 2, disable VBLANK
    sta VBLANK              ; 3
;===========================================================================
; Visible scanlines
;===========================================================================
DrawGame:
    ; FIXME -- improve drawing routines so that setup consistently occurs during h-blank period
    lda ScanlineCount       ; 3, load visible scanline count
VisibleScanlines:           ; 78
    ldx #0                  ; 2, draw P0
    jsr HandlePlayerDraw    ; 34
    ;sta WSYNC               ; 3, scanline (1 of 2) we sync after because we want
                            ; P0 and P1 to be drawn on the same line
    lda ScanlineCount       ; 3, restore scanline value for next player draw
    ldx #1                  ; 2, draw P1
    jsr HandlePlayerDraw    ; 34

HandleMissileDraw:
    ldx #0                  ; 2, value for disabling M0 graphics
    lda #4                  ; 2, the vertical width of M0 when AND-ed with ScanlineCount
    bit ScanlineCount
    beq .NoMissileDraw      ; 2, if the result of AND is not 0, do not draw M0
    ldx #2                  ; 2, enable M0 graphics
.NoMissileDraw:
    stx ENAM0               ; 3, enable or disable M0 graphics

    lda ScanlineCount       ; 3, load current scanline
HandleBallDraw:
    ldx #BALL_SIZE          ; 2, load ball width in X
    ldy #%00000010          ; 2, load ball graphics enable value in Y
    sec                     ; 2, set carry flag before subtract
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
    lda #2                  ; 2, enable VBLANK
    sta VBLANK              ; 3

OutputOverscan:
    TIMER_SETUP 29           ; use timer instead of iterating over each scanline

;===========================================================================
; Handle Ball collisions with screen (update ball y-velocity)
; X - ball y-position
; A - ball y-velocity (+1 or -1)
;===========================================================================
HandleScreenCollision:
    ldx BallY               ; load ball y-position in X
    lda #$ff                ; 2, load negative y-velocity (-1) in A
    cpx #SCREEN_Y_MAX       ; is ball at top of screen?
    bcs .UpdateBallYVel

    lda #1                  ; 2, load positive y-velocity (1) in A
    cpx #SCREEN_Y_MIN       ; is ball at bottom of screen?
    bcc .UpdateBallYVel

    lda BallYVel            ; load current y-velocity value
.UpdateBallYVel:
    sta BallYVel            ; set ball y-velocity to either -1, 1 or its current value

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
    bmi .ReverseXVelocity
    ldy #$d0                ; if x-velocity is positive, set negative and update P0 score
    ldx #0
.ReverseXVelocity:
    sty BallXVel
    inc Player0Score,x      ; 6, increment p0 or p1 score
    jmp .CommitBallUpdate
.NoGoal:
    inc GoalFrameCount      ; 5, increment frame count

;===========================================================================
; Handle Ball collisions with players (update ball x-velocity)
; - if collides with P1, set x-velocity to negative
; - if collides with P0, set x-velocity to positive
;===========================================================================
HandlePlayerCollision:
    lda BallXVel            ; 3, load current x-velocity in A
    bmi .HandleBallMoveLeft ; 2, is ball moving left?

    lda #%01000000          ; 2, has ball collided with P1?
    bit CXP1FB              ; 3
    bvc .NoPlayerCollision  ; 2

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

    lda #$30                ; 2, update x-velocity to positive value
    sta BallXVel            ; 3
    lda #0                  ; 2, reset goal frame counter
    sta GoalFrameCount
    lda #PLAYER_TO_GOAL_FRAMES
    sta GoalFrameTarget
.NoPlayerCollision:
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

;===========================================================================
; Data
;===========================================================================
    align $100              ; ensure data doesn't cross page boundary

; Scoreboard digits (0-9, 8x8 pixels)
DigitFontTable:
	.hex 003c6666766e663c007e181818381818
    .hex 007e60300c06663c003c66061c06663c
    .hex 0006067f661e0e06003c6606067c607e
    .hex 003c66667c60663c00181818180c667e
    .hex 003c66663c66663c003c66063e66663c

;===========================================================================
; Complete ROM
;===========================================================================
    org $fffc       ; move to memory address $fffc
    .word Start      ; write 2 bytes with program reset address
    .word Start      ; write 2 butes with interruption vector
