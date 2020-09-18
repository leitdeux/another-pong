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
; 5. -- handle ball collisions with top, bottom, left, right
; DONE - top
; DONE - bottom
; 6. -- handle collisions between p0,p1 and ball
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
BallYVel .byte              ; ball y velocity (signed)
BallXVel .byte              ; ball x velocity (signed)
BallXFrac .byte             ; ball x fractional value
                            ; BallXVel is added to BallXFrac so that the Ball
                            ; can move in incremenets less than 1px per frame

;-----------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------
SCANLINE_COUNT = 96         ; 2-line kernel; 192 / 2 = 96
SCREEN_Y_MAX = 94           ; top of the screen
SCREEN_Y_MIN = 4            ; bottom of the screen

PLAYER_HEIGHT = 16
PLAYER_0_X_INIT = 0         ; initial P0 x-position (in pixels)
PLAYER_1_X_INIT = 160       ; initial P1 x-position
PLAYER_Y_INIT = 45          ; initial Player y-position
PLAYER_Y_MAX = 80           ; max player y-position
PLAYER_SPRITE = %00000111

BALL_HEIGHT = 4
BALL_X_INIT = 88            ; initial Ball x-position
BALL_Y_INIT = 50            ; initial Ball y-position
BALL_SIZE = %00101000       ; 3 pixels

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

    lda #BALL_Y_INIT        ; 2, set default ball y-position
    sta BallY               ; 3

    lda #1                  ; 2, set default ball velocity
    sta BallYVel            ; 3
    lda #48                 ; 2
    sta BallXVel

    lda #PLAYER_0_X_INIT    ; 2, set player default x-position
    sta Player0X            ; 3
    lda #PLAYER_1_X_INIT    ; 2
    sta Player1X            ; 3

    lda #BALL_X_INIT        ; 2, set ball default x-position
    sta BallX               ; 3

    lda #COLOR_PURPLE       ; 2, set player colors
    sta COLUP0              ; 3
    lda #COLOR_YELLOW       ; 2
    sta COLUP1              ; 3
    lda #COLOR_WHITE        ; 2, set ball color
    sta COLUPF              ; 3

;-----------------------------------------------------------------------
; Init x offsets for all motion objects (144)
;-----------------------------------------------------------------------
    sta WSYNC               ; 3, clear horiz. motion registers
    sta HMCLR               ; 3

    lda Player0X            ; 3, handle P0 x-position
    ldx #0                  ; 2
    jsr HandleObjXPosition  ; 39

    lda Player1X            ; 3, handle P1 x-position
    ldx #1                  ; 2
    jsr HandleObjXPosition  ; 39

    lda BallX               ; 3, handle ball x-position
    ldx #4                  ; 2
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
    lda ScanlineCount       ; 3, load visible scanline count
VisibleScanlines:           ; 78
    ldx #0                  ; 2, draw P0
    jsr HandlePlayerDraw    ; 34
    sta WSYNC               ; 3, scanline (1 of 2) we sync after because we want
                            ; P0 and P1 to be drawn on the same line
    lda ScanlineCount       ; 3, restore scanline value for next player draw
    ldx #1                  ; 2, draw P1
    jsr HandlePlayerDraw    ; 34

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
    txa                     ; 2, transfer ball width to A
    sta CTRLPF              ; 3
    tya                     ; 2, transfer graphics enable value to A
    sta ENABL               ; 3, enable ball graphic

    sta WSYNC               ; 3, draw scanline (2 of 2)

    dec ScanlineCount       ; 5, decrement visible scanline count
    lda ScanlineCount       ; 3, load updated scanline count
    bne VisibleScanlines    ; 2, branch if all scanlines have been drawn

;===========================================================================
; Output overscan
;===========================================================================
    lda #2                  ; 2, enable VBLANK
    sta VBLANK              ; 3

    ; consider using timer: https://8bitworkshop.com/v3.6.0/?file=examples%2Fcollisions.a&platform=vcs
    ; to ensure we spend only 29 scanlines worth of work in here
    ; at the end of it all, we call again WSYNC and HMOVE to apply horizontal motion updates

    ;ldx #29                 ; 2
OutputOverscan:
    ;sta WSYNC               ; 3
    ;dex                     ; 2
    ;bne OutputOverscan      ; 2

    TIMER_SETUP 29           ; use timer instead of iterating over each scanline

;===========================================================================
; Update x offsets for motion objects
;===========================================================================
    ; sta WSYNC               ; 3, clear horiz. motion registers
    ; sta HMCLR               ; 3

    ; lda BallX               ; 3, handle ball x-position
    ; ldx #4                  ; 2
    ; jsr HandleObjXPosition  ; 39

    ; sta WSYNC               ; 3, sync scanline before setting horizontal registers
    ; sta HMOVE               ; 3, apply *all* horizontal motion registers (HM__)

;===========================================================================
; Handle Ball collisions
;===========================================================================
    ldx BallY               ; load ball y-position in X
    lda #$ff                ; 2, load -1 in A
    cpx #SCREEN_Y_MAX       ; is ball at top of screen?
    bcs .UpdateBallYVel
    lda #1                  ; 2, load +1 in A
    cpx #SCREEN_Y_MIN       ; is ball at bottom of screen?
    bcc .UpdateBallYVel
    lda BallYVel            ; load current y-velocity value
.UpdateBallYVel:
    sta BallYVel            ; set ball y-velocity to either -1, 1 or its current value

;===========================================================================
; Update Ball position via velocity
;===========================================================================
UpdateBallPosition:
    sta WSYNC               ; 3, necessary?
    sta HMCLR               ; 3

    lda BallY               ; 3, update y-position
    clc                     ; 2, clear carry

; TODO - handle y-position update
;     lda BallYVel
;     bmi .DecrementBallYVel
;     lda BallY
;     adc BallYVel
;     adc BallYVel
; .DecrementBallYVel:
;     lda BallY
;     sec
;     sbc BallYVel

    adc BallYVel            ; 3, increment y-position by y-velocity
    sta BallY               ; 3

    lda BallXVel            ; 3, update x-position
    bmi .MoveBallLeft       ; 2, if x-velocity < 0, move Ball left
    clc                     ; 2, clear carry
    adc BallXFrac           ; 3, increment x-position by x-velocity
    sta BallXFrac           ; 3, BallXFrac += BallXVel
    lda #$f0                ; 2
    sta HMBL                ; 3, set ball motion register
    bne .Foo                ; 2
.MoveBallLeft:
    sec                     ; 2, set carry flag
    sbc BallXFrac           ; 3?, subtract x-velocity in A from BallXFrac
    sta BallXFrac           ; 3, update x-velocity value
    bcs .Foo                ; 2, branch if carry flag was set
    lda #$10                ; 2
    sta HMBL                ; 3, set ball motion register
.Foo:
    sta WSYNC               ; 3
    sta HMOVE               ; 3

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
; X is type of object (0 = P0, 1 = P1, 2 = M0, 3 = M3, 4 = Ball)
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
    ; IDEA: -- do an additional: (so each object uses 2 scanlines instead of just one, might be useful)
    ; sta WSYNC
    ; sta HMOVE
    ; sta HMCLR
    rts                     ; 6

;===========================================================================
; Data
;===========================================================================
; TODO -- scoreboard digits
;===========================================================================
; Complete ROM
;===========================================================================
    org $fffc       ; move to memory address $fffc
    .word Start      ; write 2 bytes with program reset address
    .word Start      ; write 2 butes with interruption vector
