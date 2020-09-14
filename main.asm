	processor 6502

;===========================================================================
; Include required files
;===========================================================================
    include "include/vcs.h"
    include "include/macro.h"

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
; - use it directly after vblank
; 4. -- move ball
; - velocity
; 5. -- handle ball collisions with top, bottom, left, right
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

;-----------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------
SCANLINE_COUNT = 96         ; 2-line kernel; 192 / 2 = 96

PLAYER_HEIGHT = 16
PLAYER_0_X_INIT = 0        ; initial P0 x-position
PLAYER_1_X_INIT = 80       ; initial P1 x-position
PLAYER_Y_INIT = 47          ; the initial y-position in pixels
PLAYER_SPRITE = %00000111

BALL_HEIGHT = 5
BALL_X_INIT = 0            ; the initial x-position in pixels
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
    lda #PLAYER_Y_INIT      ; set default y-position
    sta Player0Y
    sta Player1Y
    sta BallY

    lda #PLAYER_0_X_INIT    ; set player default x-position
    sta Player0X
    lda #PLAYER_1_X_INIT
    sta Player1X

    lda #BALL_X_INIT        ; set ball default x-position
    sta BallX

    lda #COLOR_PURPLE       ; set player colors
    sta COLUP0
    lda #COLOR_YELLOW
    sta COLUP1
    lda #COLOR_BLUE         ; set background color
    sta COLUBK
    lda #COLOR_WHITE        ; set ball color
    sta COLUPF

NextFrame:
    lsr SWCHB               ; handle game reset
    bcc Start

    lda #SCANLINE_COUNT     ; reset visible scanline count
    sta ScanlineCount

;===========================================================================
; Output VSync, VBlank
;===========================================================================
    lda #2                  ; enable VBLANK, VSYNC
    sta VBLANK
    sta VSYNC

    sta WSYNC               ; generate 3 lines of VSYNC
    sta WSYNC
    sta WSYNC

    lda #0                  ; disable VSYNC
    sta VSYNC

    ldx #37
OutputVBlank:
    sta WSYNC
    dex
    bne OutputVBlank

    lda #0                  ; disable VBLANK
    sta VBLANK

    ;sta WSYNC
    ;sta HMCLR               ; reset previous fine x-position(s)

;-----------------------------------------------------------------------
; Set horizontal offsets for all motion objects
;-----------------------------------------------------------------------
    lda Player0X            ; handle P0 x-position
    ldx #0
    jsr HandleObjXPosition

    lda Player1X            ; handle P1 x-position
    ldx #1
    jsr HandleObjXPosition

    lda BallX               ; handle ball x-position
    ldx #4
    jsr HandleObjXPosition

    sta WSYNC               ; sync scanline before setting horizontal registers
    sta HMOVE               ; enable *all* horizontal motion registers (HM__)

;===========================================================================
; Visible scanlines
;===========================================================================
DrawGame:
    lda #1                  ; delay drawing of P0 until P1 is drawn
    sta VDELP0

    lda ScanlineCount       ; load visible scanline count
VisibleScanlines:
    ldx #0                  ; draw P0
    jsr HandlePlayerDraw    ; 6
    sta WSYNC               ; scanline (1 of 2) we sync after because we want
                            ; P0 and P1 to be drawn on the same line
    lda ScanlineCount       ; restore scanline value for next player draw
    ldx #1                  ; draw P1
    jsr HandlePlayerDraw    ; 6

    lda ScanlineCount       ; load current scanline
HandleBallDraw:
    ldx #BALL_SIZE          ; load ball width in X
    ldy %00000010           ; load ball graphics enable value in Y
    sec                     ; set carry flag before subtract
    sbc BallY               ; subtract ball y-position from current scanline count
    cmp #BALL_HEIGHT        ; is ball found in scanline?
    bcc .DrawBall
    ldx #0                  ; draw nothing
    ldy #0
.DrawBall:
    txa                     ; transfer ball width to A
    sta CTRLPF
    tya                     ; transfer graphics enable value to A
    sta ENABL               ; enable ball graphic

    sta WSYNC               ; draw scanline (2 of 2)

    dec ScanlineCount       ; decrement visible scanline count
    lda ScanlineCount       ; load updated scanline count
    bne VisibleScanlines    ; branch if all scanlines have been drawn

;===========================================================================
; Output overscan
;===========================================================================
    lda #2
    sta VBLANK

    ldx #29
OutputOverscan:
    sta WSYNC
    dex
    bne OutputOverscan

;===========================================================================
; Handle player input for next frame
;===========================================================================
    lda #%00010000          ; handle P0 input
    ldx Player0Y            ; load P0 y-position
    jsr HandlePlayerInput
    stx Player0Y            ; update P0 y-position

    lda #%00000001          ; handle P1 input
    ldx Player1Y            ; load P1 y-position
    jsr HandlePlayerInput
    stx Player1Y            ; update P1 y-position

;===========================================================================
; Draw next frame
;===========================================================================
    jmp NextFrame

;===========================================================================
; Subroutines
;===========================================================================
;-----------------------------------------------------------------------
; HandlePlayerDraw
; A is current scanline count
; X is player to draw (0 = player-0, 1 = player-1)
; Y is player sprite value (0 if not drawn)
;-----------------------------------------------------------------------
HandlePlayerDraw subroutine
    ldy #PLAYER_SPRITE      ; load player sprite in Y
    sec                     ; set carry for subtract
    sbc Player0Y,x          ; subtract the player-y from A
    cmp #PLAYER_HEIGHT      ; is sprite found in scanline?
    bcc .DrawPlayer
    ldy #0                  ; reset value in Y (don't draw player)
.DrawPlayer:
    clc                     ; clear carry flag
    tya                     ; transfer sprite value in Y to A
    sta GRP0,x              ; store player-0 bitmap
    rts                     ; 6

;-----------------------------------------------------------------------
; HandlePlayerInput
; A is the bitmask which is AND-ed to detect input change
; X is P0 or P1 y-position
;-----------------------------------------------------------------------
HandlePlayerInput subroutine
    bit SWCHA
    bne .HandleDownInput
    cpx #80                 ; is y-position at top of screen?
    beq .HandleDownInput
    inx                     ; increment y-position in X
.HandleDownInput:
    asl                     ; bit-shift left to test the down input
    bit SWCHA
    bne .HandleNoInput
    cpx #4                  ; is y-position at bottom of screen?
    beq .HandleNoInput
    dex                     ; decrement y-position in X
.HandleNoInput:
    rts

;-----------------------------------------------------------------------
; Handle motion object x-position with fine offset
; A is desired x-position in pixels
; X is type of object (0 = P0, 1 = P1, 2 = M0, 3 = M3, 4 = Ball)
;-----------------------------------------------------------------------
HandleObjXPosition subroutine
    sta WSYNC               ; we want to do this work during the h-blank period
    sec                     ; ensure carry flag is set for subtraction
.DivideBy15Clocks:
    sbc #15                 ; subtract 15 TIA clocks (5 CPU cycles) from A
    bcs .DivideBy15Clocks   ; loop until negative
    eor #7                  ; adjust remainder between -8 and +7 using XOR
    asl                     ; bit-shift left 4 times, as HM__ uses left-most nibble
    asl
    asl
    asl
    sta HMP0,x              ; set fine x-position for object x
    sta RESP0,x             ; set coarse x-position for object x
    rts

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
