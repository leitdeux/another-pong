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
; - increase velocity as scores increase
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
Player0Y .byte              ; P0 y-position
Player1Y .byte              ; P1 y-position
BallX .byte                 ; ball x-position
BallY .byte                 ; ball y-position

;-----------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------
SCANLINE_COUNT = 96         ; 2-line kernel, so 192 / 2 = 96
PLAYER_HEIGHT = 16
PLAYER_Y_INIT = 47
PLAYER_SPRITE = %00000111

BALL_HEIGHT = 6
BALL_SIZE = %00101000       ; 3 pixels

COLOR_WHITE = $0e
COLOR_BLUE = $a0
COLOR_PURPLE = $8e
COLOR_YELLOW = $1a


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
; 4. -- move ball
; 5. -- handle collisions

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

    lda #COLOR_PURPLE        ; set player colors
    sta COLUP0
    lda #COLOR_YELLOW
    sta COLUP1

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

;===========================================================================
; Visible scanlines
;===========================================================================
DrawGame:
    SLEEP 12                ; set P0 x-position
    sta RESP0

    ; FIXME -- this only works with a stationary ballww
    SLEEP 20                ; set ball x-position
    sta RESBL

    SLEEP 17                ; set P1 x-position
    sta RESP1

    lda #COLOR_BLUE         ; set background color
    sta COLUBK

    lda ScanlineCount       ; load visible scanline count
VisibleScanlines:
    ldx #0                  ; draw P0
    jsr HandlePlayerDraw    ; 6
    sta WSYNC               ; scanline (1 of 2) we sync after because we want
                            ; P0 and P1 to be drawn on the same line
    lda ScanlineCount       ; restore scanline value for next player draw
    ldx #1                  ; draw P1
    jsr HandlePlayerDraw    ; 6
    sta WSYNC               ; scanline (2 of 2)

    ; TODO -- handle ball drawing here
    ;lda #%00101000         ; set ball width to 3 pixels
    ;sta CTRLPF
    ;lda #2                  ; draw ball graphics
    ;sta ENABL

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
    cmp #PLAYER_HEIGHT      ; in sprite?
    bcc .DrawPlayer
    ldy #0                  ; reset value in Y (don't draw player)
.DrawPlayer:
    clc                     ; clear carry flag
    tya                     ; transfer sprite value in Y to A
    ;sta WSYNC               ; scanline sync (1 of 2)
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

;===========================================================================
; Data
;===========================================================================

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
