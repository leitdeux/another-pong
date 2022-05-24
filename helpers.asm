;===========================================================================
; Another Pong - by Elliott Fiedler (@leitdeux)
; helpers.asm - subroutines
;===========================================================================

;-----------------------------------------------------------------------
; HandlePlayerDraw (28 cycles)
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

