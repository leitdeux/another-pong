;===========================================================================
; Another Pong - by Elliott Fiedler (@leitdeux)
; game.asm - the game loop
; - by Elliott Fiedler (@leitdeux)
;===========================================================================
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
    jmp NextFrame

