
    org SmoothFunx

***********************************************************
* Check Detect
*
* This function tests the ninth data point against the ninth
* extrapolated point and sets the alarm byte to FF if there is
* a detection
***********************************************************
check_detect:
     lda     samp8
     ldx     samp8
     mul                ; Y[8]^2

     clr     INTACC1
     clr     INTACC1+1
     stx     INTACC1+2
     sta     INTACC1+3

     lda     approx8
     ldx     approx8
     mul                ; Yapprox[8]^2

     clr     INTACC2
     clr     INTACC2+1
     stx     INTACC2+2
     sta     INTACC2+3

     jsr     add_32bit  ; intacc1 = y8^2 + y8approx^2

     lda     samp8
     ldx     approx8
     mul

     clr     INTACC2
     clr     INTACC2+1
     stx     INTACC2+2
     sta     INTACC2+3

     clc
     rol     INTACC2+3
     rol     INTACC2+2
     rol     INTACC2+1
     rol     INTACC2

     clc
     lda   samp8
     sub   approx8
     bcs   no_std_comp
     jsr   twos_comp

no_std_comp:
     jsr   add_32bit                 ; intacc1 = y8^2 + y8approx^2 + cond*2*y8*y8approx, where cond = -1 if y8 > y8approx

     mov   std_sum,INTACC2
     mov   std_sum+1,INTACC2+1
     mov   std_sum+2,INTACC2+2
     mov   std_sum+3,INTACC2+3
     jsr   twos_comp

     jsr   add_32bit                              ; intacc1 = std_calculations  -  4*S^2
     brset 7,INTACC1,no_detect                    ; check if result is negative... if it is, then no detection
     clc
     lda   samp8
     sub   approx8
     bcc   positive_std                           ; if samp8 > approx8, then positive std
     mov   #$FF,detect
     rts

positive_std:
     mov   #$01,detect
     rts

no_detect:
     mov   #$00,detect
     rts









***********************************************************
* Compute Standard of Deviation
*
* This function computes the standard of deviation
* on samp0-samp7 and approx0-approx7
*
* Standard Deviation stored in standard_dev
*
***********************************************************
compute_std:

      clr   std_sum
      clr   std_sum+1
      clr   std_sum+2
      clr   std_sum+3

      lda   samp0
      sta   std_samp
      mov   approx0,std_approx
      jsr   std_help

      lda   samp1
      sta   std_samp
      mov   approx1,std_approx
      jsr   std_help

      lda   samp2
      sta   std_samp
      mov   approx2,std_approx
      jsr   std_help

      lda   samp3
      sta   std_samp
      mov   approx3,std_approx
      jsr   std_help

      lda   samp4
      sta   std_samp
      mov   approx4,std_approx
      jsr   std_help

      lda   samp5
      sta   std_samp
      mov   approx5,std_approx
      jsr   std_help

      lda   samp6
      sta   std_samp
      mov   approx6,std_approx
      jsr   std_help

      lda   samp7
      sta   std_samp
      mov   approx7,std_approx
      jsr   std_help

********* at this point std_sum = sum((samp-approx)^2) *********
* This std_sum needs to be divided by the number of samples (N=8)
* However, we will be evaluating STD at 2*std or 4*S^2
* so just divide std_sum by two
*

      clc
      ror   std_sum
      ror   std_sum+1
      ror   std_sum+2
      ror   std_sum+3
      clc

      rts






**********************************************************
* Helper function for standard deviation function
*
**********************************************************
std_help:
      clc
      lda   std_samp
      sub   std_approx
      bcc   no_flip
      lda   std_approx
      sub   std_samp

no_flip:
      tax
      mul                       ; (Y-Yapprox)^2
      clr   INTACC1
      clr   INTACC1+1
      stx   INTACC1+2
      sta   INTACC1+3
      mov   std_sum,INTACC2
      mov   std_sum+1,INTACC2+1
      mov   std_sum+2,INTACC2+2
      mov   std_sum+3,INTACC2+3
      jsr   add_32bit
      mov   INTACC1,std_sum
      mov   INTACC1+1,std_sum+1
      mov   INTACC1+2,std_sum+2
      mov   INTACC1+3,std_sum+3

      rts







************************************************************
* Compute Approximation Curve
*
* This routine fits a quadratic curve to a series of 8 data
* points stored in samp0-samp7
*
* This function looks at a table stored in smooth_coefficients
* Each entry in the table is 32 bits and the MSB is a sign bit
* if MSB = 1, the coefficient value is supposed to be negative
*
* Nine approximated values are stored in approx0-approx8
* The ninth (extrapolated) value should be compared to samp8
* and the difference compared to the stand. dev.
*
*************************************************************
smooth_points:

        ldhx     #smooth_coefficients       ; load address of coefficient table
        sthx     smooth_row
        jsr      help_smooth                ; compute approx0
        mov      approx_temp,approx0

        ldhx     #smooth_coefficients
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx1
        mov      approx_temp,approx1

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx2
        mov      approx_temp,approx2

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx3
        mov      approx_temp,approx3

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx4
        mov      approx_temp,approx4

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx5
        mov      approx_temp,approx5

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx6
        mov      approx_temp,approx6

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx7
        mov      approx_temp,approx7

        ldhx     #smooth_coefficients
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        aix      #$20
        sthx     smooth_row                 ; increment row of table
        jsr      help_smooth                ; compute approx8
        mov      approx_temp,approx8

        rts

***********************************************************************
*  Smooth Helper Function
*
*  This routine does the math for the smoothing function
*
*  ASSUME - sum_terms ds 4 is declared
*           sample_temp ds 1 is declared
*
*  INPUT  - smooth_row:  the index for the coefficients
*  OUTPUT - approx_temp: the result is stored here
*
***********************************************************************
help_smooth:
        clr      sum_terms
        clr      sum_terms+1
        clr      sum_terms+2
        clr      sum_terms+3

        ldhx     smooth_row                             ; increment pointer to zero coefficient
        aix      #$00
        sthx     smooth_row
        lda      samp0
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to one coefficient
        sthx     smooth_row
        lda      samp1
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to two coefficient
        sthx     smooth_row
        lda      samp2
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to three coefficient
        sthx     smooth_row
        lda      samp3
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to four coefficient
        sthx     smooth_row
        lda      samp4
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to five coefficient
        sthx     smooth_row
        lda      samp5
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to six coefficient
        sthx     smooth_row
        lda      samp6
        sta      sample_temp
        jsr      help_smooth2

        ldhx     smooth_row
        aix      #$04                                   ; increment pointer to seven coefficient
        sthx     smooth_row
        lda      samp7
        sta      sample_temp
        jsr      help_smooth2

        clc                                             ; clear carry bit so it won't get rotated in
        ror      sum_terms
        ror      sum_terms+1                            ; divide by 2^17 (thats what the coefficents were scaled by)

        mov      sum_terms+1,approx_temp
        rts


***********************************************************************
* Smooth Helper of the Helper Function
*
* Computes a term
***********************************************************************
help_smooth2:
        clr      INTACC1
        clr      INTACC1+1
        clr      INTACC1+2
        mov      sample_temp,INTACC1+3                        ; load the sample value

        clr      INTACC2
        aix      #01
        mov      X+,INTACC2+1
        mov      X+,INTACC2+2
        mov      X+,INTACC2+3                                 ; load the coefficient

        jsr      UMULT32                                      ; intacc1:intacc2 = coefficient*sample (64bits)
                                                              ;                   for our purposes, intacc2 holds the answer

        ldhx     smooth_row
        mov      X+,calc_temp
        brclr    7,calc_temp,skip_neg_help1                   ; check if that term was supposed to be negative
        jsr      twos_comp

skip_neg_help1:
        mov      sum_terms,INTACC1
        mov      sum_terms+1,INTACC1+1
        mov      sum_terms+2,INTACC1+2
        mov      sum_terms+3,INTACC1+3                  ; intacc1 = sum_terms
        jsr      add_32bit                              ; intacc1 = sum_terms + current_term

        mov      INTACC1,sum_terms
        mov      INTACC1+1,sum_terms+1
        mov      INTACC1+2,sum_terms+2
        mov      INTACC1+3,sum_terms+3                  ; sum_terms = sum_terms + current_term

        rts


***********************************************
* Smoothing coefficients table
***********************************************

    org SmoothTable
smooth_coefficients:
*  The most significant bit of the following 32 bit values signifies whether it is positive or negative
*  DO NOT FORGET TO CHECK, THEN MASK OUT THE MSB WHEN YOU USE THESE VALUES!!!
*
*          < First   > <  Second > <  Third  > <  Fourth > <  Fifth  > <  Sixth  > < Seventh > < Eighth  >
       fdb $0001,$C9B2,$0000,$7C1F,$8000,$1F07,$8000,$3E0F,$8000,$1745,$0000,$1F07,$0000,$2E8B,$8000,$1F07
       fdb $0000,$7C1F,$0000,$BFB9,$0000,$A873,$0000,$5D17,$0000,$046E,$8000,$3ABC,$8000,$39A0,$0000,$2E8B
       fdb $8000,$1F07,$0000,$A873,$0000,$D5E3,$0000,$9F95,$0000,$3BD8,$8000,$1F07,$8000,$3ABC,$0000,$1F07
       fdb $8000,$3E0F,$0000,$5D17,$0000,$9F95,$0000,$A0B1,$0000,$77B0,$0000,$3BD8,$0000,$046E,$8000,$1745
       fdb $8000,$1745,$0000,$046E,$0000,$3BD8,$0000,$77B0,$0000,$A0B1,$0000,$9F95,$0000,$5D17,$8000,$3E0F
       fdb $0000,$1F07,$8000,$3ABC,$8000,$1F07,$0000,$3BD8,$0000,$9F95,$0000,$D5E3,$0000,$A873,$8000,$1F07
       fdb $0000,$2E8B,$8000,$39A0,$8000,$3ABC,$0000,$046E,$0000,$5D17,$0000,$A873,$0000,$BFB9,$0000,$7C1F
       fdb $8000,$1F07,$0000,$2E8B,$0000,$1F07,$8000,$1745,$8000,$3E0F,$8000,$1F07,$0000,$7C1F,$0001,$C9B2
       fdb $8001,$0000,$0001,$2492,$0001,$2492,$0000,$0000,$8001,$4924,$8001,$B6DB,$8000,$4924,$0004,$0000



