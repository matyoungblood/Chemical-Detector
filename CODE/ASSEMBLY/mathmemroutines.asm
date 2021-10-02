

  org MathRoutines
********************************************************************************
*
*     32 x 16 Unsigned Divide
*
*     This routine takes the 32-bit dividend stored in INTACC1.....INTACC1+3
*     and divides it by the 16-bit divisor stored in INTACC2:INTACC2+1.
*     The quotient replaces the dividend and the remainder replaces the divisor.
*
UDVD32    EQU     *
*
DIVIDEND  EQU     INTACC1+2
DIVISOR   EQU     INTACC2
QUOTIENT  EQU     INTACC1
REMAINDER EQU     INTACC1
*
        PSHH                            ;save h-reg value
        PSHA                            ;save accumulator
        PSHX                            ;save x-reg value
        AIS     #-3                     ;reserve three bytes of temp storage
        LDA     #!32                    ;
        STA     3,SP                    ;loop counter for number of shifts
        LDA     DIVISOR                 ;get divisor msb
        STA     1,SP                    ;put divisor msb in working storage
        LDA     DIVISOR+1               ;get divisor lsb
        STA     2,SP                    ;put divisor lsb in working storage
*
*     Shift all four bytes of dividend 16 bits to the right and clear
*     both bytes of the temporary remainder location
*
        MOV     DIVIDEND+1,DIVIDEND+3   ;shift dividend lsb
        MOV     DIVIDEND,DIVIDEND+2     ;shift 2nd byte of dividend
        MOV     DIVIDEND-1,DIVIDEND+1   ;shift 3rd byte of dividend
        MOV     DIVIDEND-2,DIVIDEND     ;shift dividend msb
        CLR     REMAINDER               ;zero remainder msb
        CLR     REMAINDER+1             ;zero remainder lsb
*
*     Shift each byte of dividend and remainder one bit to the left
*
SHFTLP  LDA     REMAINDER               ;get remainder msb
        ROLA                            ;shift remainder msb into carry
        ROL     DIVIDEND+3              ;shift dividend lsb
        ROL     DIVIDEND+2              ;shift 2nd byte of dividend
        ROL     DIVIDEND+1              ;shift 3rd byte of dividend
        ROL     DIVIDEND                ;shift dividend msb
        ROL     REMAINDER+1             ;shift remainder lsb
        ROL     REMAINDER               ;shift remainder msb
*
*     Subtract both bytes of the divisor from the remainder
*
        LDA     REMAINDER+1             ;get remainder lsb
        SUB     2,SP                    ;subtract divisor lsb from remainder lsb
        STA     REMAINDER+1             ;store new remainder lsb
        LDA     REMAINDER               ;get remainder msb
        SBC     1,SP                    ;subtract divisor msb from remainder msb
        STA     REMAINDER               ;store new remainder msb
        LDA     DIVIDEND+3              ;get low byte of dividend/quotient
        SBC     #0                      ;dividend low bit holds subtract carry
        STA     DIVIDEND+3              ;store low byte of dividend/quotient
*
*     Check dividend/quotient lsb. If clear, set lsb of quotient to indicate
*     successful subraction, else add both bytes of divisor back to remainder
*
        BRCLR   0,DIVIDEND+3,SETLSB     ;check for a carry from subtraction
                                        ;and add divisor to remainder if set
        LDA     REMAINDER+1             ;get remainder lsb
        ADD     2,SP                    ;add divisor lsb to remainder lsb
        STA     REMAINDER+1             ;store remainder lsb
        LDA     REMAINDER               ;get remainder msb
        ADC     1,SP                    ;add divisor msb to remainder msb
        STA     REMAINDER               ;store remainder msb
        LDA     DIVIDEND+3              ;get low byte of dividend
        ADC     #0                      ;add carry to low bit of dividend
        STA     DIVIDEND+3              ;store low byte of dividend
        BRA     DECRMT                  ;do next shift and subtract

SETLSB  BSET    0,DIVIDEND+3            ;set lsb of quotient to indicate
                                        ;successive subtraction
DECRMT  DBNZ    3,SP,SHFTLP             ;decrement loop counter and do next
                                        ;shift
*
*     Move 32-bit dividend into INTACC1.....INTACC1+3 and put 16-bit
*     remainder in INTACC2:INTACC2+1
*
        LDA     REMAINDER               ;get remainder msb
        STA     1,SP                    ;temporarily store remainder msb
        LDA     REMAINDER+1             ;get remainder lsb
        STA     2,SP                    ;temporarily store remainder lsb
        MOV     DIVIDEND,QUOTIENT       ;
        MOV     DIVIDEND+1,QUOTIENT+1   ;shift all four bytes of quotient
        MOV     DIVIDEND+2,QUOTIENT+2   ; 16 bits to the left
        MOV     DIVIDEND+3,QUOTIENT+3   ;
        LDA     1,SP                    ;get final remainder msb
        STA     INTACC2                 ;store final remainder msb
        LDA     2,SP                    ;get final remainder lsb
        STA     INTACC2+1               ;store final remainder lsb
*
*     Deallocate local storage, restore register values, and return from
*     subroutine
*
        AIS     #3                      ;deallocate temporary storage
        PULX                            ;restore x-reg value
        PULA                            ;restore accumulator value
        PULH                            ;restore h-reg value
        RTS                             ;return
********************************************************************************



********************************************************************************
*
* Unsigned 32 x 32 Multiply
*
* This routine multiplies the unsigned 32-bit number stored in locations
* INTACC1:INTACC1+3 by the unsigned 32-bit number stored in locations
* INTACC2:INTACC2+3 and places the unsigned 64-bit result in locations
* INTACC1:INTACC2+3 (INTACCC1 = MSB:INTACC2+3 = LSB).
*
********************************************************************************
UMULT32: EQU *
        PSHA                    ;save acc
        PSHX                    ;save x-reg
        PSHH                    ;save h-reg
        CLRX                    ;zero x-reg
        CLRA                    ;zero accumulator
        AIS #-35T               ;reserve 35 bytes of temporary storage
                                ;on stack
        TSX                     ;transfer stack pointer + 1 to H:X
        AIX #32T                ;add number of bytes in storage table
        STHX SPVAL              ;save end of storage table value
        AIX #-32T               ;reset H:X to stack pointer value


* Clear 32 bytes of storage needed to hold the intermediate results


INIT:   CLR ,X             ;xero a byte of storage
        INCX                    ;point to next location
        CPHX SPVAL              ;check for end of table
        BNE INIT                ;

*
* Initialize multiplicand and multiplier byte position pointers,
* temporary storage for carry from the multiplication process, and
* intermediate storage location pointer
*

        STA 35T,SP              ;zero storage for multiplication carry
        LDA #3                  ;load acc w/ 1st byte position
        STA 33T,SP              ;pointer for multiplicand byte
        STA 34T,SP              ;pointer for multiplier byte
        TSX                     ;transfer stack pointer + 1 to H:X
        AIX #7                  ;position of 1st column in storage
        STHX SPVAL              ;pointer to interm. storage position
        CLRH                    ;clear h-reg

*
* Multiply each byte of the multiplicand by each byte of the multiplier
* and store the intermediate results
*

MULTLP: LDX 33T,SP       ;load x-reg w/multiplicand byte pointer
        LDA INTACC2,X           ;load acc with multiplicand
        LDX 34T,SP              ;load x-reg w/ multiplier byte pointer
        LDX INTACC1,X           ;load x-reg w/ multiplier
        MUL                     ;multiply
        ADD 35T,SP              ;add carry from previous multiply
        BCC NOINC32             ;check for carry from addition
        INCX                    ;increment result MSB
NOINC32: STX 35T,SP      ;move result MSB to carry
        LDHX SPVAL              ;load x-reg w/ storage position pointer
        STA ,X                  ;store intermediate value
        AIX #-1                 ;decrement storage pointer
        STHX SPVAL              ;store new pointer value
        CLRH                    ;clear h-reg
        DEC 34T,SP              ;decrement multiplier pointer
        BPL MULTLP              ;multiply all four bytes of multiplier
                                ;by one byte of the multiplicand
        LDHX SPVAL              ;load x-reg w/ storage position pointer
        LDA 35T,SP              ;load acc w/ carry (MSB from last mult)
        STA ,X                  ;store MSB of intermediate result
        AIX #!11                ;add offset for next intermediate
                                ;result starting position
        STHX SPVAL              ;store new value
        CLRH                    ;clear h-reg
        CLR 35T,SP              ;clear carry storage
        LDX #3                  ;
        STX 34T,SP              ;reset multiplier pointer
        DEC 33T,SP              ;point to next multiplicand
        BPL MULTLP              ;loop until each multiplicand has been
                                ;multiplied by each multiplier

*
* Initialize temporary stack variables used in the addition process
*

        TSX                     ;transfer stack pointer to H:X
        AIX #7                  ;add offset for LSB of result
        STHX SPVAL              ;store position of LSB
        CLR 35T,SP              ;clear addition carry storage
        LDA #7                  ;
        STA 33T,SP              ;store LSB position of final result
        LDA #3                  ;
        STA 34T,SP              ;store counter for number of rows

*
* add all four of the entries in each column together and store the
* final 64-bit value in locations INTACC1:INTACC2+3.
*

OUTADDLP LDA 35T,SP     ;load acc with carry
        CLR 35T,SP              ;clear carry
INADDLP ADD ,X          ;add entry in table to accumulator
        BCC ADDFIN              ;check for carry
        INC 35T,SP              ;increment carry
ADDFIN AIX #8           ;load H:X with position of next entry
                                ;column

        DEC 34T,SP              ;decrement row counter
        BPL INADDLP             ;loop until all four entries in column
                                ;have been added together
        CLRH                    ;clear h-reg
        LDX #3                  ;
        STX 34T,SP              ;reset row pointer
        LDX 33T,SP              ;load final result byte pointer
        STA INTACC1,X           ;store one byte of final result
        LDHX SPVAL              ;load original column pointer
        AIX #-1                 ;decrement column pointer
        STHX SPVAL              ;store new pointer value
        DEC 33T,SP              ;decrement final result byte pointer
        BPL OUTADDLP            ;loop until all eight columns have
                                ;been added up and the final results
                                ;stored
*
* Reset stack pointer and recover original registers values
*
        AIS #35T                ;deallocate local storage
        PULH                    ;restore h-reg
        PULX                    ;restore x-reg
        PULA                    ;restore accumulator
        RTS                     ;return
*************************************************************




**************************************************************
* Write to Memory - this function writes a byte to memory    *
*                   assumes the byte stored in byte_write    *
**************************************************************
push_memory:
       psha
       pshx
       pshh

       brset 0,DDRB,skip_setout1
       mov   byte_write,PTB     ; put data onto latch inputs
       mov   #$FF,DDRB          ; set port B as output

skip_setout1:

       bclr  3,PTA              ; enable latchIn
       jsr delay500ns
       bset  2,PTA              ; write to data_in latch
       jsr delay500ns
       bclr  2,PTA              ; disable writing to data_in latch

*****  AT THIS POINT LATCH_IN CONTAINS DATA TO BE WRITTEN ******
       mov  memory_add,PTC     ; write memory address to memory
       mov  memory_add+1,PTB   ; chip address lines

       bclr 1,PTD               ; set memory CS_N to 0 to select chip
       bclr 6,PTA               ; set memory WE_N to 0 to allow writes
       jsr delay
       bset 6,PTA               ; disallow writes
       bset 1,PTD               ; deselect chip

       bset 3,PTA               ; disable latch_in

       ldhx     memory_add      ; increment memory_add pointer
       aix      #1
       sthx     memory_add

       pulh
       pulx
       pula

       rts

**************************************************************
* Read from Memory - this function reads a byte from memory  *
*                    requires that "memory_add" contain the  *
*                    correct address before calling routine  *
**************************************************************
read_memory:

       psha
       pshx
       pshh

       mov   memory_add,PTC     ; write memory address to memory

       brset 0,DDRB,skip_setout2

       mov  memory_add+1,PTB   ; chip address lines
       mov   #$FF,DDRB          ; set port B as output

skip_setout2:
       jsr delay500ns

       bclr 1,PTD               ; set memory CS_N to 0 to select chip
       jsr delay500ns

       bclr 2,PTD               ; set memory OE_N to 0 to enable output
       jsr delay500ns


       bset  4,PTA              ; write to data_out latch
       jsr delay500ns
       bclr  4,PTA              ; disable writing to data_out latch

       bset 2,PTD               ; disable OE_N to disallow memory output
       bset 1,PTD               ; disable CS_N to deselect memory chip

       brclr 0,DDRB,skip_setin1
       mov   #$00,DDRB          ; set port B as input

skip_setin1:

       bclr  5,PTA              ; enable LatchOut
       mov   PTB,byte_read       ; grab the byte
       bset  5,PTA              ; disable LatchOut

       ldhx     memory_add      ; increment memory_add pointer
       aix      #-1
       sthx     memory_add

       pulh
       pulx
       pula
       rts


********************************************************************************
*     Unsigned 16x16 multiply
*
*     This routine multiplies the 16-bit unsigned number stored in
*     locations INTACC1:INTACC1+1 by the 16-bit unsigned number stored in
*     locations INTACC2:INTACC2+1 and places the 32-bit result in locations
*     INTACC1....INTACC1+3 (INTACC1 = MSB.....INTACC1+3 = LSB).
*
********************************************************************************
UMULT16     EQU     *
            PSHA                        ;save acc
            PSHX                        ;save x-reg
            PSHH                        ;save h-reg
            AIS     #-6                 ;reserve six bytes of temporary
                                        ;storage on stack
            CLR     6,SP                ;zero storage for multiplication carry
*
*     Multiply (INTACC1:INTACC1+1) by INTACC2+1
*
            LDX     INTACC1+1           ;load x-reg w/multiplier lsb
            LDA     INTACC2+1           ;load acc w/multiplicand lsb
            MUL                         ;multiply
            STX     6,SP                ;save carry from multiply
            STA     INTACC1+3           ;store lsb of final result
            LDX     INTACC1             ;load x-reg w/multiplier msb
            LDA     INTACC2+1           ;load acc w/multiplicand lsb
            MUL                         ;multiply
            ADD     6,SP                ;add carry from previous multiply
            STA     2,SP                ;store 2nd byte of interm. result 1.
            BCC     NOINCA              ;check for carry from addition
            INCX                        ;increment msb of interm. result 1.
NOINCA      STX     1,SP                ;store msb of interm. result 1.
            CLR     6,SP                ;clear storage for carry
*
*     Multiply (INTACC1:INTACC1+1) by INTACC2
*
            LDX     INTACC1+1           ;load x-reg w/multiplier lsb
            LDA     INTACC2             ;load acc w/multiplicand msb
            MUL                         ;multiply
            STX     6,SP                ;save carry from multiply
            STA     5,SP                ;store lsb of interm. result 2.
            LDX     INTACC1             ;load x-reg w/multiplier msb
            LDA     INTACC2             ;load acc w/multiplicand msb
            MUL                         ;multiply
            ADD     6,SP                ;add carry from previous multiply
            STA     4,SP                ;store 2nd byte of interm. result 2.
            BCC     NOINCB              ;check for carry from addition
            INCX                        ;increment msb of interm. result 2.
NOINCB      STX     3,SP                ;store msb of interm. result 2.
*
*     Add the intermediate results and store the remaining three bytes of the
*     final value in locations INTACC1....INTACC1+2.
*
            LDA     2,SP                ;load acc with 2nd byte of 1st result
            ADD     5,SP                ;add acc with lsb of 2nd result
            STA     INTACC1+2           ;store 2nd byte of final result
            LDA     1,SP                ;load acc with msb of 1st result
            ADC     4,SP                ;add w/ carry 2nd byte of 2nd result
            STA     INTACC1+1           ;store 3rd byte of final result
            LDA     3,SP                ;load acc with msb from 2nd result
            ADC     #0                  ;add any carry from previous addition
            STA     INTACC1             ;store msb of final result
*
*     Reset stack pointer and recover original register values
*
            AIS     #6                  ;deallocate the six bytes of local
                                        ;storage
            PULH                        ;restore h-reg
            PULX                        ;restore x-reg
            PULA                        ;restore accumulator
            RTS                         ;return
********************************************************************************




    org ExtraStuff
**************************************************************
* Display Stack -                                            *
**************************************************************
display_stack:
       ldhx   #$0230
next_stack_val:
       lda    ,X
       nsa
       jsr    conv_hex2ascii
       jsr    LCD_write_data
       lda    ,X
       jsr    conv_hex2ascii
       jsr    LCD_write_data
       aix    #1
       txa
       sub    #$38
       beq    end_stack_disp
       add    #4
       beq    stack_newline
       bra    next_stack_val
stack_newline:
       jsr    LCD_line2
       bra    next_stack_val
end_stack_disp:
       jsr    LCD_homecursor

       lda      #$02                   ; wait 2 Sec before next look
       jsr      pause_1sec

       rts




**************************************************************
* Store History -                                            *
*      Hardware stack last 8 values  $0230 current           *
*                                    $0231 most recent       *
*                                    $0238 most oldest       *
**************************************************************
store_history:
       ldhx   samplestack
       txa
       sub    #$2F
       bne    push_value
       aix    #9
       lda    ,X
       sta    sample_overflow           ; popped off sample overflow
                                        ; will go to offchip memory someday
       aix    #-1
shift_stack:
       lda    ,X
       aix    #1
       sta    ,X
       aix    #-2
       txa
       sub    #$2F
       bne    shift_stack
       aix    #1
       lda    kilohertz
       add    reference_value
       sta    ,X
       aix    #-1
       sthx   samplestack
       rts
push_value:
       lda    kilohertz
       add    reference_value
       sta    ,X
       aix    #-1
       sthx   samplestack
       rts


print_five:
        lda     ten4th
        add     #$30
        jsr     LCD_write_data
        lda     ten3rd
        add     #$30
        jsr     LCD_write_data
        lda     hundreds
        add     #$30
        jsr     LCD_write_data
        lda     tens
        add     #$30
        jsr     LCD_write_data
        lda     ones
        add     #$30
        jsr     LCD_write_data
        rts


