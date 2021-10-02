
$Include 'gpgtregs.inc'



RAMStart     EQU  $0040
SampleStart  EQU  $0230
exp_table_p  EQU  $8000
exp_table_n  EQU  $8100
Text         EQU  $8200
MathRoutines EQU  $8800
ExtraStuff   EQU  $8D00
GasValues    EQU  $9000
SmoothTable  EQU  $9200
SmoothFunx   EQU  $9400
ROMStart     EQU  $D000
VectorStart  EQU  $FFDC




    org RAMStart

pulse_count      ds 2
pulse_xtra       ds 1
pulse_timer      ds 1
count_looks      ds 1
pause1ms         ds 1
pause1sec        ds 1
keybd_flag       ds 1
gas_flag         ds 1
timer_flag       ds 1
gas_text         ds 2
gas_number       ds 1
gas_index        ds 2

total_time       ds 2
top_time         ds 2
time_flag        ds 1

rs_line          ds 1

cali_counter     ds 1
cali_pointer     ds 2
cali_values      ds 4
reference_value  ds 1

text_address     ds 2

samplestack      ds 2
sample_overflow  ds 1

calc_temp        ds 1
LCD_counter      ds 1
hist_counter     ds 1

INTACC1          ds 4            ;32-bit integer accumulator #1
INTACC2          ds 4            ;32-bit integer accumulator #2

exp_index        ds 1
dose_value       ds 2
old_dose_value   ds 2
conc_value       ds 2
detect           ds 1

SPVAL            ds 2            ;storage for stack pointer value

kilohertz        ds 1
hertz            ds 1
ones             ds 1     ; count of how many ones are in a hex number
tens             ds 1     ; count of how many tens are in a hex number
hundreds         ds 1     ; count of how many hundreds are in a hex number
ten3rd           ds 1
ten4th           ds 1

memory_add   ds 2
byte_write   ds 1
byte_read    ds 1
double_byte  ds 2
top_add      ds 2
memory_flag  ds 1

sum_terms        ds 4
sample_temp      ds 1
smooth_row       ds 2
approx_temp      ds 1
standard_dev     ds 1
std_temp         ds 2
std_sum          ds 4
std_samp         ds 1
std_approx       ds 1
check_temp       ds 4

approx0          ds 1
approx1          ds 1
approx2          ds 1
approx3          ds 1
approx4          ds 1
approx5          ds 1
approx6          ds 1
approx7          ds 1
approx8          ds 1



    org SampleStart
samp8      ds 1
samp7      ds 1
samp6      ds 1
samp5      ds 1
samp4      ds 1
samp3      ds 1
samp2      ds 1
samp1      ds 1
samp0      ds 1


$Include 'std_dev.asm'



    org RomStart

*************************************************************
* LATCH FUNCTIONS
*                           left          right
*  OE  E   Dx Qx        LatchOUT(U5)   LatchIN(U6)
*
*  0   1   1  1           OE   A5        OE  A3
*  0   1   0  0           E    A4        E   A2
*  0   0   X  Qx'         Q0   B0        D0  B0
*  1   X   X  HiZ         ..   ..        ..  ..
*                         Q7   B7        D7  B7
*
* MEMORY FUNCTIONS
*						    Mem   908
*  CS  OE  WE                            CS   D1
*  0   0   1    read data from Addr      OE   D2
*  0   1   0    write data to Addr       WE   A6
*  1   1   1    no op.  HiZ output
*
*  initial state
*  latchIN/OUT OE = 1                           intial state
*  latchIN/OUT E  = 0
*  mem[CS:OE:WE]  = 111

*  read data
*  set 15-bit address into PTC[6:0]:PTB[7:0]    get memory contents
*  mem[CS:OE:WE]  = 001
*  latchOUT E     = 1                           write memory into latch
*  latchOUT E     = 0
*  mem[CS:OE:WE]  = 111                         turn off memory
*  latchOUT OE    = 0                           enable LatchOut
*  read data at PTB
*  latchOUT OE    = 1                           turn off latch

*  write data
*  set data at PTB
*  latchIN OE     = 0
*  latchIN E      = 1                           write PTB into LatchIn
*  latchIN E      = 0
*  set 15-bit address into PTC[6:0]:PTB[7:0]    enable memory address
*  mem[CS:OE:WE]  = 010                         write
*  mem[CS:OE:WE]  = 111                         turn off memory
*  latchOUT OE    = 1                           turn off latch
*
*************************************************************
init_mem:

       bset  1,PTD              ; mem chip select - off
       bset  2,PTD              ; mem output enable - off
       bset  6,PTA              ; mem write enable - off
       bset  3,PTA              ; disable output of latchIn
       bclr  2,PTA              ; disable writing to latchIn
       bset  5,PTA              ; disable output of LatchOut
       bclr  4,PTA              ; disable writing to latchOut
       clr  memory_add
       mov  #$20,memory_add+1   ; start memory address 20 bytes forward.
       rts

**************************************************************
* Init_IRQ -  Initialize the IRQ Register                    *
**************************************************************
Init_IRQ:
       clr   INTSCR               ; damn, that was easy
       rts

**************************************************************
* Init_SCI -  Initialize the SCI Module                      *
**************************************************************
Init_SCI:

       bset     4,SCBR  ; SCBR = 11 011   9600 Baud (8 MHz Bus)
       bset     5,SCBR  ;        PD = 13
                        ;        BR = 0

       bset     6,SCC1  ; ENable SCI and baud rate generator
       ;bset     3,SCC2  ; enable sci REceiver
       rts


**************************************************************
* Init_PLL -  Initializes the PLL Registers                  *
*             for 8.00 MHz bus frequency                     *
*             and corresponding period of 0.125 us           *
**************************************************************
Init_PLL:
         bclr   5,PCTL ; Disable PLL (enable access to all PLL registers)
         mov    #$02,PCTL ; P (PRE) = 0 (Prescaler=1), E (VPR) = 2 (2^E = 4)
         mov    #$80,PBWC ; Automatic bandwidth control
         mov    #$03,PMSH ; Upper byte of $3D1 = PLL multiplier (N)
         mov    #$D1,PMSL ; Lower byte of $3D1 = PLL multiplier
         mov    #$D0,PMRS ; VCO range select (L) = $D0
         mov    #$01,PMDS ; PLL reference divider (R) = 1
         bset   5,PCTL ; Enable PLL
wait_here:
         nop
         brclr  6,PBWC,wait_here ; wait until PLL stabilizes
         bset   4,PCTL ; switch clock source to PLL
         rts

*****************************************************************
* Init_Timer11 -  Pulse Width Modulation   4 kHz   T2 vCH1
* clear bit 5 of T2SC to play, set bit 5 of T2SC to stop        *
*****************************************************************
Init_Timer11:
       mov    #$30,T2SC
       mov    #$07,T2MODH
       mov    #$D0,T2MODL
       clr         T2CH1H
       mov    #$80,T2CH1L
       mov    #$16,T2SC1
       ;bclr   5,T2SC
       rts


**************************************************************
* Init_Timer - Turns on timer 1 channel 0 for Input Compare  *
*                                                            *
**************************************************************
Init_Timer:

       lda    #$01           ; Kill the dog now
       ora    CONFIG1
       sta    CONFIG1

       lda    #$01
       ora    CONFIG2        ; obscurity rules
       sta    CONFIG2

       mov    #$34,T1SC      ; stop, reset, /16
       clr    T1CH0H         ; clear TIM1 Ch0
       clr    T1CH0L
       mov    #$F4,T1MODH    ; set TIM interrupt at xF424 "tics"
       mov    #$24,T1MODL
       mov    #$04,T1SC0     ; input capture, rising edge
       rts

**************************************************************
* Init_Keybd -  Initializes the Keyboard Interrupt Registers *
*               Port A0 thru A7 are Keyboard port pins       *
**************************************************************
Init_Keybd:
       lda      SBFCR              ; allows software to clear interrupt latches
       ora      #$80               ; although this option is not being used
       sta      SBFCR              ;
       mov      #$03,INTKBSCR      ; enable IMASKK, disable MODEK: falling edge and low
       mov      #$03,INTKBIER      ; enable Ports A1, A0 (pins 34, 33)
       bset     2,INTKBSCR         ; enable ACKK to clear false interrupts
       rts

**************************************************************
* Init_Regs -                                                *
*                                                            *
**************************************************************
Init_Registers:

       clra
       clrx
       clr      pulse_count
       clr      pulse_count+1
       clr      pulse_xtra
       clr      timer_flag
       clr      keybd_flag
       clr      gas_flag
       clr      rs_line
       clr      time_flag

       clr      total_time
       clr      total_time+1

       clr      detect

       clr      INTACC1
       clr      INTACC1+1
       clr      INTACC1+2
       clr      INTACC1+3
       clr      INTACC2
       clr      INTACC2+1
       clr      INTACC2+2
       clr      INTACC2+3

       clr      dose_value
       clr      dose_value+1

       clr      exp_index

       ldhx     #$0238
       sthx     samplestack
       ldhx     #$0230
clear_stack:
       clr      ,X
       aix      #1
       txa
       sub      #$39
       bne      clear_stack

       clr   PTC           ; clear ports
       clr   PTB
       mov   #$FF,DDRB     ; set ports B, C as output
       mov   #$FF,DDRC     ;
       mov   #$FC,DDRA     ; set ports A1, A0, E1 as input
       mov   #$01,DDRE
       mov   #$2F,DDRD     ; set D[4] as input, rest output


       mov      #$00,PTAPUE   ; set ports A pullup resistors

       bclr    0,PTD          ; kill LED

       rts

**************************************************************
* Init_LCD -                                                 *
*                                                            *
**************************************************************
Init_LCD:
       lda    #1
       jsr    pause_1sec
                                ; D3 = Enable
       bclr   7,PTA             ; A7 = R/~W
       bclr   4,PTC             ; C4 = RS
                                ; C3 = DB7
       lda    #$03              ; C0 = DB4
       jsr    LCD_write_command
       lda    #$03
       jsr    LCD_write_command
       lda    #$03
       jsr    LCD_write_command
       lda    #$02              ; 4 bit transmission
       jsr    LCD_write_command
       lda    #$28              ; two-line display
       jsr    LCD_write_command
       lda    #$0c              ; display on , cursor off, blink off
       jsr    LCD_write_command
       lda    #$06              ; direction increment, no shift
       jsr    LCD_write_command

       rts

**************************************************************
* Main_Init - This is the point where code starts executing  *
*             after a RESET.                                 *
**************************************************************
Main_Init:
       rsp
       ldhx     #$01FF
       txs

       bset     5,T2SC ;*ALARM OFF*

       jsr      Init_Timer
       ;jsr      Init_Timer11
       jsr      Init_Keybd
       jsr      Init_Irq
       jsr      Init_SCI
       jsr      Init_Registers
       jsr      Init_Mem
       jsr      Init_PLL
       jsr      Init_LCD
       jsr      LCD_clear_display
       cli                              ; clear global interrupt mask
main_loop:
       bclr    1,INTKBSCR              ; disable IMASKK to enable KBI
       jsr     prompt_gas
       jsr     calibrate
       bset    1,INTKBSCR               ; enable IMASKK to disable KBI
main2:
       bset     5,T2SC ;*ALARM OFF*
       jsr     look_at_card
       jsr     start_pulse_timer
       jsr     avg_pulses
       jsr     pulse_to_freq
       jsr     store_history

       jsr     smooth_points
       jsr     compute_std

       jsr     look_detect
       jsr     calculate_dose
       jsr     calculate_concentration
       jsr     output_results


       jsr     write_memory

main3:
       bclr     1,INTKBSCR              ; disable IMASKK to enable KBI
       brset   2,keybd_flag,get_results
       brclr   7,timer_flag,main2
       bra     main3
get_results:
       jsr     download_history
       clr     keybd_flag
       bra     main2


look_detect:
       jsr     LCD_clear_display
       tst     detect
       bge     no_detect_alarm
       bclr     5,T2SC ;*ALARM ON*
no_detect_alarm:
       lda       detect
       jsr       conv_h2a
       lda       tens              ; Display the tens character
       jsr       LCD_write_data
       lda       ones              ; Display the ones character
       jsr       LCD_write_data
       lda     #$FA
       jsr     pause_ms
       lda     #$FA
       jsr     pause_ms
       rts



**************************************************************
*  read from memory
*                      reset "memory_add"
**************************************************************

download_history:
       clr     memory_flag

       bset    3,SCC2                   ; transmit enable

       lda     memory_add
       sta     top_add
       lda     memory_add+1
       sta     top_add+1

       jsr     LCD_clear_display
       ldhx    #prompt_memory
       jsr     LCD_print
       jsr     acknowledge
       jsr     LCD_clear_display
       ldhx    #loading
       jsr     LCD_print
       bset    1,INTKBSCR               ; enable IMASKK to disable KBI



sixteenbytes:
       lda     memory_add
       ora     memory_add+1
       bne     not_addr_zero
       mov     #$01,memory_flag

       lda     #$41
       jsr     send_byte
       jsr     send_byte
       jsr     send_byte
       jsr     send_byte
       lda     #$5A
       jsr     send_byte
       jsr     send_byte
       jsr     send_byte
       jsr     send_byte




not_addr_zero:
       jsr     read_memory
       lda     byte_read
       jsr     send_byte

       bra     sixteenbytes
memory_read_done:

       rts




;store_hex_byte
;       psha
;       jsr     conv_h2a
;       lda     tens
; ;      sta     byte_write
;       jsr     push_memory
 ;      lda     ones
;       sta     byte_write
 ;      jsr     push_memory
 ;      pula
;       rts



**************************************************************
*  send byte
*               byte to be sent in A register
**************************************************************
send_byte:
      nop
      brclr     6,SCS1,send_byte
      sta       SCDR
      rts


**************************************************************
*  write_values to memory
*               only if within first 16 "looks" or if
*               exposure data has changed
**************************************************************

write_memory:
       psha
       pshx
       pshh

       lda    total_time
       bne    over16
       lda    total_time+1
       sub    #13
       blo    write_values
over16:
       lda    dose_value
       sub    old_dose_value
       bne    write_values
       lda    dose_value+1
       sub    old_dose_value+1
       beq    write_done
write_values:

       mov    #8,hist_counter
       ldhx   #SampleStart
copy_stack:
       mov    X+,byte_write
       jsr    push_memory
       dbnz   hist_counter,copy_stack

       mov    detect,byte_write
       jsr    push_memory

       mov    conc_value+1,byte_write
       jsr    push_memory
       mov    conc_value,byte_write
       jsr    push_memory
       mov    dose_value+1,byte_write
       jsr    push_memory
       mov    dose_value,byte_write
       jsr    push_memory
       mov    total_time+1,byte_write
       jsr    push_memory
       mov    total_time,byte_write
       jsr    push_memory



write_done:
       pulh
       pulx
       pula
       rts



**************************************************************
*  start pulse timer --  one pulse every 5 seconds
*                        'total_time' 5 second increments
**************************************************************

start_pulse_timer:
       mov     #$04,T1SC0         ; t1ch0 input capture disable
       mov     #$1E,pulse_timer   ; 30*0.125 + 1.25 = 5 seconds
       bset    7,timer_flag
       bclr    0,timer_flag
       bset    4,T1SC             ; clear timer counter (and prescaler)
       mov     #$44,T1SC          ; start tim1 /16 and overflow enable

       ldhx    total_time
       aix     #1
       sthx    total_time

       rts




**************************************************************
*  calibrate --
**************************************************************
calibrate:
       jsr      LCD_clear_display

       clr      cali_values
       clr      cali_values+1
       clr      cali_values+2
       clr      cali_values+3


       ldhx     #prompt_cali
       jsr      LCD_print
       jsr      LCD_line2
       lda      #6
       jsr      LCD_shift_cursor

       lda      gas_number        ; Convert Hex Ref. Value to Decimal
       jsr      conv_hex2dec
       lda      tens              ; Display the tens character
       add      #$30              ;
       jsr      LCD_write_data
       lda      ones              ; Display the ones character
       add      #$30              ;
       jsr      LCD_write_data
       jsr      LCD_homecursor

       lda      #$01
       jsr      pause_1sec

       ldhx     gas_text
       jsr      LCD_print
       lda      #$01
       jsr      pause_1sec

       ldhx     #prompt_enter
       jsr      LCD_print
       bclr     2,keybd_flag
       jsr      acknowledge
       ldhx     #please_wait
       jsr      LCD_print
       lda      #$01
       jsr      pause_1sec
       jsr      LCD_clear_display

       ldhx     #cali_pass
       sthx     cali_pointer
       clr      cali_counter
read_calibration:
       ldhx     cali_pointer
       jsr      LCD_oneline
       jsr      look_at_card
       jsr      avg_pulses
       jsr      pulse_to_freq
       jsr      LCD_line2
       lda      cali_counter
       lsla
       jsr      LCD_shift_cursor
       ldhx     #cali_values
       txa
       add      cali_counter
       tax
       lda      kilohertz
       sta      ,X
       brset    2,keybd_flag,calibrate_good

       nsa
       jsr      conv_hex2ascii
       jsr      LCD_write_data
       lda      ,X
       jsr      conv_hex2ascii
       jsr      LCD_write_data
       jsr      LCD_homecursor

       lda      #2
       jsr      pause_1sec

       lda      ,X
       and      #$F0
       ora      #$3F
       add      #$01
       bne      bad_card

       lda      cali_values
       sub      ,X
       bne      bad_calibrate

       ldhx     cali_pointer
       aix      #$09
       sthx     cali_pointer

       inc      cali_counter
       lda      cali_counter
       sub      #$04
       beq      calibrate_good
       bra      read_calibration

calibrate_good:
       bclr     2,keybd_flag
       ldhx     #cali_good
       jsr      LCD_print
       lda      #$F0
       sub      cali_values
       sta      reference_value
       nsa
       jsr      conv_hex2ascii
       jsr      LCD_write_data
       lda      reference_value
       jsr      conv_hex2ascii
       jsr      LCD_write_data

       lda      #2
       jsr      pause_1sec
       jsr      LCD_clear_display
       ldhx     #begin_monitor
       jsr      LCD_print

       jsr      acknowledge
       jsr      LCD_clear_display
       rts

bad_card:
       ldhx     #print_badcard
       jsr      LCD_print
       lda      #2
       jsr      pause_1sec
       ldhx     #prompt_enter
       jsr      LCD_print
       jmp      calibrate

bad_calibrate:
       ldhx     #print_badcali
       jsr      LCD_print
       lda      #2
       jsr      pause_1sec
       ldhx     #prompt_enter
       jsr      LCD_print
       jmp      calibrate

skip_cali:
       rts



**************************************************************
*  acknowledge -- waits for 'enter' button to be pressed
**************************************************************
acknowledge:
       clr      keybd_flag
wait_for_ack:
       nop
       brset    2,keybd_flag,double_press
       brset    1,keybd_flag,acknowledge
       brclr    0,keybd_flag,wait_for_ack
double_press:
       rts


**************************************************************
*  prompt gas -- chooses which gas to look for
**************************************************************
prompt_gas:
       jsr      LCD_clear_display
       ldhx     #prompt_text
       jsr      LCD_print
       clr      gas_flag
       clr      keybd_flag
       clr      gas_number
wait_1st_press:
       nop
       brset    0,keybd_flag,w1p_wrong
       brset    1,keybd_flag,got_1st_press
       bra      wait_1st_press
w1p_wrong:
       clr      keybd_flag
       bra      wait_1st_press
got_1st_press
       ldhx     #gas_names                ; first gas name at $8210
       sthx     gas_text
       jsr      LCD_print
       clr      keybd_flag
wait_next_press:
       tst      keybd_flag
       beq      wait_next_press
       brset    1,keybd_flag,select_button
       brset    0,keybd_flag,enter_button
       bra      wait_next_press
select_button:
       ldhx     gas_text
       aix      #$10                    ; successive gas names at $0010 increments
       sthx      gas_text
       inc      gas_number
       lda      gas_number
       sub      #$12
       bne      no_rollover            ; last gas name at $8330
       clr      gas_number
       ldhx     #gas_names
       sthx     gas_text
no_rollover:
       jsr      LCD_print
       clr      keybd_flag
       bra      wait_next_press
enter_button:
       clr      keybd_flag
       clra
       rts

**************************************************************
*  output_results --
**************************************************************
output_results:
       ;ldhx    #units_text
       ;aix     #$10
       ;sthx    text_address
       ;jsr     display_freq

       ;lda     #$01
       ;jsr     pause_1sec

       jsr     LCD_clear_display
       ldhx    #units_text
       jsr     LCD_oneline

       ;jsr     LCD_line2
       ;ldhx    #units_text
       ;aix     #$08
       ;jsr     LCD_oneline

       jsr     display_dose

       ;lda     #$02
       ;jsr     pause_1sec
       ;jsr     LCD_clear_display
       ;ldhx    #units_text
       ;aix     #$10
       ;jsr     LCD_oneline

       jsr     LCD_line2
       ldhx    #units_text
       aix     #$10
       jsr     LCD_oneline
       jsr     LCD_line2
       jsr     display_conc

       ;lda     #$02
       ;jsr     pause_1sec

       rts


***********************************************************
* Displays the conc_value in dec
*
***********************************************************
display_conc:

        lda     #2
        jsr     LCD_shift_cursor

        ldhx    conc_value
        jsr     conv_16bitH2D

        lda     ten4th
        beq     conc_ten3
        add     #$30
        jsr     LCD_write_data
conc_ten3:
        lda     ten3rd
        beq     conc_ten2
        add     #$30
        jsr     LCD_write_data
conc_ten2:
        lda     hundreds
        beq     conc_ten1
        add     #$30
        jsr     LCD_write_data
conc_ten1:
        lda     tens
        add     #$30
        jsr     LCD_write_data
        lda     #$2E
        jsr     LCD_write_data
        lda     ones
        add     #$30
        jsr     LCD_write_data

        lda     #$20
        jsr     LCD_write_data
        jsr     LCD_write_data
        jsr     LCD_write_data

        rts



***********************************************************
* Displays the dose_value in dec
*
***********************************************************
display_dose:

        lda     #2
        jsr     LCD_shift_cursor


        ldhx    dose_value
        jsr     conv_16bitH2D

        lda     ten4th
        beq     dose_ten3
        add     #$30
        jsr     LCD_write_data
dose_ten3:
        lda     ten3rd
        add     #$30
        jsr     LCD_write_data

        lda     #$2E
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

        lda     #$20
        jsr     LCD_write_data

        rts


************************************************************
* Compute Dose
*
* This routine computes the dose for a gas
* given a one byte frequency count stored in the accumulator
* and gas_number which indexes to the gas_constants table
*
* Dose = 2^(dose_a + dose_b*freq + dose_c*freq^2)
*
* This function will create an index stored in gas_index
* gas_index + 0 = xxxxx000:   dose_a = +dose_a
*                             dose_b = +dose_b
*                             dose_c = +dose_c
* gas_index + 0 = xxxxx001:   dose_a = +dose_a
*                             dose_b = +dose_b
*                             dose_c = -dose_c
* etc.
*
* gas_index + 4 = dose_a
* gas_index + 8 = dose_b
* gas_index + C = dose_c
*
*************************************************************
calculate_dose:
        clr      INTACC1                   ; CLEAR APPROPRIATE VARIABLES
        clr      INTACC1+1
        clr      INTACC1+2
        clr      INTACC1+3
        clr      INTACC2
        clr      INTACC2+1
        clr      INTACC2+2
        clr      INTACC2+3
        mov      dose_value,old_dose_value
        mov      dose_value+1,old_dose_value+1
        clr      dose_value
        clr      dose_value+1
        clr      exp_index

************** LOAD gas_index ****************
        ldhx     #gas_constants              ; H:X = $9000
        lda      gas_number                  ; gas0 @ $9000, gas1 @ $9010, gas2 @ $9020, etc...
find_gas_index:
        beq      found_gas_index
        aix      #$10                        ; increment H:X by address jumps of $0010
        sub      #1
        bra      find_gas_index              ;

found_gas_index:
        sthx     gas_index                   ; H:X now equals address of control byte, a, b, c

************ COMPUTE THIRD TERM ***************
        lda      kilohertz
        add      reference_value             ; A,X = kilohertz + calibration reference
        tax
        mul                                  ; [x:a] = freq^2

        sta      INTACC1+3
        stx      INTACC1+2                   ; INTACC1 = freq^2
        clr      INTACC1+1
        clr      INTACC1

        ldhx     gas_index
        aix      #$0C
        mov      X+,INTACC2
        mov      X+,INTACC2+1
        mov      X+,INTACC2+2
        mov      X+,INTACC2+3                ; INTACC2 = dose_c

        jsr      UMULT32                     ; INTACC1:INTACC2 = dose_c*freq^2

        ldhx     gas_index
        lda      ,X                          ; load the control byte
        sta      calc_temp
        brclr    0,calc_temp,skip_negate1    ; if the dose_c term is negative, intacc2 = -intacc2
        jsr      twos_comp                   ; result should be 32 bits or less

skip_negate1:
        ldhx     #INTACC2
        lda      ,X
        psha
        incx
        lda      ,X
        psha
        incx
        lda      ,X
        psha
        incx
        lda      ,X
        psha                    ; SP+4:SP+1 = (+/-)dose_c*freq^2

************* COMPUTE SECOND TERM ****************
        lda      kilohertz
        add      reference_value
        sta      INTACC1+3
        clr      INTACC1+2
        clr      INTACC1+1
        clr      INTACC1                     ; INTACC1 = freq
        ldhx     gas_index
        aix      #8
        mov      X+,INTACC2
        mov      X+,INTACC2+1
        mov      X+,INTACC2+2
        mov      X+,INTACC2+3       ; INTACC2 = dose_b

        jsr      UMULT32                     ; INTACC1:INTACC2 = freq*dose_b

        ldhx     gas_index
        lda      ,X                          ; load the control byte
        sta      calc_temp
        brclr    1,calc_temp,skip_negate2    ; if the dose_b term is negative, intacc2 = -intacc2
        jsr      twos_comp                   ; result should be 32 bits or less

skip_negate2:
        ldhx     #INTACC1+3                  ; intacc2 = B*freq
        pula                                 ; intacc1 = C*freq^2
        sta      ,X
        decx
        pula
        sta      ,X
        decx
        pula
        sta      ,X
        decx
        pula
        sta      ,X

        jsr      add_32bit                   ; intacc1:intacc2 = dose_b*freq + dose_c*freq^2

************* COMPUTE FIRST TERM ******************
        ldhx     gas_index
        aix      #4
        mov      X+,INTACC2
        mov      X+,INTACC2+1
        mov      X+,INTACC2+2
        mov      X+,INTACC2+3       ; INTACC1 = dose_a

        ldhx     gas_index
        lda      ,X                          ; load the control byte
        sta      calc_temp
        brclr    2,calc_temp,skip_negate3    ; if the dose_a term is negative, intacc2 = -intacc2
        jsr      twos_comp

skip_negate3:
        jsr      add_32bit                   ; intacc1 = dose_a + dose_b*freq + dose_c*freq^2

************* LOOK UP DOSE VALUE ******************
        jsr       lookup_value               ; dose_value = 2^intacc1
        rts



*************************************************************
* concentration
*
*************************************************************
calculate_concentration:
        clr      INTACC1
        clr      INTACC1+1
        clr      INTACC1+2
        clr      INTACC1+3
        clr      INTACC2
        clr      INTACC2+1
        clr      INTACC2+2
        clr      INTACC2+3

        mov      old_dose_value,INTACC2+2
        mov      old_dose_value+1,INTACC2+3
        jsr      twos_comp
        mov      dose_value,INTACC1+2
        mov      dose_value+1,INTACC1+3

        jsr      add_32bit
        brset    7,INTACC1,zero_conc


        clr      INTACC2
        clr      INTACC2+1
        mov      #$02,INTACC2+2
        mov      #$D0,INTACC2+3
        jsr      UMULT32

        ldhx     #INTACC1
        mov      INTACC2,X+
        mov      INTACC2+1,X+
        mov      INTACC2+2,X+
        mov      INTACC2+3,X+
        ldhx     #$0064
        sthx     INTACC2
        clr      INTACC2+2
        clr      INTACC2+3

        jsr      UDVD32

        mov      INTACC1+2,conc_value
        mov      INTACC1+3,conc_value+1
        rts
zero_conc:
        clr      conc_value
        clr      conc_value+1
        rts


*************************************************************
* Look-up y=2^intacc1
*
*************************************************************

lookup_value:
        ldhx     gas_index              ; 'gas_index' offset by 2 and 3 bytes
        aix      #2                     ; contain the bit pattern needed
        lda      ,X                     ; to extract the meaingful exponent
        and      INTACC1+1              ; bits in the 'exp_index' register
        nsa
        sta      exp_index

        incx
        lda      ,X
        and      INTACC1+2
        nsa
        ora      exp_index
        sta      exp_index

        lda      ,X                     ; byte = #$00 then 2^20
        beq      pow2_20                ; byte = #$C0 then 2^18
        add      #$40                   ; byte = #$F0 then 2^16
        beq      pow2_18
        bra      pow2_16
pow2_20:
        lda      exp_index
        nsa
        sta      exp_index
        bra      pow2_16
pow2_18:
        clc
        ror      exp_index
        ror      exp_index
        ror      exp_index
pow2_16:
        ldhx     #index_negs
        brset    7,exp_index,get_entry
        ldhx     #index_pos
get_entry:
        lda      exp_index
        lsla
        tax

        lda      ,X
        sta      dose_value
        aix      #1
        lda      ,X
        sta      dose_value+1

        rts

********************************************************************************
*     twos_comp:   twos complements the INTACC2, 4-byte value

twos_comp:
        ldhx     #INTACC2                   ; load hx with address of intacc1
        mov      #$04,calc_temp             ; counter
loop1:
        lda      ,X                         ; loads a with mem[h:x]
        coma                                ; complement a
        sta      ,X                         ;
        aix      #1                         ; increment h:x
        dbnz     calc_temp,loop1            ; loop
        mov      #$04,calc_temp             ; counter
        sec                                 ; set carry bit to one
loop2:                                      ; this is the "add one" part of 2s complement
        aix      #-1                        ; decrement h:x
        lda      ,X                         ; load a with mem[h:x]
        adc      #0                         ; add 0 with carry
        sta      ,X                         ; blah
        dbnz     calc_temp,loop2            ; blah
        rts

********************************************************************************
*     add_32bit:   adds INTACC1 and INTACC2, 4-byte result back into INTACC1

add_32bit:
        ldhx     #INTACC1                   ; load hx with address of intacc1
        aix      #3                         ; move to LSB of INTACC1
        mov      #$04,calc_temp             ; counter
        clc                                 ; clear the carry bit (CYA)
loop3:
        lda      ,X                         ; loads A with byte of INTACC1
        aix      #4                         ; move H:X to same byte of INTACC2
        adc      ,X                         ; add two together
        aix      #-4                        ; move H:X back to proper INTACC1 byte
        sta      ,X                         ; and store
        aix      #-1                        ; decrease H:X for next significant byte
        dbnz     calc_temp,loop3            ; loop
        rts



**************************************************************
* LOOK AT CARD -                                             *
*        looks at card 8 times for 0.125 seconds each time   *
*        sums total pulses counted in 24-bit value:          *
*                                                            *
*        pulse_xtra : pulse_count : pulse_count+1            *
**************************************************************
look_at_card:
        clr     pulse_count
        clr     pulse_count+1
        clr     pulse_xtra
        mov     #$08,count_looks
        clrh
        clrx
        bclr    2,PTD
        bset    1,PTD
        bset    0,PTD           ; fire up LED
        lda     #$FA            ; pause 250mS for teh hotness
        jsr     pause_ms


        mov     #$44,T1SC0      ; t1ch0 input capture enable
look_125ms:
        bclr    0,timer_flag
        clrh
        clrx
        clra
        bset    4,T1SC          ; clear timer counter (and prescaler)
        mov     #$44,T1SC       ; start tim1 /16 and overflow enable
wait_overflow:
        nop

        brclr   0,timer_flag,wait_overflow
        clc
        txa
        add     pulse_count+1
        sta     pulse_count+1
        pshh
        pula
        adc     pulse_count
        sta     pulse_count
        clra
        adc     pulse_xtra
        sta     pulse_xtra
        dbnz    count_looks,look_125ms
        bclr    0,PTD           ; kill LED
        bset    2,PTD
        bset    1,PTD
        rts

**************************************************************
* Average Pulse -                                            *
*       averages (and rounds) pulse_count to give #pulses    *
*       for a 250 mSec period.  16-bit value is stored in    *
*                                                            *
*       pulse_count : pulse_count+1                          *
**************************************************************
avg_pulses:
        clc
        ror     pulse_xtra
        ror     pulse_count
        ror     pulse_count+1
        clc
        ror     pulse_xtra
        ror     pulse_count
        ror     pulse_count+1
        bcc     no_round
        inc     pulse_count+1
no_round:
        clrh
        lda     pulse_count
        ldx     #$0A
        div
        sub     #2
        bmi     no_adjust
        adc     pulse_count+1
        sta     pulse_count+1
        clra
        adc     pulse_count
        sta     pulse_count
no_adjust:
        rts


**************************************************************
* DISPLAY HEX VALUES -     send a byte in A
*                          receive hex digits in 'tens','ones'
**************************************************************
conv_h2a:
       psha
       psha
       bsr      conv_hex2ascii
       sta      ones
       pula
       nsa
       bsr      conv_hex2ascii
       sta      tens
       pula
       rts

conv_hex2ascii:
       and      #$0F
       sub      #$0A
       blt      not_hex
       add      #$07
not_hex:
       add      #$3A
       rts



**************************************************************
*  DISPLAY_FREQUENCY -- converts the binary values
*  in 'kilohertz' and 'hertz' to decimal equivalent characters
*  and print them to the LCD
**************************************************************
display_freq:
       ldhx      text_address
       jsr       LCD_print

       lda       kilohertz         ; Convert Hex Kilohertz to Decimal
       jsr       conv_hex2dec
       lda       hundreds          ; Display the hundreds character
       add       #$30              ;
       jsr       LCD_write_data
       lda       tens              ; Display the tens character
       add       #$30              ;
       jsr       LCD_write_data
       lda       ones              ; Display the ones character
       add       #$30              ;
       jsr       LCD_write_data

       lda       #$2B              ; "+"
       jsr       LCD_write_data

       lda       reference_value   ; Convert Hex Ref. Value to Decimal
       jsr       conv_hex2dec
       lda       tens              ; Display the tens character
       add       #$30              ;
       jsr       LCD_write_data
       lda       ones              ; Display the ones character
       add       #$30              ;
       jsr       LCD_write_data

       lda       #$20
       jsr       LCD_write_data    ; spaces
       jsr       LCD_write_data

       rts

**************************************************************
* PULSES TO FREQ -                                           *
*       converts the number in 16-bit pulse_count to         *
*       kilohertz and hertz.   pulse_count must hold the     *
*       number of pulses per *quarter second*  (250 ms)      *
**************************************************************
pulse_to_freq:
        ldhx    pulse_count
        txa
        ldx     #$FA
        div
        sta     kilohertz
        pshh
        pula
        sta     hertz
        lda     #$7D
        sub     hertz
        bpl     skip_roundup
        inc     kilohertz
skip_roundup:
        rts


**************************************************************
* conv_hex2dec
*
* This function converts the value stored in A to a dec. number
* stored as single digit values in 'hundreds', 'tens', 'ones'
**************************************************************
conv_hex2dec:
       clr      ones
       clr      tens
       clr      hundreds
       clr      ten3rd
       clr      ten4th
hundreds_loop:
       inc      hundreds
       sub      #$64             ; subtract 100's
       beq      exit_hex2dec
       bcc      hundreds_loop    ; checks for 'subtraction underflow'
       dec      hundreds
       add      #$64             ; already been negativified
tens_loop:
       inc      tens
       sub      #$0A             ; subtract 10's
       beq      exit_hex2dec
       bcc      tens_loop        ; checks for 'subtraction underflow'
       dec      tens
       add      #$0A
ones_loop:
       inc      ones
       sub      #$01
       beq      exit_hex2dec
       bpl      ones_loop
       dec      ones
exit_hex2dec:
       rts

**************************************************************
* conv_16bitH2D
*
* ENTRY VALUES:  H:X contains an unsigned 16-bit value
* EXIT VALUES:   the decimal equivalent is stored as follows:
*                       10000's -> 'ten4th'
*                        1000's -> 'ten3rd'
*                         100's -> 'hundreds'
*                          10's -> 'tens'
*                           1's -> 'ones
**************************************************************
conv_16bitH2D:
        psha
        pshx
        pshh
        clr     INTACC1
        clr     INTACC1+1
        sthx    INTACC1+2
        ldhx    #10000T
        sthx    INTACC2
        jsr     UDVD32
        mov     INTACC1+3,ten4th
        ldhx    INTACC2
        sthx    INTACC1+2
        ldhx    #1000T
        sthx    INTACC2
        jsr     UDVD32
        mov     INTACC1+3,ten3rd
        ldhx    INTACC2
        pshx
        pula
        ldx     #100T
        div
        sta     hundreds
        pshh
        pula
        clrh
        ldx     #10T
        div
        sta     tens
        pshh
        pula
        sta     ones
        pulh
        pulx
        pula
        rts



**************************************************************
* DELAY_LOOP -                                               *
*    core delay loop.  for 8MHz bus clock,  X:A gives delay  *
*       of .0005(257*X + A - 250) milliseconds               *
**************************************************************
delay500ns:
      pshx
      ldx     #$04
      lda     #$DE
      bra     more
delay:                    ;
      pshx                ; $27E3 = 5.00 ms            2
      ldx     #$08        ; $08C2 = 1.00 ms            2
      lda     #$C2        ; $04DE = 0.50 ms            2
more:                     ; $02DC = 0.25 ms
      deca                                          ; 1
      bne     more                                  ; 3
      decx                                          ; 1
      bne     more                                  ; 3
      pulx                                          ; 2
      rts                                           ; 4

**************************************************************
* pause_ms  -  call with 'A' requesting 1ms increments.
*              250 ms effective maximum
**************************************************************

pause_ms:
      sta     pause1ms
pause2:
      jsr     delay
      dbnz    pause1ms,pause2
      rts

**************************************************************
* PAUSE_1SEC  -  call with 'A' requesting 1s. increments.
*             -- 64 seconds effective maximum

pause_1sec:
      lsla
      lsla
      sta     pause1sec
bp_2:
      lda     #$FA
      jsr     pause_ms
      dbnz    pause1sec,bp_2
      rts



**************************************************************
* LCD_print  -
*    H:X must contain beginning address of a 16-byte ascii  string
*    first 8 bytes will be printed on line 1, second 8 on line 2
**************************************************************
LCD_print:
       psha
       pshx
       pshh
       mov    #$08,LCD_counter
first_line:
       lda    ,X
       jsr    LCD_write_data
       aix    #1
       dbnz   LCD_counter,first_line
       mov    #$08,LCD_counter
       jsr    LCD_line2
second_line:
       lda    ,X
       jsr    LCD_write_data
       aix    #1
       dbnz   LCD_counter,second_line
       jsr    LCD_homecursor
       pulh
       pulx
       pula
       rts

**************************************************************
* LCD_oneline -- prints text on current line
*             H:X contains address of null-terminated text
*             returns cursor to home
**************************************************************
LCD_oneline:
       pshx
       pshh
       psha
next_character:
       lda    ,X
       beq    oneline_done
       jsr    LCD_write_data
       aix    #1
       bra    next_character
oneline_done:
       jsr    LCD_homecursor
       pula
       pulh
       pulx
       rts

**************************************************************
* LCD_shift_cursor  -- moves cursor to the right
*                      'A' register determines how many places
*                      if A=-1, backspace (left) one place
**************************************************************
LCD_shift_cursor:
       psha
       sta    LCD_counter
       beq    rshift_done
       bmi    lshift_one
rshift:
       lda    #$14
       jsr    LCD_write_command
       dbnz   LCD_counter,rshift
rshift_done:
       pula
       rts
lshift_one:
       lda    #$10
       jsr    LCD_write_command
       bra    rshift_done

**************************************************************
* LCD_shift_disp -- shifts text display to the left
*                   for reading long lines.  The 'A' register
*                   contains number of units to shift
**************************************************************
LCD_shift_disp:
       psha
       sta    LCD_counter
       beq    lshift_done
lshift:
       lda    #$18
       jsr    LCD_write_command
       lda    #$7d                ;pause for teh smoothness
       jsr    pause_ms
       dbnz   LCD_counter,lshift
lshift_done:
       pula
       rts

**************************************************************
* LCD_line2
*
**************************************************************
LCD_line2:
       psha
       lda      #$02                   ; Set to second line
       jsr      LCD_write_command
       lda      #$c0
       jsr      LCD_write_command
       pula
       rts

**************************************************************
* LCD_clear_display
* This function clears the display and returns the cursor to
* address 0    $01
**************************************************************
LCD_clear_display:
       psha
       lda      #$00                   ; clear display
       sta      PTC
       bset     3,PTD
       jsr      delay
       jsr      delay
       bclr     3,PTD
       jsr      delay
       lda      #$01                   ; clear display
       sta      PTC
       bset     3,PTD
       jsr      delay
       jsr      delay
       bclr     3,PTD
       jsr      delay
       pula
       rts

**************************************************************
* LCD_homecursor
*
**************************************************************
LCD_homecursor:
       psha
       lda     #$80
       jsr     LCD_write_command
       pula
       rts



       psha
       lda      #$00                   ;
       sta      PTC
       bset     3,PTD
       jsr      delay
       jsr      delay
       bclr     3,PTD
       jsr      delay
       lda      #$02                   ;
       sta      PTC
       bset     3,PTD
       jsr      delay
       jsr      delay
       bclr     3,PTD
       jsr      delay
       pula
       rts

**************************************************************
* If LCD_write_command is called:
*      rs_line = $00 for command
*
* If LCD_write_data is called
*      rs_line = $10 for data
*
* value in the accumulator is then written to the LCD.
**************************************************************
LCD_write_data:
       mov         #$10,rs_line
LCD_write_command:
       psha                     ; store the original value
       psha                     ; once more
       nsa                      ; swap the nibbles
       and         #$0f         ; mask the low nibble (formerly high nibble)
       ora         rs_line      ; don't forget about the RS line!!
       sta         PTC
       jsr         pulse_e
       pula
       and         #$0f         ; mask the low nibble
       ora         rs_line      ; don't forget about the RS line!!
       sta         PTC
       jsr         pulse_e
       clr         rs_line      ; clear rs_line for command default
       pula
       rts



**************************************************************
* PULSE_E
* This function pulses the enable line to the LCD
* thereby clocking in data
**************************************************************
pulse_e:
        bset    3,PTD
        jsr     delay
        bclr    3,PTD
        jsr     delay
        rts

***************************************************************
* KEYBD_ISR - Keyboard Interrupt Service Routine.             *
***************************************************************
keybd_isr:
         bset    1,INTKBSCR              ; enable IMASKK to disable KBI
         brset   2,keybd_flag,kbi_done   ; if both buttons had been pressed

         lda     PTA
         and     #03            ; check if both are pressed -- highest priority
         beq     both_press

         tst    keybd_flag
         bne    kbi_done        ; if a single button already pressed, keep it

         brclr   0,PTA,button0
         brclr   1,PTA,button1
         clr     keybd_flag
         bra     kbi_done
both_press:
         lda     keybd_flag
         ora     #$07
         sta     keybd_flag
         bra     kbi_done

button0: bset    0,keybd_flag
         bra     kbi_done

button1: bset    1,keybd_flag
kbi_done:
        bset    2,INTKBSCR     ; write ACKK to clear false interrupts
        bclr    1,INTKBSCR     ; disable IMASKK to enable KBI
        ;brset   3,INTKBSCR,kbi_done
        rti


pulse_LED:
        psha
        bset    0,PTD
        lda     #64
        jsr     pause_ms
        bclr    0,PTD
        lda     #64
        jsr     pause_ms
        pula
        sub     #1
        bne     pulse_LED
        lda     #$C8
        jsr     pause_ms
        rts


timr1_isr:

        rti



**************************************************************
* TIMR0_ISR - Timer Channel 0 Interrupt Service Routine.     *
* OVER1_ISR - Timer 1 Overflow Interrupt                     *
**************************************************************
timr0_isr:
        sei
        aix     #$01            ; count pulses into H:X         2

        lda     T1SC0           ;                               3
        and     #$7F            ;                               2
        sta     T1SC0           ; clear ch0 Interrupt           3

        cli                     ;                               2
        ais     #$03            ;                               2
        rts                     ;                               4

over1_isr:
        brset   7,timer_flag,countdown_10sec

        bset    0,timer_flag    ; set flag
        bset    5,T1SC          ; stop timer
        lda     T1SC            ; clear Tim1 Overflow Interrupt
        and     #$27
        sta     T1SC
        cli
        rti

countdown_10sec:
        dbnz    pulse_timer,c10s2
        bclr    7,timer_flag
        bset    5,T1SC          ; stop timer
c10s2:
        lda     T1SC            ; clear Tim1 Overflow Interrupt
        and     #$67
        sta     T1SC
        rti

**************************************************************
* IRQ_ISR - Interrupt Request (IRQ) Service Routine.         *
*
**************************************************************
irq_isr:
       bset     2,INTSCR                   ; acknowledge current IRQ interrupt
       bclr     1,INTSCR                   ; enable IRQ interrupts
       rti

**************************************************************
* DUMMY_ISR - Dummy Interrupt Service Routine.               *
*             Just does a return from interrupt.             *
**************************************************************
dummy_isr:
        rti


**********************************************
* Exponent Table
*********************************************


    org exp_table_p

index_pos:
    fdb 1000T,1044T,1091T,1139T,1189T,1242T,1297T,1354T,1414T,1477T,1542T,1610T,1682T,1756T,1834T,1915T,2000T,2089T
    fdb 2181T,2278T,2378T,2484T,2594T,2709T,2828T,2954T,3084T,3221T,3364T,3513T,3668T,3830T,4000T,4177T
    fdb 4362T,4555T,4757T,4967T,5187T,5417T,5657T,5907T,6169T,6442T,6727T,7025T,7336T,7661T,8000T,8354T
    fdb 8724T,9110T,9514T,9935T,10375T,10834T,11314T,11815T,12338T,12884T,13454T,14050T,14672T,15322T,16000T
    fdb 16708T,17448T,18221T,19027T,19870T,20749T,21668T,22627T,23629T,24675T,25768T,26909T,28100T,29344T
    fdb 30643T,32000T,33417T,34896T,36441T,38055T,39739T,41499T,43336T,45255T,47258T,49351T,51536T,53817T
    fdb 56200T,58688T,61287T,64000T


    org exp_table_n

index_negs:
    fdb 4T,4T,4T,4T,5T,5T,5T,5T,6T,6T,6T,6T,7T,7T,7T,7T,8T,8T,9T,9T,9T,10T,10T,11T,11T,12T,12T
    fdb 13T,13T,14T,14T,15T,16T,16T,17T,18T,19T,19T,20T,21T,22T,23T,24T,25T,26T,27T,29T,30T,31T,33T
    fdb 34T,36T,37T,39T,41T,42T,44T,46T,48T,50T,53T,55T,57T,60T,63T,65T,68T,71T,74T,78T,81T,85T,88T
    fdb 92T,96T,101T,105T,110T,115T,120T,125T,131T,136T,142T,149T,155T,162T,169T,177T,185T,193T,201T
    fdb 210T,220T,229T,239T,250T,261T,273T,285T,297T,310T,324T,339T,354T,369T,386T,403T,420T,439T,459T
    fdb 479T,500T,522T,545T,569T,595T,621T,648T,677T,707T,738T,771T,805T,841T,878T,917T,958T


    org Text
prompt_text   fcb "Select  Gas   ->"
gas_names     fcb "Ammonia (L)     Anilene (L)     H)CarbonMonoxideChlorine        Formel- dahyde  "
gas_names2    fcb "Gluterel-dahyde HydrogenSulf.(H)HydrogenSulf.(L)Iso-    cyanidesMethanol        "
gas_names3    fcb "MMH             Mustard         NitrogenDiox.(H)NitrogenDiox.(L)Ozone           "
gas_names4    fcb "PropylneOxide   Sulfur  Diox.(H)Sulfur  Diox.(L)"
units_text    fcb "D=     ",0,"ppm-hrs",0,"C=     ",0,"ppm    ",0,"P=     ",0,"Pulses ",0
prompt_cali   fcb "CalibratGas #   "
prompt_enter  fcb "Enter toBegin ->"
please_wait   fcb "Please  Wait ..."
cali_pass     fcb "1st Pass",0,"2nd Pass",0,"3rd Pass",0,"4th Pass",0
cali_good     fcb "   Cali.Success!"
begin_monitor fcb "Monitor Ready ->"
print_badcard fcb "Bad CardRetry ->"
print_badcali fcb "CalibratFailure "
prompt_memory fcb "DownloadHistory "
loading       fcb "        >>load<<"

    org GasValues
gas_constants:
       fdb $0200,$0FF0,$000E,$123F,$0000,$0C72,$0000,$0000   ; (0)  Ammonia (L)
       fdb $0100,$3FC0,$0016,$641C,$0000,$5807,$0000,$007B   ; (1)  Anilene (L)          -- 2^18
       fdb $0300,$0FF0,$000C,$6885,$0000,$096E,$0000,$0001   ; (2)  Carbon Monoxide (H)
       fdb $0500,$0FF0,$000B,$E7CE,$0000,$38B3,$0000,$0030   ; (3)  Chlorine
       fdb $0200,$0FF0,$0035,$E6E6,$0000,$3D58,$0000,$0000   ; (4)  Formeldahyde (C)
       fdb $0200,$0FF0,$001E,$EF74,$0000,$2428,$0000,$0000   ; (5)  Glutereldahyde
       fdb $0200,$FF00,$004F,$980E,$0000,$883F,$0000,$000B   ; (6)  Hydrogen Sulfide (H)  -- 2^20
       fdb $0200,$0FF0,$0005,$5DFA,$0000,$0A79,$0000,$0000   ; (7)  Hydrogen Sulfide (L)
       fdb $0500,$0FF0,$0003,$671D,$0000,$2B88,$0000,$0027   ; (8)  Isocyanides
       fdb $0200,$0FF0,$0037,$EA13,$0000,$503F,$0000,$0019   ; (9)  Methanol
       fdb $0500,$0FF0,$0078,$975F,$0001,$58BD,$0000,$00E4   ; (10) MMH
       fdb $0500,$0FF0,$0065,$A9B2,$0001,$1A7C,$0000,$00BD   ; (11) Mustard
       fdb $0200,$0FF0,$000C,$C3BE,$0000,$11F5,$0000,$0000   ; (12) Nitrogen Dioxide (H)
       fdb $0300,$0FF0,$0004,$D506,$0000,$04C2,$0000,$0005   ; (13) Nitrogen Dioxide (L)
       fdb $0500,$0FF0,$0041,$C85B,$0000,$CC3D,$0000,$0089   ; (14) Ozone
       fdb $0300,$0FF0,$000A,$14A5,$0000,$090D,$0000,$0001   ; (15) Propyline Oxide
       fdb $0500,$0FF0,$0010,$3896,$0000,$3E5C,$0000,$0032   ; (16) Sulfur Dioxide (H)
       fdb $0500,$0FF0,$0007,$87E4,$0000,$22C0,$0000,$0021   ; (17) Sulfur Dioxide (L)




$Include 'mathmemroutines.asm'


**************************************************************
* Vectors - Timer Interrupt Service Routine.                 *
*             after a RESET.                                 *
**************************************************************
   org  VectorStart

        dw  dummy_isr    ; Time Base Vector
        dw  dummy_isr    ; ADC Conversion Complete
        dw  keybd_isr    ; Keyboard Vector
        dw  dummy_isr    ; SCI Transmit Vector
        dw  dummy_isr    ; SCI Receive Vector
        dw  dummy_isr    ; SCI Error Vector
        dw  dummy_isr    ; SPI Transmit Vector
        dw  dummy_isr    ; SPI Receive Vector
        dw  dummy_isr    ; TIM2 Overflow Vector
        dw  dummy_isr    ; TIM2 Channel 1 Vector
        dw  dummy_isr    ; TIM2 Channel 0 Vector
        dw  over1_isr    ; TIM1 Overflow Vector
        dw  timr1_isr    ; TIM1 Channel 1 Vector
        dw  timr0_isr    ; TIM1 Channel 0 Vector
        dw  dummy_isr    ; ICG/CGM Vector
        dw  irq_isr    ; ~IRQ1 Vector
        dw  dummy_isr    ; SWI Vector
        dw  main_init    ; Reset Vector

