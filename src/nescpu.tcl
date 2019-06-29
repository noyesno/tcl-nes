# Ref: http://obelisk.me.uk/6502/reference.html

namespace eval nescpu {
  variable flags
  variable register

  # I : interrupt disable

  variable opcode

  # byte {OPCODE bytes cycle note} 
  array set opcode_schema {
    0xea {NOP ""       3 "No Operation"}
    0x00 {BRK ""       3 "Force Interrupt"}
    0x40 {RTI ""       3 "Return From Interrupt"}

    0x48 {PHA ""       3 "Push Accumulator Regsiter"}
    0x08 {PHP ""       3 "Push Processor Status"}
    0x68 {PLA ""       3 "Pull Accumulator Regsiter"}
    0x28 {PLP ""       3 "Pull Processor Status"}

    0x10 {BPL "int8"   2 "Branch if Positive"}
    0x30 {BMI "int8"   2 "Branch if Minus"}

    0xd0 {BNE "int8"   2 "Branch if Not Equal"}
    0xf0 {BEQ "int8"   2 "Branch if Equal"}
    0x90 {BCC "int8"   2 "Branch if Carry Clear"}
    0xb0 {BCS "int8"   2 "Branch if Carry Set"}
    0x50 {BVC "int8"   2 "Branch if Overflow Clear"}
    0x70 {BVS "int8"   2 "Branch if Overflow Set"}
    0x20 {JSR "int16"  2 "Jump to Subroutine"}
    0x60 {RTS ""       6 "Return from Subroutine"}
    0x4c {{JMP -addr}     "int16"  2 "Jump, Absolute"}
    0x6c {{JMP -indirect} "int16"  2 "Jump, Indirect"}
    0xaa {TAX ""       2 "Transfer Accumulator to X"}
    0xa8 {TAY ""       2 "Transfer Accumulator to Y"}
    0xba {TSX ""       2 "Transfer Stack Pointer to X"}
    0x8a {TXA ""       2 "Transfer X to Accumulator"}
    0x9a {TXS ""       2 "Transfer X to Stack Pointer"}
    0x98 {TYA ""       2 "Transfer Y to Accumulator"}

    0x29 {{AND -value}      "int8"   2 "Logical AND - Immediate"}
    0x25 {{AND -zero}       "int8"   2 "Logical AND - Immediate"}
    0x35 {{AND -zero-x}     "int8"   2 "Logical AND - Immediate"}
    0x2d {{AND -addr}       "int16"  2 "Logical AND - Immediate"}
    0x3d {{AND -addr-x}     "int16"  2 "Logical AND - Immediate"}
    0x39 {{AND -addr-y}     "int16"  2 "Logical AND - Immediate"}
    0x21 {{AND -indirect-x} "int8"   2 "Logical AND - Immediate"}
    0x31 {{AND -indirect-y} "int8"   2 "Logical AND - Immediate"}

    0x49 {{EOR -value}      "int8"   2 "Exclusive OR"}
    0x45 {{EOR -zero}       "int8"   2 "Exclusive OR"}
    0x55 {{EOR -zero-x}     "int8"   2 "Exclusive OR"}
    0x4d {{EOR -addr}       "int16"  2 "Exclusive OR"}
    0x5d {{EOR -addr-x}     "int16"  2 "Exclusive OR"}
    0x59 {{EOR -addr-y}     "int16"  2 "Exclusive OR"}
    0x41 {{EOR -indirect-x} "int8"  2 "Exclusive OR"}
    0x51 {{EOR -indirect-y} "int8"  2 "Exclusive OR"}

    0x09 {{ORA -value}      "int8"   2 "Exclusive OR"}
    0x05 {{ORA -zero}       "int8"   2 "Exclusive OR"}
    0x15 {{ORA -zero-x}     "int8"   2 "Exclusive OR"}
    0x0d {{ORA -addr}       "int16"  2 "Exclusive OR"}
    0x1d {{ORA -addr-x}     "int16"  2 "Exclusive OR"}
    0x19 {{ORA -addr-y}     "int16"  2 "Exclusive OR"}
    0x01 {{ORA -indirect-x} "int8"  2 "Exclusive OR"}
    0x11 {{ORA -indirect-y} "int8"  2 "Exclusive OR"}

    0x29 {{AND -value}      "int8"   2 "Exclusive OR"}
    0x25 {{AND -zero}       "int8"   2 "Exclusive OR"}
    0x35 {{AND -zero-x}     "int8"   2 "Exclusive OR"}
    0x2d {{AND -addr}       "int16"  2 "Exclusive OR"}
    0x3d {{AND -addr-x}     "int16"  2 "Exclusive OR"}
    0x39 {{AND -addr-y}     "int16"  2 "Exclusive OR"}
    0x21 {{AND -indirect-x} "int8"   2 "Exclusive OR"}
    0x31 {{AND -indirect-y} "int8"   2 "Exclusive OR"}


    0x69 {{ADC -value}      "int8"   2 "Add with Carry"}
    0x65 {{ADC -zero}       "int8"   2 "Add with Carry"}
    0x75 {{ADC -zero-x}     "int8"   2 "Add with Carry"}
    0x6d {{ADC -addr}       "int16"  2 "Add with Carry"}
    0x7d {{ADC -addr-x}     "int16"  2 "Add with Carry"}
    0x79 {{ADC -addr-y}     "int16"  2 "Add with Carry"}

    0xe9 {{SBC -value}      "int8"   2 "Sub with Carry"}
    0xe5 {{SBC -zero}       "int8"   2 "Sub with Carry"}
    0xf5 {{SBC -zero-x}     "int8"   2 "Sub with Carry"}
    0xed {{SBC -addr}       "int16"  2 "Sub with Carry"}
    0xfd {{SBC -addr-x}     "int16"  2 "Sub with Carry"}
    0xf9 {{SBC -addr-y}     "int16"  2 "Sub with Carry"}

    0xc9 {{CMP -value}      "int8"   2 "Compare"}
    0xc5 {{CMP -zero}       "int8"   2 "Compare"}
    0xd5 {{CMP -zero-x}     "int8"   2 "Compare"}
    0xcd {{CMP -addr}       "int16"  2 "Compare"}
    0xdd {{CMP -addr-x}     "int16"  2 "Compare"}
    0xd9 {{CMP -addr-y}     "int16"  2 "Compare"}
    0xc1 {{CMP -indirect-x} "int8"   2 "Compare"}
    0xd1 {{CMP -indirect-y} "int8"   2 "Compare"}

    0xe0 {{CPX -value}      "int8"   2 "Compare"}
    0xe4 {{CPX -zero}       "int8"   2 "Compare"}
    0xec {{CPX -addr}       "int16"  2 "Compare"}
    0xc0 {{CPY -value}      "int8"   2 "Compare"}
    0xc4 {{CPY -zero}       "int8"   2 "Compare"}
    0xcc {{CPY -addr}       "int16"  2 "Compare"}

    0x4a {{LSR -reg-a}  ""      2 "Logical Shift Right"}
    0x46 {{LSR -zero}   "int8"  5 "Logical Shift Right"}
    0x56 {{LSR -zero-x} "int8"  5 "Logical Shift Right"}
    0x4e {{LSR -addr}   "int16" 5 "Logical Shift Right"}
    0x5e {{LSR -addr-x} "int16" 5 "Logical Shift Right"}

    0x6a {{ROR -reg-a}  ""      2 "Rotate Right"}
    0x66 {{ROR -zero}   "int8"  5 "Rotate Right"}
    0x76 {{ROR -zero-x} "int8"  5 "Rotate Right"}
    0x6e {{ROR -addr}   "int16" 5 "Rotate Right"}
    0x7e {{ROR -addr-x} "int16" 5 "Rotate Right"}

    0x0a {{ASL -reg-a}  ""      2 "Logical Shift Right"}
    0x06 {{ASL -zero}   "int8"  5 "Logical Shift Right"}
    0x16 {{ASL -zero-x} "int8"  5 "Logical Shift Right"}
    0x0e {{ASL -addr}   "int16" 5 "Logical Shift Right"}
    0x1e {{ASL -addr-x} "int16" 5 "Logical Shift Right"}

    0x78 {SEI ""       2 "Set Interrupt Disable"}
    0xf8 {SED ""       2 "Set Decimal Disable"}
    0x38 {SEC ""       2 "Set Carry Disable"}
    0x58 {CLI ""       2 "Clear Interrupt Flag"}
    0x18 {CLD ""       2 "Clear Decimal Mode"}
    0xd8 {CLC ""       2 "Clear Carry Flag"}
    0xb8 {CLV ""       2 "Clear Overflow Flag"}

    0xe8 {INX ""       2 "Increment X Register"}
    0xc8 {INY ""       2 "Increment X Register"}
    0xe6 {{INC -zero}   "int8"      2 "Increment Memory"}
    0xf6 {{INC -zero-x} "int8"      2 "Increment Memory"}
    0xee {{INC -addr}   "int16"     2 "Increment Memory"}
    0xfe {{INC -addr-x} "int16"     2 "Increment Memory"}

    0xca {DEX ""                2 "Decrement X Register"}
    0x88 {DEY ""                2 "Decrement Y Register"}
    0xc6 {{DEC -zero}   "int8"      2 "Decrement Memory"}
    0xd6 {{DEC -zero-x} "int8"      2 "Decrement Memory"}
    0xce {{DEC -addr}   "int16"     2 "Decrement Memory"}
    0xde {{DEC -addr-x} "int16"     2 "Decrement Memory"}

    0xa9 {{LDA -value}      "int8"   2 "Load Accumulator - Immediate Addr"}
    0xa5 {{LDA -zero}       "int8"   2 "Load Accumulator - Zero Page"}
    0xb5 {{LDA -zero-x}     "int8"   2 "Load Accumulator - Zero Page, X"}
    0xad {{LDA -addr}       "int16"  4 "Load Accumulator - Absolute"}
    0xbd {{LDA -addr-x}     "int16"  4 "Load Accumulator - Absolute, X"}
    0xb9 {{LDA -addr-y}     "int16"  4 "Load Accumulator - Absolute, Y"}
    0xa1 {{LDA -indirect-x} "int8"  4 "Load Accumulator - (Indirect, X)"}
    0xb1 {{LDA -indirect-y} "int8"  4 "Load Accumulator - (Indirect), Y"}

    0xa2 {{LDX -value}  "int8"   2 "Load X Regsiter - Immediate Addr"}
    0xa6 {{LDX -zero}   "int8"   3 "Load X Regsiter - Zero Page"}
    0xb6 {{LDX -zero-y} "int8"   4 "Load X Regsiter - Zero Page, Y"}
    0xae {{LDX -addr}   "int16"  4 "Load X Regsiter - Absolute"}
    0xbe {{LDX -addr-y} "int16"  4 "Load X Regsiter - Absolute, Y"}

    0xa0 {{LDY -value}  "int8"   2 "Load Y Regsiter - Immediate Addr"}
    0xa4 {{LDY -zero}   "int8"   3 "Load Y Regsiter - Zero Page"}
    0xb4 {{LDY -zero-x} "int8"   4 "Load Y Regsiter - Zero Page, X"}
    0xac {{LDY -addr}   "int16"  4 "Load Y Regsiter - Absolute"}
    0xbc {{LDY -addr-x} "int16"  4 "Load Y Regsiter - Absolute, X"}

    0x85 {{STA -zero}       "int8"   3 "Store Accumulator - Zero Page"}
    0x95 {{STA -zero-x}     "int8"   3 "Store Accumulator - Zero Page,X"}
    0x8d {{STA -addr}       "int16"  4 "Store Accumulator - Absolute Addr"}
    0x9d {{STA -addr-x}     "int16"  4 "Store Accumulator - Absolute Addr, X"}
    0x99 {{STA -addr-y}     "int16"  4 "Store Accumulator - Absolute Addr, X"}
    0x81 {{STA -indirect-x} "int8"  4 "Store Accumulator - Absolute Addr, X"}
    0x91 {{STA -indirect-y} "int8"  4 "Store Accumulator - Absolute Addr, X"}

    0x86 {{STX -zero}   "int8"   3 "Store X Register - Zero Page"}
    0x96 {{STX -zero-y} "int8"   3 "Store X Register - Zero Page,Y"}
    0x8e {{STX -addr}   "int16"  4 "Store X Register - Absolute Addr"}

    0x84 {{STY -zero}   "int8"   3 "Store Y Register - Zero Page"}
    0x94 {{STY -zero-x} "int8"   3 "Store Y Register - Zero Page,X"}
    0x8c {{STY -addr}   "int16"  4 "Store Y Register - Absolute Addr"}
  }

  proc stack_push {value} {
     variable stack
     variable register

     if {$register(SP)<[llength $stack]} {
       lset stack $register(SP) $value
     } else {
       lappend stack $value
     }
     incr register(SP)
     # puts [list $register(SP) $stack]
  }

  proc stack_pop {} {
     variable stack
     variable register

     # puts [list $register(SP) $stack]
     incr register(SP) -1
     return [lindex $stack $register(SP)]
  }

  proc reset {} {
    variable flags
    variable register
    variable stack
    variable memory

    set stack [list]
    set memory [lrepeat 0xffff 0]

    array set register {
      "PC"  0
      "SP"  0
      "A"   0
      "X"   0
      "Y"   0
    }

    # C: Carry Flag
    # Z: Zero Flag
    # I: Interrupt Disable
    # D: Decimal Mode Flag
    # B: Break Command
    # V: Overflow Flag
    # N: Negative Flag
    array set flags {
      "C" 0
      "Z" 0
      "I" 0
      "D" 0
      "B" 0
      "V" 0
      "N" 0
    }
  }

  proc mem_set {addr value {mode "-addr"}} {
    variable memory
    variable register
    variable flags

    switch -- $mode {
      "-addr" {
        set addr $addr
      }
      "-zero" {
        set addr  [expr {$addr & 0xff}]
      }
      "-zero-x" {
        set addr  [expr {($addr + $register(X)) & 0xff}]
      }
      "-zero-y" {
        set addr  [expr {($addr + $register(Y)) & 0xff}]
      }
      "-addr-x" {
        set addr  [expr {($addr + $register(X))}]
      }
      "-addr-y" {
        set addr  [expr {($addr + $register(Y))}]
      }
      "-indirect-x" {
        set addr0 [lindex $memory [expr {($addr + $register(X)) & 0xff}]]
        set addr1 [lindex $memory [expr {($addr + $register(X) +1) & 0xff}]]
        set addr  [expr {$addr0 + ($addr1<<8)}]
      }
      "-indirect-y" {
        puts "indirect-y $addr"
        set addr0 [lindex $memory [expr {$addr}]]
        set addr1 [lindex $memory [expr {($addr+1) & 0xff}]]
        set addr  [expr {$addr0 + ($addr1<<8) + $register(Y)}]
        puts "indirect-y $addr1 $addr0 $addr"
      }
      default {
        error "unknown addr mode $addr"
      }
    }

    if {$addr == 0x2000} {
      puts [format "write addr PPUCTRL %04x %08b" $addr $value]
    } elseif {$addr == 0x2001} {
      puts [format "write addr PPUMASK %04x %08b" $addr $value]
    } elseif {$addr == 0x2002} {
      puts [format "write addr PPUSTATUS %04x %08b" $addr $value]
    } elseif {$addr == 0x2003} {
      puts [format "write addr OAMADDR %04x %04x" $addr $value]
    } elseif {$addr == 0x2004} {
      puts [format "write addr OAMDATA %04x %04x" $addr $value]
    } elseif {$addr == 0x2005} {
      puts [format "write addr PPUSCROLL %04x %04x" $addr $value]
    } elseif {$addr == 0x2006} {
      puts [format "write addr PPUADDR %04x %02x" $addr $value]
    } elseif {$addr == 0x2007} {
      puts [format "write addr PPUDATA %04x %02x" $addr $value]
    } elseif {$addr == 0x4014} {
      puts [format "write addr OAMDMA %04x %04x" $addr $value]
    } elseif {$addr >= 0x2000} {
      puts [format "write addr = %04x %04x" $addr $value]
    }

    lset memory $addr $value
  }

  proc mem_get {addr {mode "-addr"}} {
    variable memory
    variable register

    switch -- $mode {
      "-value" {
        set value $addr
        set addr  0x0000
      }
      "-addr" {
        set value [lindex $memory $addr]
      }
      "-zero" {
        set addr  [expr {$addr & 0xff}]
        set value [lindex $memory $addr]
      }
      "-zero-x" {
        set addr  [expr {($addr + $register(X)) & 0xff}]
        set value [lindex $memory $addr]
      }
      "-zero-y" {
        set addr  [expr {($addr + $register(Y)) & 0xff}]
        set value [lindex $memory $addr]
      }
      "-addr-x" {
        set addr  [expr {($addr + $register(X))}]
        set value [lindex $memory $addr]
      }
      "-addr-y" {
        set addr  [expr {($addr + $register(Y))}]
        set value [lindex $memory $addr]
      }
      "-indirect-x" {
        set addr0 [lindex $memory [expr {($addr + $register(X)) & 0xff}]]
        set addr1 [lindex $memory [expr {($addr + $register(X) +1) & 0xff}]]
        set addr  [expr {$addr0 + ($addr1<<8)}]
        set value [lindex $memory $addr]
      }
      "-indirect-y" {
        puts "indirect-y $addr"
        set addr0 [lindex $memory [expr {$addr}]]
        set addr1 [lindex $memory [expr {($addr+1) & 0xff}]]
        set addr  [expr {$addr0 + ($addr1<<8) + $register(Y)}]
        puts "indirect-y $addr1 $addr0 $addr"
        set value [lindex $memory $addr]
      }
      default {
        error "unknown addr mode $addr"
      }
    }

    if {$addr == 0x2002} {
      puts [format "read addr PPUSTATUS %04x %08b" $addr $value]
    } elseif {$addr == 0x2007} {
      puts [format "read addr PPUDATA %04x %02x" $addr $value]
    } elseif {$addr >= 0x2000} {
      puts [format "read addr %04x %02x" $addr $value]
    }

    return $value
  }

  proc ADC {mode addr} {
    variable register
    variable flags

    set value [mem_get $addr $mode]

    set result [expr {$register(A) + $value + $flags(C)}]

    set register(A) $result
   
    if {$result>0xff} {
      # TODO: Overflow Clear if overflow in bit 7
      set flags(C) 1
      set register(A) [expr {$result & 0xff}]
    } else {
      set flags(C) 0
    } 

    set flags(Z) [expr {$register(A)==0}]
    set flags(N) [expr {$register(A)>>7}]
  }

  proc SBC {mode addr} {
    variable register
    variable flags

    set value [mem_get $addr $mode]

    set register(A) [expr {$register(A) - $value - (1-$flags(C))}]

    if {$register(A)<0} {
      # TODO: Overflow Clear if overflow in bit 7
      set flags(C) 1
      set register(A) [expr {$register(A) & 0xff}] 
    } else {
      set flags(C) 0
    } 

    set flags(Z) [expr {$register(A)==0}]
    set flags(N) [expr {$register(A)>>7}]

  }

  proc opcode {byte} {
    variable opcode_schema
    set opcode [format "0x%02x" $byte]

    set opcode [array get opcode_schema $opcode]
    return $opcode
  }

  proc opcode_exec {opcode} {
    
  }

  proc NOP {args} {}

  # Force Interrupt. Trigger IRQ and jump to vector from $FFFE/F 
  proc BRK {} {
    variable register
    variable flags
    set flags(B) 1

    stack_push $register(PC)
    stack_push [array get flags]

    # TODO
    set register(PC) [rom_addr 0xfffe "su"]
    puts [format "BRK to = %04x" $register(PC)]
  }

  # Return from Interrupt
  proc RTI {} {
    variable register
    variable flags
    set flags(B) 1

    array set flags  [stack_pop]
    set register(PC) [stack_pop]
  }

  proc SEI {} {
    variable flags
    set flags(I) 1
  }
  proc SEC {} {
    variable flags
    set flags(C) 1
  }
  proc SED {} {
    variable flags
    set flags(D) 1
  }

  proc CLD {} {
    variable flags
    set flags(D) 0
  }
  proc CLC {} {
    variable flags
    set flags(C) 0
  }
  proc CLI {} {
    variable flags
    set flags(I) 0
  }
  proc CLV {} {
    variable flags
    set flags(V) 0
  }


  # Branch If Positive
  proc BPL {offset} {
    variable flags
    variable register

    if {$flags(N)} {
      # Negative
    } else {
      # Positive
      set PC $register(PC)
      incr register(PC) $offset
      puts [format "debug: branch if positive %04x+%04x = %04x" $PC $offset $register(PC)]
    } 
  }

  # Branch If Minus
  proc BMI {offset} {
    variable flags
    variable register

    if {$flags(N)} {
      # Negative
      puts "debug: branch if positive $offset"
      incr register(PC) $offset
    } else {
      # Positive
    } 
  }

  # Branch If Not Equal
  proc BNE {offset} {
    variable flags
    variable register

    if {$flags(Z)} {
      # Set
    } else {
      # Clear 
      puts "debug: branch if positive $offset"
      incr register(PC) $offset
    } 
  }

  # Branch If Equal
  proc BEQ {offset} {
    variable flags
    variable register

    if {$flags(Z)} {
      # Set
      incr register(PC) $offset
      puts [format "debug: branch if equale +%04x to %04x" $offset $register(PC)]
    } else {
      # Clear 
    } 
  }

  # Branch If Carry Clear
  proc BCC {offset} {
    variable flags
    variable register

    if {$flags(C)} {
      # Set
    } else {
      # Clear 
      incr register(PC) $offset
      puts [format "debug: branch if equale +%04x to %04x" $offset $register(PC)]
    } 
  }
  # Branch If Carry Clear
  proc BCS {offset} {
    variable flags
    variable register

    if {$flags(C)} {
      # Set
      incr register(PC) $offset
      puts [format "debug: branch if equale +%04x to %04x" $offset $register(PC)]
    } else {
      # Clear 
    } 
  }
  # Branch If Overflow Set
  proc BVS {offset} {
    variable flags
    variable register

    if {$flags(V)} {
      # Set
      incr register(PC) $offset
      puts [format "debug: branch if equale +%04x to %04x" $offset $register(PC)]
    } else {
      # Clear 
    } 
  }

  # Branch If Overflow Clear
  proc BVC {offset} {
    variable flags
    variable register

    if {$flags(V)} {
      # Set
    } else {
      # Clear 
      incr register(PC) $offset
      puts [format "debug: branch if equale +%04x to %04x" $offset $register(PC)]
    } 
  }


  # Jump to Subroutine
  proc JSR {addr} {
    variable register

    # stack_push [expr {$register(PC)-1}]
    puts [format "push PC = %04x" $register(PC)]
    stack_push $register(PC)
    set register(PC) $addr
  }

  # Return from Subroutine
  proc RTS {} {
    variable register

    set register(PC) [stack_pop]
    puts [format "pop PC = %04x" $register(PC)]
  }

  proc JMP {mode addr} {
    variable register

    switch -- $mode {
      "-addr" {
      }
      default {
        error "unknown $mode"
      }
    }
    set register(PC) $addr
  }


  proc TAX {} {
    variable register

    set register(X) $register(A) 
    set flags(Z) [expr {$register(X)==0}]
    set flags(N) [expr {$register(X)>>7}]
  }
  proc TAY {} {
    variable register

    set register(Y) $register(A) 
    set flags(Z) [expr {$register(Y)==0}]
    set flags(N) [expr {$register(Y)>>7}]
  }
  proc TXA {} {
    variable register

    set register(A) $register(X) 
    set flags(Z) [expr {$register(A)==0}]
    set flags(N) [expr {$register(A)>>7}]
  }
  proc TYA {} {
    variable register

    set register(A) $register(Y) 
    set flags(Z) [expr {$register(A)==0}]
    set flags(N) [expr {$register(A)>>7}]
  }

  proc INX {} {
    variable register

    set register(X) [expr {($register(X)+1) & 0xff}]

    set flags(Z) [expr {$register(X)==0}]
    set flags(N) [expr {$register(X)>>7}]
  }
  proc INY {} {
    variable register

    set register(Y) [expr {($register(Y)+1) & 0xff}]

    set flags(Z) [expr {$register(Y)==0}]
    set flags(N) [expr {$register(Y)>>7}]

  }
  proc DEX {} {
    variable register

    set register(X) [expr {($register(X)-1) & 0xff}]

    set flags(Z) [expr {$register(X)==0}]
    set flags(N) [expr {$register(X)>>7}]
  }
  proc DEY {} {
    variable register

    set register(X) [expr {($register(Y)-1) & 0xff}]

    set flags(Z) [expr {$register(Y)==0}]
    set flags(N) [expr {$register(Y)>>7}]
  }

  proc DEC {mode addr} {
    variable register

    set value [mem_get $addr $mode]
    incr value -1
    set value [expr {$value & 0xff}]
    mem_set $addr $value $mode

    set flags(Z) [expr {$value==0}]
    set flags(N) [expr {$value>>7}]
  }

  proc INC {mode addr} {
    variable register

    set value [mem_get $addr $mode]
    incr value 1
    set value [expr {$value & 0xff}]
    mem_set $addr $value $mode

    set flags(Z) [expr {$value==0}]
    set flags(N) [expr {$value>>7}]
  }

  proc PHA {} {
    variable register

    stack_push $register(A)
  }

  proc PLA {} {
    variable register
    variable flags

    set register(A) [stack_pop]

    set flags(Z) [expr {$register(A)==0}] 
    set flags(N) [expr {$register(A)>>7}] 
    
  }

  proc PHP {} {
    variable flags
    variable register

    stack_push [array get flags]
  }

  # Pull Processor Status
  proc PLP {} {
    variable flags
    variable register

    array set flags [stack_pop]
  }


  proc AND {mode addr} {
    variable flags
    variable register 

    set value [mem_get $addr $mode]

    set register(A) [expr {$register(A) & $value}] 

    set flags(Z) [expr {$register(A)==0}] 
    set flags(N) [expr {$register(A)>>7}] 
  }

  proc CMP {mode addr} {
    variable register
    variable flags 

    set value  [mem_get $addr $mode]
    set result [expr {($register(A)-$value) & 0xff}]

    set flags(Z) [expr {$register(A)==$value}] 
    set flags(C) [expr {$register(A)>=$value}] 
    set flags(N) [expr {$result>>7}] 
  }
  proc CPX {mode addr} {
    variable register
    variable flags 

    set value  [mem_get $addr $mode]
    set result [expr {($register(X)-$value) & 0xff}]

    set flags(Z) [expr {$register(X)==$value}] 
    set flags(C) [expr {$register(X)>=$value}] 
    set flags(N) [expr {$result>>7}] 
  }

  proc CPY {mode addr} {
    variable register
    variable flags 

    set value [mem_get $addr $mode]
    set result [expr {($register(Y)-$value) & 0xff}]

    set flags(Z) [expr {$register(Y)==$value}] 
    set flags(C) [expr {$register(Y)>=$value}] 
    set flags(N) [expr {$result>>7}] 
  }

  proc EOR {mode addr} {
    variable flags
    variable register 

    set value [mem_get $addr $mode]

    set register(A) [expr {$register(A) ^ $value}] 
    set result $register(A)
    set flags(Z) [expr {$result==0}] 
    set flags(N) [expr {$result>>7}]
  }

  proc ORA {mode addr} {
    variable flags
    variable register 

    set value [mem_get $addr $mode]

    set register(A) [expr {$register(A) | $value}] 
    set result $register(A)
    set flags(Z) [expr {$result==0}] 
    set flags(N) [expr {$result>>7}]
  }

  proc AND {mode addr} {
    variable flags
    variable register 

    set value [mem_get $addr $mode]

    set register(A) [expr {$register(A) & $value}] 
    set result $register(A)
    set flags(Z) [expr {$result==0}] 
    set flags(N) [expr {$result>>7}]
  }

  proc ROR {mode {addr ""}} {
    variable register
    variable flags

    switch -- $mode {
      "-reg-a" {
        set value $register(A)
        set result [expr {($value>>1) | ($flags(C)<<7)}]
        set register(A) $result 
      }
      "-zero" {
        set value [mem_get $addr]
        set result [expr {($value>>1) | ($flags(C)<<7)}]
        mem_set $addr $result
      }
      "-zero-x" {
        set addr  [expr {($register(X)+$addr) & 0xff}]
        set value [mem_get $addr]
        set result [expr {($value>>1) | ($flags(C)<<7)}]
        mem_set $addr $result
      }
      "-addr" {
        set value [mem_get $addr]
        set result [expr {($value>>1) | ($flags(C)<<7)}]
        mem_set $addr $result
      }
      "-addr-x" {
        set addr   [expr {($register(X)+$addr)}]
        set value  [mem_get $addr]
        set result [expr {($value>>1) | ($flags(C)<<7)}]
        mem_set $addr $result
      }
      default {
        error "unknown ROR $mode"
      }
    }

    set flags(C) [expr {$value & 0x01}]
    set flags(Z) [expr {$result==0}] 
    set flags(N) [expr {$result>>7}]
  }

  proc LSR {mode {addr ""}} {
    variable register

    switch -- $mode {
      "-reg-a" {
        set value $register(A)
        set result [expr {$value>>1}]
        set register(A) $result 
      }
      "-zero" {
        set value [mem_get $addr]
        set result [expr {$value>>1}]
        mem_set $addr $result
      }
      "-zero-x" {
        set addr  [expr {($register(X)+$addr) & 0xff}]
        set value [mem_get $addr]
        set result [expr {$value>>1}]
        mem_set $addr $result
      }
      "-addr" {
        set value [mem_get $addr]
        set result [expr {$value>>1}]
        mem_set $addr $result
      }
      "-addr-x" {
        set addr   [expr {($register(X)+$addr)}]
        set value  [mem_get $addr]
        set result [expr {$value>>1}]
        mem_set $addr $result
      }
      default {
        error "unknown LSR $mode"
      }
    }

    set flags(C) [expr {$value & 0x01}]
    set flags(Z) [expr {$result==0}] 
    set flags(N) [expr {$result>>7}]
  }

  proc ASL {mode {addr ""}} {
    variable register

    switch -- $mode {
      "-reg-a" {
        set value $register(A)
        set result [expr {$value<<1}]
        set register(A) $result 
      }
      "-zero" {
        set value [mem_get $addr]
        set result [expr {$value<<1}]
        mem_set $addr $result
      }
      "-zero-x" {
        set addr  [expr {($register(X)+$addr) & 0xff}]
        set value [mem_get $addr]
        set result [expr {$value<<1}]
        mem_set $addr $result
      }
      "-addr" {
        set value [mem_get $addr]
        set result [expr {$value<<1}]
        mem_set $addr $result
      }
      "-addr-x" {
        set addr   [expr {($register(X)+$addr)}]
        set value  [mem_get $addr]
        set result [expr {$value<<1}]
        mem_set $addr $result
      }
      default {
        error "unknown ASL $mode"
      }
    }

    set flags(C) [expr {($value & 0x80)>>7}]
    set flags(Z) [expr {$result==0}] 
    set flags(N) [expr {$result>>7}]
  }

  proc LDA {mode addr} {
    variable register
    variable flags

    set value [mem_get $addr $mode]

    set register(A) $value
    set flags(Z) [expr {$register(A)==0}] 
    set flags(N) [expr {$register(A)>>7}]
  }

  proc LDX {mode addr} {
    variable register
    variable flags

    set value [mem_get $addr $mode]

    set register(X) $value
    set flags(Z) [expr {$register(X)==0}] 
    set flags(N) [expr {$register(X)>>7}]
  }

  proc LDY {mode addr} {
    variable register
    variable flags

    set value [mem_get $addr $mode]

    set register(Y) $value
    set flags(Z) [expr {$register(Y)==0}] 
    set flags(N) [expr {$register(Y)>>7}]
  }

  proc DEX {} {
    variable register
    incr register(X) -1

    set flags(Z) [expr {$register(X)==0}] 
    set flags(N) [expr {$register(X)>>7}]
  }

  proc DEY {} {
    variable register

    incr register(Y) -1

    set flags(Z) [expr {$register(Y)==0}] 
    set flags(N) [expr {$register(Y)>>7}]
  }

  proc STA {mode addr} {
    variable flags
    variable register

    mem_set $addr $register(A) $mode
  }

  proc STX {mode addr} {
    variable register

    mem_set $addr $register(X) $mode
  }

  proc STY {mode addr} {
    variable register

    mem_set $addr $register(Y) $mode
  }

}

proc nescpu::start {addr} {
  variable register 

  set register(PC) $addr

  while 1 {
    set byte [rom_addr $register(PC) "cu"]

    set opcode [nescpu::opcode $byte] 

    if {$opcode eq ""} {
      puts "unknown opcode [format %2x $byte]"
      break
    }

    set opcode_args [lindex $opcode 1 1]
    set opcode_cmd  [lindex $opcode 1 0]

    set opcode_argv [list]
    set addr $register(PC)
    incr addr
    foreach arg $opcode_args {
      switch -- $arg {
        "int8" {
          set arg_value [rom_addr $addr "cu"]
          incr addr
          lappend opcode_argv $arg_value
        }
        "int16" {
          set arg_value [rom_addr $addr "su"]
          incr addr 2
          lappend opcode_argv $arg_value
        }
        default {
          puts "unknown arg type $arg"
        }
      }
    }


    puts [format "\$%04x: %s %s" $register(PC) $opcode $opcode_argv]
    set register(PC) $addr
    incr register(PC) -1

    nescpu::[lindex $opcode_cmd 0] {*}[lindex $opcode_cmd 1] {*}$opcode_argv

    incr register(PC)

    # incr start_addr
  }
}

return
########################################################################

## Memory


Address      	Size	Device
-----------
$0000-$07FF	$0800	2KB internal RAM
-----------
$0800-$0FFF	$0800	Mirrors of $0000-$07FF
$1000-$17FF	$0800   Mirrors of $0000-$07FF
$1800-$1FFF	$0800
-----------
$2000-$2007	$0008	NES PPU registers
-----------
$2008-$3FFF	$1FF8	Mirrors of $2000-2007 (repeats every 8 bytes)
-----------
$4000-$4017	$0018	NES APU and I/O registers
-----------
$4018-$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
-----------
$4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)
-----------
See Sample RAM map for an example allocation strategy for the 2KB of internal RAM at $0000-$0800.

Note: Most common boards and iNES mappers address ROM and Save/Work RAM in this format:

$6000-$7FFF = Battery Backed Save or Work RAM
$8000-$FFFF = Usual ROM, commonly with Mapper Registers (see MMC1 and UxROM for example)

The CPU expects interrupt vectors in a fixed place at the end of the cartridge space:

## Interrupt
------------

$FFFA/B = NMI vector
$FFFC/D = Reset vector
$FFFE/F = IRQ/BRK vector

16KB banking mapper: $C000-$FFFF 


NTSC NES
---------
Master Clock:  21.477272 MHz
Clock Divisor: 12
CPU Clock:     1.789773 MHz (~559 ns per cycle)
Color subcarrier frequency fsc (approx.)	3.579545 MHz (~1118 ns)

Emulator authors may wish to emulate the NTSC NES/Famicom CPU at

 21441960 Hz ((341×262-0.5)×4×60) to ensure a synchronised/stable 60 frames per second.[1]
 21.441960 MHz





start_addr = c070
$c070: 0x78 {SEI ""       2 "Set Interrupt Disable"}
$c071: 0xa9 {{LDA -value}  "int8"   2 "Load Accumulator - Immediate Addr"} 16
$c073: 0x8d {{STA -addr}   "int16"  4 "Store Accumulator - Absolute Addr"} 8192
write addr = 2000 0010
$c076: 0xd8 {CLC ""       2 "Clear Carry Flag"}
$c077: 0xa2 {{LDX -value}  "int8"   2 "Load X Regsiter - Immediate Addr"} 2
$c079: 0xad {{LDA -addr}   "int16"  4 "Load Accumulator - Absolute"} 8194
read addr = 2002 0000
$c07c: 0x10 {BPL "int8"   2 "Branch if Positive"} 251
debug: branch if positive c07c+00fb = c177
$c07e: 0xa9 {{LDA -value}  "int8"   2 "Load Accumulator - Immediate Addr"} 6
$c080: 0x8d {{STA -addr}   "int16"  4 "Store Accumulator - Absolute Addr"} 8193
write addr = 2001 0006
$c083: 0xca {DEX ""                2 "Decrement X Register"}
$c084: 0xd0 {BNE "int8"   2 "Branch if Not Equal"} 243
debug: branch if positive 243
$c086: 0xa2 {{LDX -value}  "int8"   2 "Load X Regsiter - Immediate Addr"} 127
$c088: 0x9a {TXS ""       2 "Transfer X to Stack Pointer"}



start_addr = c070
$c070: 0x78 {SEI ""       2 "Set Interrupt Disable"}
$c071: 0xa9 {{LDA -value}  "int8"   2 "Load Accumulator - Immediate Addr"} 16
$c073: 0x8d {{STA -addr}   "int16"  4 "Store Accumulator - Absolute Addr"} 8192
write addr = 2000 0010
$c076: 0xd8 {CLC ""       2 "Clear Carry Flag"}
$c077: 0xa2 {{LDX -value}  "int8"   2 "Load X Regsiter - Immediate Addr"} 2
$c079: 0xad {{LDA -addr}   "int16"  4 "Load Accumulator - Absolute"} 8194
read addr = 2002 0000
$c07c: 0x10 {BPL "int8"   2 "Branch if Positive"} 251
debug: branch if positive c07e+00fb = c179
$c179: 0xa5 {{LDA -zero}   "int8"   2 "Load Accumulator - Zero Page"} 8
$c17b: 0x29 {AND "int8"   2 "Logical AND - Immediate"} 8
$c17d: 0xd0 {BNE "int8"   2 "Branch if Not Equal"} 70
$c17f: 0xa5 {{LDA -zero}   "int8"   2 "Load Accumulator - Zero Page"} 8
$c181: 0x29 {AND "int8"   2 "Logical AND - Immediate"} 1
$c183: 0xd0 {BNE "int8"   2 "Branch if Not Equal"} 12
$c185: 0xa5 {{LDA -zero}   "int8"   2 "Load Accumulator - Zero Page"} 6
$c187: 0x29 {AND "int8"   2 "Logical AND - Immediate"} 1
$c189: 0xf0 {BEQ "int8"   2 "Branch if Equal"} 25
debug: branch if equale 25

