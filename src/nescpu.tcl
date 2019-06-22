
# Ref: http://obelisk.me.uk/6502/reference.html

namespace eval nescpu {
  variable flags
  variable register

  # I : interrupt disable

  variable opcode

  # byte {OPCODE bytes cycle note} 
  array set opcode_schema {
    0x48 {PHA ""       3 "Push Accumulator Regsiter"}
    0x08 {PHP ""       3 "Push Processor Status"}
    0x68 {PLA ""       3 "Pull Accumulator Regsiter"}
    0x28 {PLP ""       3 "Pull Processor Status"}

    0x10 {BPL "int8"   2 "Branch if Positive"}
    0x29 {AND "int8"   2 "Logical AND - Immediate"}
    0xd0 {BNE "int8"   2 "Branch if Not Equal"}
    0xf0 {BEQ "int8"   2 "Branch if Equal"}
    0x20 {JSR "int16"  2 "Jump to Subroutine"}
    0x60 {RTS ""       6 "Return from Subroutine"}
    0xaa {TAX ""       2 "Transfer Accumulator to X"}
    0xa8 {TAY ""       2 "Transfer Accumulator to Y"}
    0xba {TSX ""       2 "Transfer Stack Pointer to X"}
    0x8a {TXA ""       2 "Transfer X to Accumulator"}
    0x9a {TXS ""       2 "Transfer X to Stack Pointer"}
    0x98 {TYA ""       2 "Transfer Y to Accumulator"}
    0xe8 {INX ""       2 "Increment X Register"}
    0xc8 {INY ""       2 "Increment X Register"}

    0x49 {{EOR -value}      "int8"   2 "Exclusive OR"}
    0x45 {{EOR -zero}       "int8"   2 "Exclusive OR"}
    0x55 {{EOR -zero-x}     "int8"   2 "Exclusive OR"}
    0x4d {{EOR -addr}       "int16"  2 "Exclusive OR"}
    0x5d {{EOR -addr-x}     "int16"  2 "Exclusive OR"}
    0x59 {{EOR -addr-y}     "int16"  2 "Exclusive OR"}
    0x41 {{EOR -indirect-x} "int8"  2 "Exclusive OR"}
    0x51 {{EOR -indirect-y} "int8"  2 "Exclusive OR"}

    0x69 {{ADC -value}      "int8"   2 "Add with Carry"}
    0x65 {{ADC -zero}       "int8"   2 "Add with Carry"}
    0x75 {{ADC -zero-x}     "int8"   2 "Add with Carry"}
    0x6d {{ADC -addr}       "int16"  2 "Add with Carry"}
    0x7d {{ADC -addr-x}     "int16"  2 "Add with Carry"}

    0xc9 {{CMP -value}      "int8"   2 "Compare"}
    0xc5 {{CMP -zero}       "int8"   2 "Compare"}
    0xd5 {{CMP -zero-x}     "int8"   2 "Compare"}
    0xcd {{CMP -addr}       "int16"  2 "Compare"}
    0xdd {{CMP -addr-x}     "int16"  2 "Compare"}
    0xd9 {{CMP -addr-y}     "int16"  2 "Compare"}

    0x4a {{LSR -A }     ""      2 "Logical Shift Right"}
    0x46 {{LSR -zero}   "int8"  5 "Logical Shift Right"}
    0x56 {{LSR -zero-x} "int8"  5 "Logical Shift Right"}
    0x4e {{LSR -addr}   "int16" 5 "Logical Shift Right"}
    0x5e {{LSR -addr-x} "int16" 5 "Logical Shift Right"}

    0x78 {SEI ""       2 "Set Interrupt Disable"}
    0xd8 {CLD ""       2 "Clear Decimal Mode"}

    0xca {DEX ""                2 "Decrement X Register"}
    0x88 {DEY ""                2 "Decrement Y Register"}
    0xa9 {{LDA -value} "int8"   2 "Load Accumulator - Immediate Addr"}
    0xa5 {{LDA -zero}  "int8"   2 "Load Accumulator - Zero Page"}
    0xad {{LDA -addr}  "int16"  4 "Load Accumulator - Absolute"}

    0xa2 {{LDX -value}  "int8"   2 "Load X Regsiter - Immediate Addr"}
    --a6 {{LDX -zero}   "int8"   3 "Load X Regsiter - Zero Page"}
    --b6 {{LDX -zero-x} "int8"   4 "Load X Regsiter - Zero Page, Y"}
    --ae {{LDX -addr}   "int16"  4 "Load X Regsiter - Absolute"}
    --be {{LDX -addr-y} "int16"  4 "Load X Regsiter - Absolute, Y"}

    0x85 {{STA -zero}   "int8"   3 "Store Accumulator - Zero Page"}
    0x95 {{STA -zero-x} "int8"   3 "Store Accumulator - Zero Page,X"}
    0x8d {{STA -addr}   "int16"  4 "Store Accumulator - Absolute Addr"}
    0x9d {{STA -addr-x} "int16"  4 "Store Accumulator - Absolute Addr, X"}
    0x99 {{STA -addr-y} "int16"  4 "Store Accumulator - Absolute Addr, X"}
  }

  proc reset {} {
    variable flags
    variable register
    variable stack
    variable memory

    set stack [list]
    set memory [split [string repeat "0" 0xffff] ""]

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

  proc ADC {mode value} {
    variable register
    variable flags

    switch -- $mode {
      "-value" {
        set register(A) [expr {$register(A)+$value}]
      }
      "-zero" {
        set value [mem_get $value]
        set register(A) [expr {$register(A)+$value}]
      }
      "-zero-x" {
        set value [mem_get [expr {($register(X)+$value)&0xff}]]
        set register(A) [expr {$register(A)+$value}]
      }
      "-addr" {
        set value [mem_get $value]
        set register(A) [expr {$register(A)+$value}]
      }
      "-addr-x" {
        set value [mem_get [expr {($register(X)+$value)}]]
        set register(A) [expr {$register(A)+$value}]
      }
      "-addr-y" {
        set value [mem_get [expr {($register(Y)+$value)}]]
        set register(A) [expr {$register(A)+$value}]
      }
    }
  }

  proc CMP {mode value} {
    variable register
    variable flags

    switch -- $mode {
      "-value" {
        set flags(Z) [expr {$register(A)==$value}]
        set flags(C) [expr {$register(A)>=$value}]
      }
      "-zero" {
        set value [mem_get $value]
        set flags(Z) [expr {$register(A)==$value}]
        set flags(C) [expr {$register(A)>=$value}]
      }
      "-zero-x" {
        set value [mem_get [expr {($register(X)+$value)&0xff}]]
        set flags(Z) [expr {$register(A)==$value}]
        set flags(C) [expr {$register(A)>=$value}]
      }
      "-addr" {
        set value [mem_get $value]
        set flags(Z) [expr {$register(A)==$value}]
        set flags(C) [expr {$register(A)>=$value}]
      }
      "-addr-x" {
        set value [mem_get [expr {($register(X)+$value)}]]
        set flags(Z) [expr {$register(A)==$value}]
        set flags(C) [expr {$register(A)>=$value}]
      }
      "-addr-y" {
        set value [mem_get [expr {($register(Y)+$value)}]]
        set flags(Z) [expr {$register(A)==$value}]
        set flags(C) [expr {$register(A)>=$value}]
      }
    }
  }

  proc opcode {byte} {
    variable opcode_schema
    set opcode [format "0x%2x" $byte]

    set opcode [array get opcode_schema $opcode]
    return $opcode
  }

  proc opcode_exec {opcode} {
    
  }

  proc SEI {} {
    variable flags
    set flags(I) 1
  }

  proc CLD {} {
    variable flags
    set flags(D) 1
  }

  proc BPL {offset} {
    variable flags
    variable register

    if {$flags(N)} {
      # Negative
    } else {
      # Positive
      puts "debug: branch if positive $offset"
      incr register(PC) $offset
    } 
  }

  proc stack_push {value} {
     variable stack
     variable register

     if {$register(SP)<[llength $stack]} {
       lappend stack $register(SP) $value
     } else {
       lset stack $register(SP) $value
     }
     incr register(SP)
  }

  proc stack_pop {} {
     variable stack
     variable register

     incr register(SP) -1
     return [lindex $stack $register(SP)]
  }

  proc JSR {addr} {
    variable register

    stack_push [expr {$register(PC)+3-1}]
    set register(PC) $addr
  }

  proc RTS {} {
    variable register

    set register(PC) [stack_pop]
  }

  proc TAX {} {
    variable register

    set register(X) $register(A) 
  }
  proc TAY {} {
    variable register

    set register(Y) $register(A) 
  }

  proc INX {} {
    variable register

    incr register(X)

    if {$register(X)==0} {
      set flags(Z) 1
    }
    if {$register(X) & 0x80} {
      set flags(N) 1
    }
  }
  proc INY {} {
    variable register

    incr register(Y)
    if {$register(Y)==0} {
      set flags(Z) 1
    }
    if {$register(Y) & 0x80} {
      set flags(N) 1
    }
  }

  proc PHA {} {
    variable register

    stack_push $register(A)
  }

  proc PHP {} {
    variable flags
    variable register

    stack_push [array get flags)
  }

  proc PLA {} {
    variable register

    set register(A) stack_pop

    if {$register(A)==0} {
      set flags(Z) 1
    }
    if {$register(A) & 0x80} {
      set flags(N) 1
    }
  }

  # Pull Processor Status
  proc PLP {} {
    variable flags
    variable register

    set register(F) [stack_pop]
    array set flags $register(F)
  }

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

  proc BEQ {offset} {
    variable flags
    variable register

    if {$flags(Z)} {
      # Set
      puts "debug: branch if equale $offset"
      incr register(PC) $offset
    } else {
      # Clear 
    } 
  }

  proc AND {value} {
    variable flags
    variable register 

    set register(A) [expr {$register(A) & $value}] 

    if {$register(A)==0} {
      set flags(Z) 1
    }
    if {$register(A) & 0x80} {
      set flags(N) 1
    }
  }

  proc EOR {mode value} {
    variable flags
    variable register 

    switch -- $mode {
      "-value" {
        set register(A) [expr {$register(A) ^ $value}] 
      }
      "-zero" {
        set value [mem_get $value]
        set register(A) [expr {$register(A) ^ $value}] 
      }
      "-zero-x" {
        set value [mem_get [expr {($register(X)+$value)&0xff}]]
        set register(A) [expr {$register(A) ^ $value}] 
      }
      "-addr" {
        set value [mem_get $value]
        set register(A) [expr {$register(A) ^ $value}] 
      }
      "-indirect-x" {
        set value [mem_get [expr {($register(X)+$value)&0xff}]]
        set register(A) [expr {$register(A) ^ $value}] 
      }
      "-indirect-y" {
        set value [mem_get [expr {($register(Y)+$value)&0xff}]]
        set register(A) [expr {$register(A) ^ $value}] 
      }
      default {
        error "unknown LDA $mode"
      }
    }

    if {$register(A)==0} {
      set flags(Z) 1
    }
    if {$register(A) & 0x80} {
      set flags(N) 1
    }
  }

  proc LSR {mode value} {
    variable register

    switch -- $mode {
      "-zero" {
        set register(A) [expr {$register(A)>>[mem_get $value]}]
      }
      "-zero-x" {
        set register(A) [expr {$register(A)>>[mem_get [expr {($register(X)+$value) & 0xff}]]}]
      }
      "-addr" {
        set register(A) [expr {$register(A)>>[mem_get $value]}]
      }
      default {
        error "unknown LDA $mode"
      }
    }
    if {$register(X)==0} {
      set flags(Z) 1
    }
    if {$register(X) & 0x80} {
      set flags(N) 1
    }
  }

  proc LDA {mode value} {
    variable register

    switch -- $mode {
      "-value" {
        set register(A) $value 
      }
      "-zero" {
        set register(A) [mem_get $value]
      }
      "-addr" {
        set register(A) [mem_get $value]
      }
      "-addr-x" {
        set value [mem_get [expr {($register(X)+$value)}]]
        set register(A) $value
      }
      default {
        error "unknown LDA $mode"
      }
    }
  }

  proc LDX {mode value} {
    variable register

    switch -- $mode {
      "-value" {
        set register(X) $value 
      }
      "-zero" {
        set register(X) [mem_get $addr]
      } 
    }
  }

  proc DEX {} {
    variable register
    incr register(X) -1

    if {$register(X)==0} {
      set flags(Z) 1
    }
    if {$register(X) & 0x80} {
      set flags(N) 1
    }
  }

  proc DEY {} {
    variable register
    incr register(X) -1

    if {$register(X)==0} {
      set flags(Z) 1
    }
    if {$register(X) & 0x80} {
      set flags(N) 1
    }
  }

  proc mem_set {addr value} {
    variable memory

    lset memory $addr $value
  }
  proc mem_get {addr} {
    variable memory

    return [lindex $memory $addr]
  }

  proc STA {mode addr} {
    variable flags
    variable register

    switch -- $mode {
      "-zero" {
        mem_set $addr $register(A)
      }
      "-zero-x" {
        mem_set [expr {($register(X)+$addr)&0xff} $register(A)
      }
      "-addr" {
        mem_set $addr $register(A)
      }
      "-addr-x" {
        set addr [expr {($register(X)+$addr)}]
        mem_set $addr $register(A)
      }
      default {
        error "unknown LDA $addr"
      }
    }
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
    puts $opcode

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

    set register(PC) $addr

    nescpu::[lindex $opcode_cmd 0] {*}[lindex $opcode_cmd 1] {*}$opcode_argv
    incr start_addr
  }
}

