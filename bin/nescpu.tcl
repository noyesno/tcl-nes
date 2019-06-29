source src/nescpu.tcl

lassign $::argv rom_file
set fp [open $rom_file "rb"]
set header [read $fp 16]
binary scan $header {a4cc} magic n_prg n_chg

set PRG_ROM [read $fp [expr {($n_prg*16)<<10}]]
set CHG_ROM [read $fp [expr {($n_chg*8)<<10}]]

# binary scan $prg_rom {uc*} bytes

# $C000-$FFFF


proc rom_addr {addr {field "c"}} {
  global PRG_ROM

  # puts "rom_addr [format {$%02x} $addr] $field"

  set pos [expr {$addr-0x0C000}]
  # puts "skip pos = $pos [string length $PRG_ROM]"
  binary scan $PRG_ROM "@$pos$field" result 
  return $result
}


set start_addr [rom_addr 0xfffc "su"]
puts [format "start_addr = %04x" $start_addr]

nescpu::reset
nescpu::start $start_addr






