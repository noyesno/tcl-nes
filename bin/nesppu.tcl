
source src/nescpu.tcl
source src/nesppu.tcl

lassign $::argv rom_file
set fp [open $rom_file "rb"]
set header [read $fp 16]
binary scan $header {a4cc} magic n_prg n_chg

set PRG_ROM [read $fp [expr {($n_prg*16)<<10}]]
set CHG_ROM [read $fp [expr {($n_chg*8)<<10}]]


proc print_pattern {} {
for {set idx 0} {$idx<=0xff} {incr idx} {
	binary scan $CHG_ROM "x[expr {$idx<<4}]cu8cu8" bit0 bit1

	foreach v0 $bit0 v1 $bit1 {
	  for {set i 7} {$i>=0} {incr i -1} {
	    set v [expr {($v0>>$i) & 0x01 | (($v1>>$i) & 0x01)<<1}]

	    if {$v==0} {
	      puts -nonewline " " 
	    } elseif {$v==1} {
	      puts -nonewline "." 
	    } elseif {$v==2} {
	      puts -nonewline "+" 
	    } else {
	      puts -nonewline "#"
	    }


	  }
	  puts ""
	}
}
}





proc bitmap_pattern {CHG_ROM} {
  puts {
  <style>
  table {
     margin-bottom:1em;
     border-spacing:0;
     border:1px solid black;
  }
  .bit-0, .bit-1, .bit-2, .bit-3  {
    width:4px;height:4px;
  }
  .bit-1  {background-color:#c99}
  .bit-2  {background-color:#c33}
  .bit-3  {background-color:#c66}
  </style> 
  }
  for {set idx 0} {$idx<=0xff} {incr idx} {
	binary scan $CHG_ROM "x[expr {$idx<<4}]cu8cu8" bit0 bit1

        set data [list]
	foreach v0 $bit0 v1 $bit1 {
          set row [list]
	  for {set i 7} {$i>=0} {incr i -1} {
	    set v [expr {($v0>>$i) & 0x01 | (($v1>>$i) & 0x01)<<1}]

            lappend row {*}[lrepeat 4 $v]
	  }
          lappend data {*}[lrepeat 4 $row]
	}

        if {1} {
          set bitmap_data [bitmap_data timage$idx $data]
          puts $bitmap_data
          set img [image create bitmap timage$idx -data $bitmap_data]  
          # $img configure -data $bitmap_data
          # return
        }

        set n 0
        puts "#$idx [llength $data] = $data"
        puts "<table>"
        foreach bit $data {
          if {$n%8==0} {
            puts "<tr>"
          }
          if {$bit} {
            puts "<td class='bit-$bit'></td>"
          } else {
            puts "<td class='bit-$bit'></td>"
          }

          if {$n%8==7} {
            puts "</tr>"
          }
          incr n
        }
        puts "</table><hr/>"
  }
  # $1xxf
  for {set idx 0} {$idx<=0xff} {incr idx} {
	binary scan $CHG_ROM "x[expr 0xfff]x[expr {$idx<<4}]cu8cu8" bit0 bit1

        set data [list]
	foreach v0 $bit0 v1 $bit1 {
	  for {set i 7} {$i>=0} {incr i -1} {
	    set v [expr {($v0>>$i) & 0x01 | (($v1>>$i) & 0x01)<<1}]

            lappend data $v
            # lappend data $v $v
	  }
	}

        if {0} {
          set bitmap_data [bitmap_data timage$idx 8 8 $data]
          puts $bitmap_data
          set img [image create bitmap timage$idx -data $bitmap_data]  
          # $img configure -data $bitmap_data
        }

        set n 0
        puts "#$idx [llength $data] = $data"
        puts "<table>"
        foreach bit $data {
          if {$n%8==0} {
            puts "<tr>"
          }
          if {$bit} {
            puts "<td class='bit-$bit'></td>"
          } else {
            puts "<td class='bit-$bit'></td>"
          }

          if {$n%8==7} {
            puts "</tr>"
          }
          incr n
        }
        puts "</table><hr/>"
  }
}

proc bits2bitmap {data} {

    set width  [llength [lindex $data 0]]
    set height [llength $data]
    
    set name "nes"

    set bmp ""
    append bmp "#define ${name}_width $width\n"
    append bmp "#define ${name}_height $height\n"
    append bmp "static char ${name}_bits[] = \{\n"
    
    set bytes {}

    foreach row $data {
      set n 0 
      # use big endian
      foreach bit $row {
        if {$n%8==0} {
          set byte 0
        }
        if {$bit} {
          # set byte [expr {($byte<<1)|0x01}]
          set byte [expr {($byte>>1)|0x80}]
        } else {
          # set byte [expr {($byte<<1)|0x00}]
          set byte [expr {($byte>>1)|0x00}]
        }
        incr n 
        if {$n%8==0} {
          lappend bytes [format 0x%02x $byte]
        }
      }
    }

    append bmp "    " [join $bytes ", "]
    append bmp "\n\}"
    
    return $bmp
}

# bitmap_pattern $CHG_ROM

nesppu::load-rom $CHG_ROM

nesppu::load-bitmap

canvas .canvas
pack .canvas -fill both -expand 1

proc draw-pattern {x y {step 24}} { 
  set cx $x
  set cy $y
  set step 48
  for {set r 0} {$r<15} {incr r} {
    set cx $x
    for {set c 0} {$c<15} {incr c} {
      set idx [expr {($r<<4) + $c + 0x1000}]
      .canvas create image $cx $cy -image nespattern_$idx
	
      incr cx $step 
    }
    incr cy $step
  }
}

proc draw-nametable {} { 

  binary scan $nesppu::CHG_ROM "x[expr 0x2000]cu960" tiles

  puts $tiles

  set x 10 
  set y 10
  set n 0
  foreach tile $tiles {
      set idx [expr {$tile + 0x1000}]
      .canvas create image $x $y -image nespattern_$idx
      incr n
       
      if {$n%32==0} {
        set  x 10
        incr y 24
      } else {
        incr x 24
      } 
  }

}

draw-pattern 64 64
# draw-nametable

#  .canvas scale all 0 0 20 20 

tkwait window .

exit


