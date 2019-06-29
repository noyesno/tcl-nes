
namespace eval nesppu {
  variable patterns
  variable CHG_ROM

  proc load-rom {data} {
    variable CHG_ROM
    set CHG_ROM $data
  }
}

proc nesppu::load-bitmap {{zoom 2}} {
  set addr 0x0000
  set idx  0
  foreach pattern [nesppu::load-pattern $addr $zoom] {
    set name [format "nespattern_%d" [expr {$addr+$idx}]]
    set bitmap_data [bits2bitmap $pattern]
    set bitmap [image create bitmap $name -data $bitmap_data -foreground teal -background ""]
    incr idx
  }

  set addr 0x1000
  set idx  0
  foreach pattern [nesppu::load-pattern $addr $zoom] {
    set name [format "nespattern_%d" [expr {$addr+$idx}]]
    set bitmap_data [bits2bitmap $pattern]
    set bitmap [image create bitmap $name -data $bitmap_data -foreground teal -background ""]
    incr idx
  }
  return
}

proc nesppu::load-pattern {addr {zoom 1}} {
  variable CHG_ROM

  set result [list]
  for {set idx 0} {$idx<=0xff} {incr idx} {
	binary scan $CHG_ROM "x[expr $addr]x[expr {$idx<<4}]cu8cu8" bit0_bytes bit1_bytes

        set data [list]
	foreach v0 $bit0_bytes v1 $bit1_bytes {
          set row [list]
	  for {set i 7} {$i>=0} {incr i -1} {
	    set v [expr {(($v0>>$i) & 0x01) | ((($v1>>$i) & 0x01)<<1)}]

            lappend row {*}[lrepeat $zoom $v]
	  }
          lappend data {*}[lrepeat $zoom $row]
	}
        lappend result $data
  }

  return $result
}


return
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Memory

Size: 16 KB

===========
$0000-$0fff  # pattern table 0    # usually from CHR-ROM on game cartridge
$1000-$1fff  # pattern table 1
-----------
$2000-$23ff  # nametable 0        # Usually mapped to dedicated RAM on NES board
$2400-$27ff  # nametable 1
$2800-$2bff  # nametable 2
$2c00-$2fff  # nametable 3
-----------
$3000-$3eff  # mirror nametable 
-----------
$3f00-$3f1f  # palette            # always mapped to internal palette control
-----------
$3f20-$3fff  # mirror palette
===========

OAM (Object Attibute Memory)

Size: 256 byte



PPU NameTable

256*240 = 61440 pixel 
32*30   = 960   tile  / pattern 
$400            byte  / 1KB
$2000-$23BF     32*30 tiles
$23C0-$23FF     64 palette, each pallete cover 4 tiles

+-------+-------+
| $2000 | $2400 |
+-------+-------+
| $2800 | $2c00 |
+-------+-------+



## PPUCTRL $2000 - Write

7  bit  0
---- ----
VPHB SINN
|||| ||||
|||| ||++- Base nametable address
|||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
|||| |+--- VRAM address increment per CPU read/write of PPUDATA
|||| |     (0: add 1, going across; 1: add 32, going down)
|||| +---- Sprite pattern table address for 8x8 sprites
||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
|||+------ Background pattern table address (0: $0000; 1: $1000)
||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
|+-------- PPU master/slave select
|          (0: read backdrop from EXT pins; 1: output color on EXT pins)
+--------- Generate an NMI at the start of the
           vertical blanking interval (0: off; 1: on)


## PPUMASK  $2001 - Write
7  bit  0
---- ----
BGRs bMmG
|||| ||||
|||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
|||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
|||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
|||| +---- 1: Show background
|||+------ 1: Show sprites
||+------- Emphasize red
|+-------- Emphasize green
+--------- Emphasize blue


## PPUSTATUS $2002 - Read

7  bit  0
---- ----
VSO. ....
|||| ||||
|||+-++++- Least significant bits previously written into a PPU register
|||        (due to register not being updated for this address)
||+------- Sprite overflow. The intent was for this flag to be set
||         whenever more than eight sprites appear on a scanline, but a
||         hardware bug causes the actual behavior to be more complicated
||         and generate false positives as well as false negatives; see
||         PPU sprite evaluation. This flag is set during sprite
||         evaluation and cleared at dot 1 (the second dot) of the
||         pre-render line.
|+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
|          a nonzero background pixel; cleared at dot 1 of the pre-render
|          line.  Used for raster timing.
+--------- Vertical blank has started (0: not in vblank; 1: in vblank).
           Set at dot 1 of line 241 (the line *after* the post-render
           line); cleared after reading $2002 and at dot 1 of the
           pre-render line.
