onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /adfmax31856_test/RSTi
add wave -noupdate /adfmax31856_test/CLKi
add wave -noupdate /adfmax31856_test/STBi
add wave -noupdate /adfmax31856_test/WEi
add wave -noupdate /adfmax31856_test/i1/state
add wave -noupdate -radix hexadecimal /adfmax31856_test/i1/streami
add wave -noupdate -radix hexadecimal /adfmax31856_test/i1/streamo
add wave -noupdate -radix hexadecimal /adfmax31856_test/i1/SPIRD
add wave -noupdate -radix hexadecimal /adfmax31856_test/i1/SPIWR
add wave -noupdate /adfmax31856_test/i1/SPItr
add wave -noupdate /adfmax31856_test/i1/SCK
add wave -noupdate /adfmax31856_test/nCS
add wave -noupdate /adfmax31856_test/i1/MOSI
add wave -noupdate /adfmax31856_test/i1/MISO
add wave -noupdate /adfmax31856_test/i1/SPIState/SCKN
add wave -noupdate -radix hexadecimal /adfmax31856_test/i1/DAT_O
add wave -noupdate -radix hexadecimal /adfmax31856_test/RESULT
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {3673407 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 244
configure wave -valuecolwidth 40
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits us
update
WaveRestoreZoom {0 ps} {21 us}
