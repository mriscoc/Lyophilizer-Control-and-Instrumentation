#create input clock which is 10 MHz=100 ns
create_clock -name CLK_I -period 100 [get_ports {CLK_I}]

#derive PLL clocks 
derive_pll_clocks

#derive clock uncertainty 
derive_clock_uncertainty

#set false path 
set_false_path -from [get_ports {BTN}] 
set_false_path -from * -to [get_ports {LEDS*}]
