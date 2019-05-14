-- File Name: MAX10ADC.tb.vhd
-- Title: Testbench
-- Version: 1.0
-- Date: 2019/04/14
-- Author: Miguel A. Risco Castillo
-- Description: Vhdl Test Bench for IP Core MAX10ADC
--------------------------------------------------------------------------------

LIBRARY ieee;                                               
USE ieee.std_logic_1164.all;                                
use ieee.numeric_std.all;

ENTITY MAX10ADC_Test IS
END MAX10ADC_Test;

ARCHITECTURE MAX10ADC_arch OF MAX10ADC_Test IS

SIGNAL ADR_I : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others=>'0');
SIGNAL CLK_I : STD_LOGIC;
SIGNAL DAT_O : STD_LOGIC_VECTOR(15 DOWNTO 0);
SIGNAL RST_I : STD_LOGIC := '1';
SIGNAL STB_I : STD_LOGIC := '0';
SIGNAL WE_I : STD_LOGIC  := '0';

SIGNAL nCS   : std_logic;        -- chipselect active low
SIGNAL MISO  : std_logic:='1';   -- Master In Slave Out
SIGNAL SCK   : std_logic;        -- SPI Clock

CONSTANT freq : positive := 10E6;
CONSTANT CLKPERIOD : time := (real(1000000000)/real(freq)) * 1 ns;

COMPONENT MAX10AD1
port(
-- SBA Interface
   RST_I : in  std_logic;        -- active high reset
   CLK_I : in  std_logic;        -- Main clock
   STB_I : in  std_logic;        -- Strobe
   WE_I  : in  std_logic;        -- Bus write, active high
   ADR_I : in  std_logic_vector; -- Register AD0/AD1 selector
   DAT_O : out std_logic_vector  -- Data output Bus
);
END COMPONENT;


BEGIN
	i1 : MAX10AD1
    PORT MAP (
-- list connections between master ports and signals
	ADR_I => ADR_I,
	CLK_I => CLK_I,
	DAT_O => DAT_O,
	RST_I => RST_I,
	STB_I => STB_I,
	WE_I  => WE_I
	);

reset : PROCESS
BEGIN
  report "CLK PERIOD: " & time'image(CLKPERIOD);
  RST_I<='1';
  WAIT FOR 4 * CLKPERIOD;
  RST_I<='0';
  WAIT;
END PROCESS reset;


clkproc : PROCESS
BEGIN
  CLK_I <= '0';
  WAIT FOR CLKPERIOD/2;
  CLK_I <= '1';
  WAIT FOR CLKPERIOD/2;
END PROCESS clkproc;

control : PROCESS
BEGIN
  wait for 200 ms;                               -- Power-Up Time
  STB_I <= '1';                                  -- Select Core
  WE_I  <= '0';                                  -- Enable read from bus
  ADR_I(0 downto 0) <= "0";                      -- Select channel 0
  wait for 250 us;
  ADR_I(0 downto 0) <= "1";                      -- Select channel 1
  WAIT;
END PROCESS control;


END MAX10ADC_arch;  
