-- File Name: ADFMAX31856.tb.vhd
-- Title: Testbench
-- Version: 1.0
-- Date: 2019/07/08
-- Author: Miguel A. Risco Castillo
-- Description: Vhdl Test Bench for IP Core ADFMAX31856
--------------------------------------------------------------------------------

LIBRARY ieee;                                               
USE ieee.std_logic_1164.all;                                
use ieee.numeric_std.all;

ENTITY ADFMAX31856_test IS
END ADFMAX31856_test;

ARCHITECTURE ADFMAX31856_arch OF ADFMAX31856_test IS

SIGNAL ADR_I : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others=>'0');
SIGNAL CLK_I : STD_LOGIC;
SIGNAL DAT_O : STD_LOGIC_VECTOR(15 DOWNTO 0);
SIGNAL DAT_I : STD_LOGIC_VECTOR(15 DOWNTO 0);
SIGNAL RST_I : STD_LOGIC := '1';
SIGNAL STB_I : STD_LOGIC := '0';
SIGNAL WE_I : STD_LOGIC  := '0';

SIGNAL nCS   : std_logic;        -- chipselect active low
SIGNAL MISO  : std_logic:='1';   -- Master In Slave Out
SIGNAL MOSI  : std_logic;        -- Master Out Slave In
SIGNAL SCK   : std_logic;        -- SPI Clock

CONSTANT freq : positive := 50E6;
CONSTANT CLKPERIOD : time := (real(1000000000)/real(freq)) * 1 ns;

COMPONENT ADFMAX31856
generic(
  debug:positive:=1;
  sysfreq:positive:=freq
);
port(
-- SBA Interface
   RST_I : in  std_logic;        -- active high reset
   CLK_I : in  std_logic;        -- Main clock
   STB_I : in  std_logic;        -- Strobe
   WE_I  : in  std_logic;        -- Bus write, active high
   ADR_I : in  std_logic_vector; -- Register AD0/AD1 selector
   DAT_O : out std_logic_vector; -- Data output Bus
   DAT_I : in  std_logic_vector; -- Data input Bus
-- Interface for ADFMAX31856
   nCS   : out std_logic;        -- Chipselect active low
   MISO  : in  std_logic;        -- Master In / Slave Out (SDO in MAX31856)
   MOSI  : out std_logic;        -- Master Out / Slave In (SDI in MAX31856)
   SCK   : out std_logic         -- SPI Clock
);
END COMPONENT;


BEGIN
	i1 : ADFMAX31856
    PORT MAP (
-- list connections between master ports and signals
	ADR_I => ADR_I,
	CLK_I => CLK_I,
	DAT_O => DAT_O,
    DAT_I => DAT_I,
	RST_I => RST_I,
	STB_I => STB_I,
	WE_I  => WE_I,
 --
    nCS  => nCS,
    MISO => MISO,
    MOSI => MOSI,
    SCK  => SCK
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


END ADFMAX31856_arch;
