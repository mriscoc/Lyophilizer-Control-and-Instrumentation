-- File Name: ADFMAX31856.tb.vhd
-- Title: Testbench
-- Version: 2.0
-- Date: 2019/07/18
-- Author: Miguel A. Risco Castillo
-- Description: Vhdl Test Bench for IP Core ADFMAX31856
--------------------------------------------------------------------------------

LIBRARY ieee;                                               
USE ieee.std_logic_1164.all;                                
use ieee.numeric_std.all;

ENTITY ADFMAX31856_test IS
END ADFMAX31856_test;

ARCHITECTURE ADFMAX31856_arch OF ADFMAX31856_test IS

-- Master SBA signals
SIGNAL RSTi  : STD_LOGIC := '1';
SIGNAL CLKi  : STD_LOGIC;
SIGNAL STBi  : STD_LOGIC := '0';
SIGNAL WEi   : STD_LOGIC := '0';
SIGNAL DATIi : STD_LOGIC_VECTOR(15 DOWNTO 0);
SIGNAL DATOi : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others=>'0');
signal INTi  : std_logic;

SIGNAL nCS   : std_logic;        -- chipselect active low
SIGNAL MISO  : std_logic:='1';   -- Master In Slave Out
SIGNAL MOSI  : std_logic;        -- Master Out Slave In
SIGNAL SCK   : std_logic;        -- SPI Clock

CONSTANT freq : positive := 50E6;
CONSTANT CLKPERIOD : time := (real(1000000000)/real(freq)) * 1 ns;

-- Results
signal RESULT : STD_LOGIC_VECTOR(15 DOWNTO 0) := (others=>'0');
signal MAXADR : std_logic_vector(7 downto 0) := (others=>'0');
signal MAXDAT : std_logic_vector(15 downto 0) := (others=>'0');

-- Utility constants
constant SPIDATA : std_logic_vector(15 downto 0) := x"1234";

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
   DAT_O : out std_logic_vector; -- Data output Bus
   DAT_I : in  std_logic_vector; -- Data input Bus
   INT_O : out std_logic;        -- Interrupt (Data ready)
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
	RST_I => RSTi,
	CLK_I => CLKi,
	STB_I => STBi,
	WE_I  => WEi,
	DAT_O => DATIi,
    DAT_I => DATOi,
    INT_O => INTi,
 --
    nCS  => nCS,
    MISO => MISO,
    MOSI => MOSI,
    SCK  => SCK
	);

reset : PROCESS
BEGIN
  report "CLK PERIOD: " & time'image(CLKPERIOD);
  RSTi<='1';
  WAIT FOR 4 * CLKPERIOD;
  RSTi<='0';
  WAIT;
END PROCESS reset;


clkproc : PROCESS
BEGIN
  CLKi <= '0';
  WAIT FOR CLKPERIOD/2;
  CLKi <= '1';
  WAIT FOR CLKPERIOD/2;
END PROCESS clkproc;

control : PROCESS
BEGIN
  wait for 1 us;                                 -- Power-Up Time
  STBi <= '1';                                   -- Select Core
  WEi  <= '1';                                   -- Enable write from bus
  DATOi <= x"5678";
  wait until rising_edge(CLKi);
  STBi <= '0';                                   -- deselect Core
  wait until INTi='1';                           -- Wait for data ready
  wait until rising_edge(CLKi);
  STBi <= '1';                                   -- Select Core
  WEi  <= '0';                                   -- Enable read from bus
  wait until rising_edge(CLKi);
  RESULT <= DATIi;
  STBi <= '0';                                   -- deselect Core
  WAIT;
END PROCESS control;


MAX_spi_out: PROCESS
BEGIN
  MISO <= '1';
  wait until RSTi='0';
  wait until nCS='0';                            -- Write mode
  for i in 23 downto 0 loop
    wait until falling_edge(SCK);
  end loop;
  wait until nCS='0';                            -- Read mode
  for i in 7 downto 0 loop
    wait until rising_edge(SCK);
    MISO <= '0';                                 -- x"00"
  end loop;
  for i in 15 downto 0 loop
    wait until rising_edge(SCK);
    MISO <= SPIDATA(i);                          -- x"1234"
  end loop;
  wait;
END PROCESS MAX_spi_out;

MAX_spi_in: process
begin
  wait until nCS='0';
  for i in 7 downto 0 loop
    wait until falling_edge(SCK);
    MAXADR <= MAXADR(6 downto 0) & MOSI;
  end loop;
  for i in 15 downto 0 loop
    wait until falling_edge(SCK);
    MAXDAT <= MAXDAT(14 downto 0) & MOSI;
  end loop;
end process MAX_spi_in;

END ADFMAX31856_arch;
