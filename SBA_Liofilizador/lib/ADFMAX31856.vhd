--------------------------------------------------------------------------------
-- ADFMAX31856
--
-- Title: SBA Slave IP Core adapter for the Adafruit MAX31856 module
--
-- Versión 0.1
-- Date 2019/07/18
-- Author: Miguel A. Risco-Castillo
--
-- sba webpage: http://sba.accesus.com
-- core webpage: https://github.com/mriscoc/SBA-Library/tree/master/ADFMAX31856
--
-- Description: The ADFMAX31856 is an SBA IPCore designed to driver the Adafruit
-- MAX31856 thermocuple to Digital converter module which performs cold-junction
-- compensation and digitizes the signal from any type of thermocouple.
-- A Write operation start a spi write cycle on CR0 and CR1 registers and then
-- the IPCore start continuous spi read cycles of the LTCBH and LTCBM registers.
--
-- Follow SBA v1.1 guidelines
--
--------------------------------------------------------------------------------
-- For Copyright and release notes please refer to:
-- https://github.com/mriscoc/SBA-Library/tree/master/ADFMAX31856
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ADFMAX31856 is
generic(
  debug:positive:=1;
  sysfreq:positive:=5E6
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
end ADFMAX31856;

architecture  ADFMAX31856_Arch of  ADFMAX31856 is
type tstate is (IniSt, DataSt, EndSt);             -- SPI Communication States
type tmode  is (idlemode, wrmode, rdmode, drdymode);

signal state    : tstate;
signal streami  : std_logic_vector(15 downto 0);   -- Serial input register
signal streamo  : std_logic_vector(23 downto 0);   -- Serial output register
signal SPIRD    : std_logic_vector(15 downto 0);   -- SPI Read Data register
signal SPIWR    : std_logic_vector(15 downto 0);   -- SPI Write Data register
signal SPIADR   : std_logic_vector(7 downto 0);    -- SPI Address register
signal SCKi     : std_logic;                       -- SCK SPI Clock
signal CSi      : std_logic;                       -- nCS SPI Chip select
signal SPItr    : std_logic;                       -- SPI transaction
signal INTF     : std_logic;                       -- Interrupt Flag
signal DRDY     : std_logic;                       -- Data ready
signal mode     : tmode;

begin

-- SPI Clock generator:

SCK1: if (sysfreq>2E6) generate
  CLK_Div : entity work.ClkDiv
  Generic map (
    infreq=>sysfreq,
    outfreq=>2E6
    )
  Port Map(
    RST_I => RST_I,
    CLK_I => CLK_I,
    CLK_O => SCKi
  );
end generate;

SCK2: if (sysfreq<=2E6) generate
  SCKi <= CLK_I;
end generate;

-- SPI BUS Proccess
  
SPIState:process (SCKi, RST_I)
variable SCKN : integer range 0 to 23;             -- SCK bit counter
  begin
    if (RST_I='1') then
      State  <= IniSt;
      SCKN   := 0;
    elsif falling_edge(SCKi) then
      case State is
        when IniSt => if (SPItr='1') then
                        state  <= DataSt;
                        SCKN   := 0;
                      end if;
        when DataSt=> if (SCKN < 23) then
                        SCKN := SCKN+1;
                      else
                        state <= EndSt;
                      end if;
        when EndSt => state <= IniSt;
      end case;
    end if;
  end process SPIState;

SPIIn:process (SCKi, RST_I)
  begin
    if (RST_I='1') then
      streami<= (others =>'0');
      SPIRD  <= (others =>'0');
    elsif falling_edge(SCKi) then
      if (State=DataSt) then
        streami <= streami(streami'high-1 downto 0) & MISO;
      elsif (State=EndSt) then
        SPIRD <= streami;
      end if;
    end if;
  end process SPIIn;

SPIOut:process (SCKi, RST_I, SPItr)
  begin
    if (RST_I='1') then
      streamo <= (others =>'0');
    elsif (SPItr='1') then
      streamo <= SPIADR & SPIWR;
    elsif rising_edge(SCKi) then
      if (State=DataSt) then
        streamo <= streamo(streamo'high-1 downto 0) & '0';
      end if;
    end if;
  end process SPIOut;

DRDYProcess:process(RST_I,SCKi,INTF)
  begin
    if (RST_I='1') or (INTF='1') then
      DRDY <= '0';
    elsif falling_edge(SCKi) and (mode = drdymode) then
      DRDY <= '1';
    end if;
  end process DRDYProcess;

modeproc:process(RST_I, SCKi, STB_I, WE_I)
  begin
    if RST_I='1' then
      SPIADR<= (others => '0');
      mode  <= idlemode;
      SPItr <= '0';
    elsif (STB_I='1' and WE_I='1') then
       SPIADR<= x"80";
       mode  <= wrmode;
       SPItr <= '1';
    elsif falling_edge(SCKi) then
      case mode is
        when idlemode => mode <= idlemode;

        when wrmode   => if (State=DataSt) then
                           SPItr <= '0';
                         end if;
                         if (State = EndSt) then
                           SPIADR<= x"0C";
                           mode <= rdmode;
                           SPItr <= '1';
                         end if;

        when rdmode   => if (State = DataSt) then
                           SPItr <= '0';
                         end if;
                         if (State = EndSt) then
                           mode <= drdymode;
                         end if;

        when drdymode => if (State = IniSt) then
                           SPIADR<= x"0C";
                           mode <= rdmode;
                           SPItr <= '1';
                         end if;
      end case;
    end if;
  end process modeproc;

SBAprocess:process(CLK_I, RST_I)
  begin
    if RST_I='1' then
      SPIWR <= (others => '0');
      INTF  <= '0';
    elsif rising_edge(CLK_I) then
      if (STB_I='1' and WE_I='1') then
        SPIWR <= DAT_I;
      end if;
      if (DRDY='0') then
        INTF <= '0';
      elsif (DRDY='1') then
        INTF <= '1';
      end if;
    end if;
  end process SBAprocess;

-- Interface signals
  nCS   <= '1' When State=IniSt else '0';
  SCK   <= SCKi When State=DataSt else '0';
  MOSI  <= streamo(streamo'high);
  INT_O <= DRDY;
  DAT_O <= std_logic_vector(SPIRD);

end  ADFMAX31856_Arch;
