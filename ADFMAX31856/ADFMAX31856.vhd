--------------------------------------------------------------------------------
-- ADFMAX31856
--
-- Title: SBA Slave IP Core adapter for the Adafruit MAX31856 module
--
-- Versión 0.1
-- Date 2019/06/25
-- Author: Miguel A. Risco-Castillo
--
-- sba webpage: http://sba.accesus.com
-- core webpage: https://github.com/mriscoc/SBA-Library/tree/master/ADFMAX31856
--
-- Description: The ADFMAX31856 is an SBA IPCore designed to driver the Adafruit
-- MAX31856 thermocuple to Digital converter module which performs cold-junction
-- compensation and digitizes the signal from any type of thermocouple.

-- this reports the measured temperature in 14 bits with 0.25°C resolution.
-- The SBA core has 2 register, selectec by  ADR_I to access the 32 bits of the
-- MAX31855, thermocuple ADR_I(0)=1 and reference junction temperatures ADR_I(0)=0.
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
   ADR_I : in  std_logic_vector; -- Register AD0/AD1 selector
   DAT_O : out std_logic_vector; -- Data output Bus
   DAT_I : in  std_logic_vector; -- Data input Bus
-- Interface for ADFMAX31856
   nCS   : out std_logic;        -- Chipselect active low
   MISO  : in  std_logic;        -- Master In / Slave Out (SDO in MAX31856)
   MOSI  : out std_logic;        -- Master Out / Slave In (SDI in MAX31856)
   SCK   : out std_logic         -- SPI Clock
);
end ADFMAX31856;

architecture  ADFMAX31856_Arch of  ADFMAX31856 is
type tstate is (IniSt, DataSt, EndSt);             -- SPI Communication States

signal state    : tstate;
signal streami  : std_logic_vector(7 downto 0);    -- Serial input register
signal streamo  : std_logic_vector(7 downto 0);    -- Serial output register
signal SPIRD    : std_logic_vector(7 downto 0);    -- SPI Read Data register
signal SPIWR    : std_logic_vector(7 downto 0);    -- SPI Write Data register
--signal DREG     : std_logic_vector(7 downto 0);
--signal AREG     : std_logic_vector(7 downto 0);

signal SCKi     : std_logic;                       -- SCK SPI Clock
signal CSi      : std_logic;                       -- nCS SPI Chip select
signal SPItr    : std_logic;                       -- SPI transaction

begin

-- SPI Clock generator:

SCK1: if (sysfreq>5E6) generate
  CLK_Div : entity work.ClkDiv
  Generic map (
    infreq=>sysfreq,
    outfreq=>5E6
    )
  Port Map(
    RST_I => RST_I,
    CLK_I => CLK_I,
    CLK_O => SCKi
  );
end generate;

SCK2: if (sysfreq<=5E6) generate
  SCKi <= CLK_I;
end generate;

-- SPI BUS Proccess
  
SPIState:process (SCKi, RST_I)
variable SCKN : integer range 0 to 15;             -- SCK bit counter
  begin
    if (RST_I='1') then
      State  <= IniSt;
      streami<= (others =>'0');
      streamo<= (others =>'0');
      SPIRD  <= (others =>'0');
      SCKN   := 0;
    elsif falling_edge(SCKi) then
      case State is
        when IniSt => if (SPItr='1') then
                        streami<= (others =>'0');
                        streamo<= SPIWR;
                        state  <= DataSt;
                        SCKN   := 0;
                      end if;
        when DataSt=> streami <= streami(streami'high-1 downto 0) & MISO;
                      streamo <= streamo(streamo'high-1 downto 0) & '0';
                      if (SCKN < 7) then
                        SCKN := SCKN+1;
                      else
                        state <= EndSt;
                      end if;
        when EndSt => SPIRD <= streami;
                      state <= IniSt;
      end case;
    end if;
  end process SPIState;


Transaction:process(CLK_I, RST_I)
  begin
    if (RST_I='1') then
      SPItr <= '0';
    elsif rising_edge(CLK_I) then
      if (STB_I='1' and WE_I='1') then
        SPItr <= '1';
      elsif (State=DataSt) then
        SPItr <= '0';
      end if;
    end if;
   end process Transaction;


SBAprocess:process(CLK_I, RST_I)
  begin
    if RST_I='1' then
      SPIWR <= (others => '0');
    elsif rising_edge(CLK_I) then
      if (STB_I='1' and WE_I='1') then
        SPIWR <= DAT_I(7 downto 0);
      end if;
    end if;
  end process SBAprocess;

-- Interface signals
  nCS   <= '1' When State=IniSt else '0';
  SCK   <= SCKi When State=DataSt else '0';
  MOSI  <= streamo(streamo'high);

  DAT_O <= std_logic_vector(resize(signed(SPIRD(7 downto 0)),DAT_O'length));

end  ADFMAX31856_Arch;
