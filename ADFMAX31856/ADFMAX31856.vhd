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
type tstate is (IniSt, AddrSt, DataSt, EndSt); -- SPI Communication States

signal state    : tstate;
signal streami  : std_logic_vector (7 downto 0);   -- Serial input register
signal streamo  : std_logic_vector (15 downto 0);  -- Serial output register
signal RDREG    : std_logic_vector (7 downto 0);   -- Read Data register
signal WDREG    : std_logic_vector (7 downto 0);   -- Write Data register
signal WAREG    : std_logic_vector (7 downto 0);   -- Write Address register
signal SCKN     : unsigned (4 downto 0);           -- SCK bit counter
signal SCKi     : std_logic;                       -- SCK SPI Clock
signal CSi      : std_logic;                       -- nCS SPI Chip select

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
  
SPIState:process (CLK_I, RST_I)
  begin
    if (RST_I='1') then
      state <= IniSt;
      CSi   <= '1';
      RDREG <= (others => '0');
    elsif falling_edge(SCKi) then
      case State is
        when IniSt =>
          if (STB_I='1') then
            CSi <= '0';
            state <= AddrSt;
          else
            CSi <= '1';
            state <= IniSt;
          end if;

        when AddrSt=> if (SCKN = 8) then
                        state <= DataSt;
                      end if;

        when DataSt=> if (SCKN = 31) then
                        state <= EndSt;
                      end if;

        when EndSt => CSi <='1';
                      RDREG <= streami(7 downto 0);
                      state <= IniSt;
      end case;
    end if;
  end process SPIState;

SPIbitcounter:process(SCKi, CSi)
  begin
    if (CSi='1') then
      SCKN  <= (others => '0');
    elsif falling_edge(SCKi) then
      SCKN <= SCKN+1;
    end if;
  end process SPIbitcounter;


SPIDataProcess:process(SCKi, CSi)
  begin
    if (CSi='1') then
      streami <= (others =>'0');
      streamo <= WAREG & WDREG;
    elsif rising_edge(SCKi) then
      streami <= streami(streami'high-1 downto 0) & MISO;
      streamo <= streamo(streamo'high-1 downto 0) & '0';
    end if;
  end process SPIDataProcess;

SBAprocess:process(CLK_I, RST_I)
  begin
    if RST_I='1' then
      WDREG <= (others => '0');
      WAREG <= (others => '0');
    elsif rising_edge(CLK_I) then
      if (STB_I='1' and WE_I='1') then
        WAREG <= DAT_I(15 downto 8);
        WDREG <= DAT_I(7 downto 0);
      end if;
    end if;
  end process SBAprocess;

-- Interface signals
  nCS   <= CSi;
  SCK   <= SCKi When CSi='0' else '1';
  MOSI  <= streamo(streamo'high);

  DAT_O <= std_logic_vector(resize(signed(RDREG(7 downto 0)),DAT_O'length));

end  ADFMAX31856_Arch;
