--------------------------------------------------------------------------------
-- Project Name: SBA_Liofilizador
-- Title: Control Principal SBA
-- Version: 0.1.1
-- Date: 2019/04/02
-- Project Author: Miguel A. Risco Castillo
-- Description: Sistema de control e instrumentación para el Liofilizador
--------------------------------------------------------------------------------
-- Template version: 1.2
-- Template date: 2018/03/18
--------------------------------------------------------------------------------
-- Copyright example, you can use or modify at your convenience for your project.
--
-- This code, modifications, derivate work or based upon, can not be used or
-- distributed without the complete credits on this header.
--
-- The copyright notices in the source code may not be removed or modified.
-- If you modify and/or distribute the code to any third party then you must not
-- veil the original author. It must always be clearly identifiable.
--
-- Although it is not required it would be a nice move to recognize my work by
-- adding a citation to the application's and/or research. If you use this
-- component for your research please include the appropriate credit of Author.
--
-- FOR COMMERCIAL PURPOSES REQUEST THE APPROPRIATE LICENSE FROM THE AUTHOR.
--
-- For non commercial purposes this version is released under the GNU/GLP license
-- http://www.gnu.org/licenses/gpl.html
--------------------------------------------------------------------------------

Library IEEE;
use IEEE.std_logic_1164.all;
use work.SBA_Liofilizador_SBAconfig.all;

entity SBA_Liofilizador_Top is
port (
  CLK_I     : in  std_logic;
  nRST      : in  std_logic;
  RXRS      : in  std_logic;
  TXRS      : out std_logic;
  BTN       : in  std_logic;
  LEDS      : out std_logic_vector(7 downto 0);
  TCTRL     : out std_logic;
  TC1_nCS   : out std_logic;
  TC1_MISO  : in  std_logic;
  TC1_SCK   : out std_logic
);
end SBA_Liofilizador_Top;

--------------------------------------------------------------------------------

architecture SBA_Liofilizador_structural of SBA_Liofilizador_Top is
     
-- SBA internal signals
  Signal RSTi  : Std_Logic;
  Signal CLKi  : Std_Logic;
  Signal ADRi  : ADDR_type;
  Signal DATOi : DATA_type;
  Signal DATIi : DATA_type;
  Signal ADATi : ADAT_type;
  Signal STBEi : std_logic;
  Signal STBi  : std_logic_vector(Stb_width-1 downto 0);
  Signal WEi   : Std_Logic;
  Signal ACKi  : Std_Logic;
  Signal INTi  : Std_Logic;

-- Auxiliary external to internal signals
  Signal CLKe  : std_logic;
  Signal RSTe  : std_logic;
  Signal SWTe  : std_logic_vector(8 downto 0);
  Signal GPOe  : std_logic_vector(8 downto 0);
-- Auxiliary IPCores signals
  Signal INT_TIMER  : std_logic;

--------------------------------------------------------------------------------

begin

  SBA_Liofilizador_SysCon: entity work.SysCon
  generic map(
    PLL   => true
  )
  port Map(
    CLK_I => CLKe,
    CLK_O => CLKi,
    RST_I => RSTe,
    RST_O => RSTi
  );

  SBA_Liofilizador_Master: entity work.SBA_Liofilizador_SBAcontroller
  port Map(
    RST_I => RSTi, 
    CLK_I => CLKi,  
    DAT_I => DATIi,  
    DAT_O => DATOi,  
    ADR_O => ADRi,
    STB_O => STBEi,
    WE_O  => WEi,
    ACK_I => ACKi,
    INT_I => INTi  
  );

  SBA_Liofilizador_mux: entity work.SBA_Liofilizador_SBAmux
  port Map(
    STB_I => STBEi,             -- Address Enabler
    -- ADDRESS decoder --------
    ADR_I => ADRi,              -- Address input Bus
    STB_O => STBi,              -- Strobe Chips selector
    -- DATA mux ---------------
    ADAT_I=> ADATi,             -- Array of data buses
    DAT_O => DATIi              -- Data out bus
  );

  GPIO: entity work.GPIO
  generic map(
    SIZE    => 9
  )
  port map(
    -------------
    RST_I => RSTi,
    CLK_I => CLKi,
    STB_I => STBi(STB_GPIO),
    WE_I  => WEi,
    DAT_I => DATOi,
    DAT_O => ADATi(STB_GPIO),
    -------------
    P_I   => SWTe,
    P_O   => GPOe
  );

  PMODTC1: entity work.PMODTC1
  generic map(
    debug   => debug,
    sysfreq => sysfreq
  )
  port map(
    -------------
    RST_I => RSTi,
    CLK_I => CLKi,
    STB_I => STBi(STB_PMODTC1),
    ADR_I => ADRi,
    WE_I  => WEi,
    DAT_O => ADATi(STB_PMODTC1),
    -------------
    nCS   => TC1_nCS,
    MISO  => TC1_MISO,
    SCK   => TC1_SCK
  );

  TIMER: entity work.TIMER
  generic map(
    chans   => 1
  )
  port map(
    -------------
    RST_I => RSTi,
    CLK_I => CLKi,
    STB_I => STBi(STB_TIMER),
    ADR_I => ADRi,
    WE_I  => WEi,
    DAT_I => DATOi,
    DAT_O => ADATi(STB_TIMER),
    INT_O => INT_TIMER,
    -------------
    TOUT  => open
  );

  UART: entity work.UART
  generic map(
    debug   => debug,
    sysfreq => sysfreq,
    baud    => 115200,
    rxbuff  => 8
  )
  port map(
    -------------
    RST_I => RSTi,
    CLK_I => CLKi,
    STB_I => STBi(STB_UART),
    ADR_I => ADRi,
    WE_I  => WEi,
    DAT_I => DATOi,
    DAT_O => ADATi(STB_UART),
    -------------
    RX    => RXRS,
    TX    => TXRS
  );


-- External Signals Assignments
-------------------------------
 RSTe  <= not nRST;             -- SBA reset is active high, negate if it is necessary
 CLKe  <= CLK_I;
 SWTe  <= (0=>BTN,others=>'1');
 LEDS  <= GPOe(7 downto 0);
 TCTRL <= GPOe(8);

-- Internal Signals Assignments
-------------------------------
 ACKi  <= '1';                  -- If None Slave IPCore use ACK then ACKi must be '1'
 INTi  <= INT_TIMER;            -- Interrupts;

end SBA_Liofilizador_structural;

--------------------------------------------------------------------------------
