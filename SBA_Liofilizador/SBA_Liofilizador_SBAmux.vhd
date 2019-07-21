--------------------------------------------------------------------------------
-- Project Name: SBA_Liofilizador
-- Title: Control Principal SBA
-- Version: 0.1.1
-- Date: 2019/04/02
-- Project Author: Miguel A. Risco Castillo
-- Description: Sistema de control e instrumentación para el Liofilizador
--------------------------------------------------------------------------------
-- SBA Mux
--
-- SBA Address Decoder and Data Mux
-- Based on SBA v1.2 guidelines
--
-- v1.0 2018/03/18
--
-- SBA Author: Miguel A. Risco-Castillo
-- sba webpage: http://sba.accesus.com
--
--------------------------------------------------------------------------------
-- Copyright of the component SBA Address decoder:
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SBA_Liofilizador_SBAconfig.all;

entity SBA_Liofilizador_SBAMux  is
port(
  STB_I : in std_logic;                                -- Address Enabler
  -- ADDRESS decoder --------
  ADR_I : in ADDR_type;                                -- Address input Bus
  STB_O : out std_logic_vector(Stb_width-1 downto 0);  -- Strobe Chips selector
  -- DATA mux ---------------
  ADAT_I: in ADAT_type;                                -- Array of data buses
  DAT_O : out DATA_type                                -- Data out bus
);
end SBA_Liofilizador_SBAMux;

architecture SBA_Liofilizador_SBAmux_Arch of SBA_Liofilizador_SBAMux is

Signal STBi : std_logic_vector(STB_O'range);

function stb(val:natural) return std_logic_vector is
variable ret : unsigned(Stb_width-1 downto 0);
begin
  ret:=(0 => '1', others=>'0');
  return std_logic_vector((ret sll (val)));
end;

begin

ADDRProc:process (ADR_I)
Variable ADRi : integer;
begin
  ADRi := to_integer(unsigned(ADR_I));
  case ADRi is
  ------------------------------------------------------------------------------
     When GPIO                => STBi <= stb(STB_GPIO);     -- GPIO       = 0
     When TC1R0               => STBi <= stb(STB_PMODTC1);  -- TC1R0      = 2
     When TC1R1               => STBi <= stb(STB_PMODTC1);  -- TC1R1      = 3
     When TMRDATL             => STBi <= stb(STB_TIMER);    -- TMRDATL    = 4
     When TMRDATH             => STBi <= stb(STB_TIMER);    -- TMRDATH    = 5
     When TMRCFG              => STBi <= stb(STB_TIMER);    -- TMRCFG     = 6
     When TMRCHS              => STBi <= stb(STB_TIMER);    -- TMRCHS     = 7
     When UART0               => STBi <= stb(STB_UART);     -- UART0      = 8
     When UART1               => STBi <= stb(STB_UART);     -- UART1      = 9
     When HXR0                => STBi <= stb(STB_HX711);    -- HX711 R0   = 10
     When HXR1                => STBi <= stb(STB_HX711);    -- HX711 R1   = 11
     when ADFMAX              => STBi <= stb(STB_ADFMAX);   -- ADFMAX     = 12
     When OTHERS              => STBi <= (others =>'0');
  ------------------------------------------------------------------------------
  end case;
end process ADDRProc;

DATAProc:process (ADR_I,ADAT_I)
Variable ADRi : integer;
begin
  ADRi := to_integer(unsigned(ADR_I));
  case ADRi is
  ------------------------------------------------------------------------------
     When GPIO                => DAT_O <= ADAT_I(STB_GPIO);     -- GPIO     = 0
     When TC1R0               => DAT_O <= ADAT_I(STB_PMODTC1);  -- TC1R0    = 2
     When TC1R1               => DAT_O <= ADAT_I(STB_PMODTC1);  -- TC1R1    = 3
     When TMRDATL             => DAT_O <= ADAT_I(STB_TIMER);    -- TMRDATL  = 4
     When TMRDATH             => DAT_O <= ADAT_I(STB_TIMER);    -- TMRDATH  = 5
     When TMRCFG              => DAT_O <= ADAT_I(STB_TIMER);    -- TMRCFG   = 6
     When TMRCHS              => DAT_O <= ADAT_I(STB_TIMER);    -- TMRCHS   = 7
     When UART0               => DAT_O <= ADAT_I(STB_UART);     -- UART0    = 8
     When UART1               => DAT_O <= ADAT_I(STB_UART);     -- UART1    = 9
     When HXR0                => DAT_O <= ADAT_I(STB_HX711);    -- HX711 R0 = 10
     When HXR1                => DAT_O <= ADAT_I(STB_HX711);    -- HX711 R1 = 11
     when ADFMAX              => DAT_O <= ADAT_I(STB_ADFMAX);   -- ADFMAX   = 12
     When OTHERS              => DAT_O <= (others =>'X');
  ------------------------------------------------------------------------------
  end case;
end process DATAProc;

  STB_O <= STBi When STB_I='1' else (others=>'0');

end SBA_Liofilizador_SBAmux_Arch;

