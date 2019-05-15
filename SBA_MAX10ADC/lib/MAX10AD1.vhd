--------------------------------------------------------------------------------
-- MAX10AD1
--
-- Title: SBA Slave IP Core adapter for MAX10 Single ADC
--
-- Versión 0.1
-- Date 2019/04/27
-- Author: Miguel A. Risco-Castillo
--
-- sba webpage: http://sba.accesus.com
-- core webpage: https://github.com/mriscoc/SBA-Library/tree/master/MAX10AD1
--
-- Description:
--
-- Follow SBA v1.1 guidelines
--
-- Release Notes:
--
-- v0.1 2019/04/27
-- Initial release
--
--------------------------------------------------------------------------------
-- Copyright:
--
-- This code, modifications, derivate work or based upon, can not be used or
-- distributed without the complete credits on this header.
--
-- This version is released under the GNU/GLP license
-- http://www.gnu.org/licenses/gpl.html
-- if you use this component for your research please include the appropriate
-- credit of Author.
--
-- The code may not be included into ip collections and similar compilations
-- which are sold. If you want to distribute this code for money then contact me
-- first and ask for my permission.
--
-- These copyright notices in the source code may not be removed or modified.
-- If you modify and/or distribute the code to any third party then you must not
-- veil the original author. It must always be clearly identifiable.
--
-- Although it is not required it would be a nice move to recognize my work by
-- adding a citation to the application's and/or research.
--
-- FOR COMMERCIAL PURPOSES REQUEST THE APPROPRIATE LICENSE FROM THE AUTHOR.
--------------------------------------------------------------------------------

Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MAX10AD1 is
port(
-- SBA Interface
   RST_I : in  std_logic;        -- Active high reset
   CLK_I : in  std_logic;        -- Main clock
   STB_I : in  std_logic;        -- Strobe
   WE_I  : in  std_logic;        -- Bus write, active high
   ADR_I : in  std_logic_vector; -- AD Register selector
   DAT_O : out std_logic_vector  -- Data output Bus
);
end MAX10AD1;


architecture MAX10AD1_arch of MAX10AD1 is

signal clock_clk : std_logic;
signal reset_sink_reset_n : std_logic;
signal adc_pll_clock_clk : std_logic;
signal adc_pll_locked_export : std_logic;
signal command_valid : std_logic;
signal command_channel : std_logic_vector(4 downto 0);
signal command_startofpacket : std_logic;
signal command_endofpacket : std_logic;
signal command_ready : std_logic;
signal response_valid : std_logic;
signal response_channel : std_logic_vector(4 downto 0);
signal response_data : std_logic_vector(11 downto 0);
signal response_startofpacket : std_logic;
signal response_endofpacket : std_logic;

signal ADREG:std_logic_vector(response_data'range);
signal adcread:std_logic;

  component MAX10ADC is
  port (
  	clock_clk              : in  std_logic                     := 'X';             -- clk
  	reset_sink_reset_n     : in  std_logic                     := 'X';             -- reset_n
  	adc_pll_clock_clk      : in  std_logic                     := 'X';             -- clk
  	adc_pll_locked_export  : in  std_logic                     := 'X';             -- export
  	command_valid          : in  std_logic                     := 'X';             -- valid
  	command_channel        : in  std_logic_vector(4 downto 0)  := (others => 'X'); -- channel
  	command_startofpacket  : in  std_logic                     := 'X';             -- startofpacket
  	command_endofpacket    : in  std_logic                     := 'X';             -- endofpacket
  	command_ready          : out std_logic;                                        -- ready
  	response_valid         : out std_logic;                                        -- valid
  	response_channel       : out std_logic_vector(4 downto 0);                     -- channel
  	response_data          : out std_logic_vector(11 downto 0);                    -- data
  	response_startofpacket : out std_logic;                                        -- startofpacket
  	response_endofpacket   : out std_logic                                         -- endofpacket
  );
  end component MAX10ADC;

begin

MAX10AD : component MAX10ADC
port map (
	clock_clk              => clock_clk,              --          clock.clk
	reset_sink_reset_n     => reset_sink_reset_n,     --     reset_sink.reset_n
	adc_pll_clock_clk      => adc_pll_clock_clk,      --  adc_pll_clock.clk
	adc_pll_locked_export  => adc_pll_locked_export,  -- adc_pll_locked.export
	command_valid          => command_valid,          --        command.valid
	command_channel        => command_channel,        --               .channel
	command_startofpacket  => command_startofpacket,  --               .startofpacket
	command_endofpacket    => command_endofpacket,    --               .endofpacket
	command_ready          => command_ready,          --               .ready
	response_valid         => response_valid,         --       response.valid
	response_channel       => response_channel,       --               .channel
	response_data          => response_data,          --               .data
	response_startofpacket => response_startofpacket, --               .startofpacket
	response_endofpacket   => response_endofpacket    --               .endofpacket
);

clock_clk <= CLK_I;
adc_pll_clock_clk <= CLK_I;
adc_pll_locked_export <= '1';
reset_sink_reset_n <= not RST_I;
command_channel <= "00000";
command_valid <= '1';
command_startofpacket <= '1';

	reset_reset_n <= '1';
	command_valid <= '1';
	command_startofpacket <= '1';
	command_ready <= '1';



process(CLK_I,RST_I)
  variable adcflg : STD_LOGIC := '0';
begin
  if RST_I='1' then
    adcflg := '0';
  elsif rising_edge(CLK_I) and response_valid = '1' then
    if adcread = '1' and adcflg = '1' then
      ADREG <= response_data;
      adcflg := '0';
    end if;
    if adcread = '0' then
      adcflg := '1';
    end if;
  end if;
end process;

process(CLK_I,RST_I)
begin
  if RST_I='1' then
    adcread <= '0';
  elsif rising_edge(CLK_I) then
    adcread <= '1';
  end if;
end process;

DAT_O <= std_logic_vector(resize(unsigned(ADREG),DAT_O'length));

end MAX10AD1_arch;

