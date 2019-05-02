-- /SBA: Controller ============================================================
--
-- /SBA: Program Details =======================================================
-- Project Name: SBA_Liofilizador
-- Title: Control Principal SBA
-- Version: 0.1.1
-- Date: 2019/04/02
-- Project Author: Miguel A. Risco Castillo
-- Description: Sistema de control e instrumentación para el Liofilizador
-- /SBA: End Program Details ---------------------------------------------------
--
-- SBA Master System Controller v1.70 2019/04/22 (added support for multisubroutines)
-- Based on Master Controller for SBA v1.2 Guidelines
--
-- SBA Author: Miguel A. Risco-Castillo
-- SBA web page: http://sba.accesus.com
--
--------------------------------------------------------------------------------
-- Copyright:
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
use work.SBApackage.all;

entity SBA_Liofilizador_SBAcontroller  is
port(
   RST_I : in std_logic;                     -- active high reset
   CLK_I : in std_logic;                     -- main clock
   DAT_I : in std_logic_vector;              -- Data input Bus
   DAT_O : out std_logic_vector;             -- Data output Bus
   ADR_O : out std_logic_vector;             -- Address output Bus
   STB_O : out std_logic;                    -- Strobe enabler
   WE_O  : out std_logic;                    -- Write / Read
   ACK_I : in  std_logic;                    -- Strobe Acknowledge
   INT_I : in  std_logic                     -- Interrupt request
);
end SBA_Liofilizador_SBAcontroller;

architecture SBA_Liofilizador_SBAcontroller_Arch of SBA_Liofilizador_SBAcontroller is

  subtype STP_type is integer range 0 to 58;
  type STPS_type is array (0 to 7) of STP_type;
  subtype ADR_type is integer range 0 to (2**ADR_O'length-1);

  signal D_Oi : unsigned(DAT_O'range);       -- Internal Data Out signal (unsigned)
  signal A_Oi : ADR_type;                    -- Internal Address signal (integer)
  signal S_Oi : std_logic;                   -- strobe (Address valid)   
  signal W_Oi : std_logic;                   -- Write enable ('0' read enable)
  signal STPi : STP_type;                    -- STeP counter
  signal NSTPi: STP_type;                    -- Step counter + 1 (Next STep)
  signal IFi  : std_logic;                   -- Interrupt Flag
  signal IEi  : std_logic;                   -- Interrupt Enable


begin

  Main : process (CLK_I, RST_I)

-- General variables
  variable jmp  : STP_type;                  -- Jump step register
  variable dati : unsigned(DAT_I'range);     -- Input Internal Data Bus
  alias    dato is D_Oi;                     -- Output Data Bus alias

-- Multiroutine support
  variable STPS  : STPS_type;                -- Step Stack
  variable STPS_P : natural range STPS'range;-- Step Stack pointer

-- Interrup support variables
  variable reti : STP_type;                  -- Return from Interrupt
  variable rfif : std_logic;                 -- Return from Interrupt flag
  variable tmpdati : unsigned(DAT_I'range);  -- Temporal dati
  variable tiei : std_logic;                 -- Temporal Interrupt Enable

-- /SBA: Procedures ============================================================

  -- Prepare bus for reading from DAT_I in the next step
  procedure SBAread(addr:in integer) is
  begin
    if (debug=1) then
      Report "SBAread: Address=" &  integer'image(addr);
    end if;

    A_Oi <= addr;
    S_Oi <= '1';
    W_Oi <= '0';
  end;

  -- Write values to bus
  procedure SBAwrite(addr:in integer; data: in unsigned) is
  begin
    if (debug=1) then
      Report "SBAwrite: Address=" &  integer'image(addr) & " Data=" &  integer'image(to_integer(data));
    end if;

    A_Oi <= addr;
    S_Oi <= '1';
    W_Oi <= '1';
    D_Oi <= resize(data,D_Oi'length);
  end;

  -- write integers
  procedure SBAwrite(addr:in integer; data: in integer) is
  begin
    SBAwrite(addr,to_unsigned(data,D_Oi'length));
  end;		   

  -- Do not make any modification to bus in that step
  procedure SBAwait is
  begin
    S_Oi<='1'; 
  end;

  -- Jump to arbitrary step
  procedure SBAjump(stp:in integer) is
  begin
	 jmp:=stp;
  end;

  -- Jump to rutine and storage return step in ret variable
  procedure SBAcall(stp:in integer) is
  begin
	 jmp:=stp;
     STPS(STPS_P):=NSTPi;
     dec(STPS_P);
  end;

  -- Return from subrutine
  procedure SBAret is
  begin
    inc(STPS_P);
    jmp:=STPS(STPS_P);
  end;

  -- Return from interrupt
  procedure SBAreti is
  begin
    jmp:=reti;
    IEi<=tiei;
    rfif:='1';
  end;

  -- Interrupt enable disable
  procedure SBAinte(enable:boolean) is
  begin
    if enable then IEi<='1'; else IEi<='0'; end if;
  end;

-- /SBA: End Procedures --------------------------------------------------------

  
-- /SBA: User Registers and Constants ==========================================

  variable counter : natural range 0 to 65535;  -- Simple counter
  variable timerf  : std_logic;                 -- timer interrupt flag
  variable Idx     : natural;                   -- General purpose index
  variable T       : signed(15 downto 0);       -- Temperature register
  variable Sign    : std_logic;                 -- Sign bit
  variable T0      : signed(21 downto 0);       -- Temperature Ch0
  variable T1      : signed(21 downto 0);       -- Temperature Ch1
  constant TXRDY   : integer:=14;               -- TXRDY Flag is bit 14
  constant RXRDY   : integer:=15;               -- RXRDY Flag is bit 15
  variable UARTFlg : std_logic;                 -- aux bit for UART flags
  variable RSTmp   : unsigned(7 downto 0);      -- Temporal register for UART

  type tarrchar is array (natural range <>) of character;
  constant vMsg    : tarrchar (0 to 18):="Liofilizador v0.1.1";

  variable bin_in  : unsigned(15 downto 0);    -- 16 bit input register
  variable bcd_out : unsigned(19 downto 0);    -- 20 bit output register

-- /SBA: End User Registers and Constants --------------------------------------

-- /SBA: Label constants =======================================================
  constant UARTSendChar: integer := 003;
  constant UARTGetChar: integer := 006;
  constant UARTSendBCD: integer := 009;
  constant Bin2BCD: integer := 016;
  constant INT: integer := 019;
  constant Init: integer := 021;
  constant SendMsg: integer := 027;
  constant LoopMain: integer := 031;
  constant EndLoopMain: integer := 032;
  constant SendTemperatureData: integer := 033;
-- /SBA: End Label constants ---------------------------------------------------

begin

  if rising_edge(CLK_I) then
  
    if (debug=1) then
      Report "Step: " &  integer'image(STPi);
    end if;
	 
	jmp := 0;			      -- Default jmp value
    S_Oi<='0';                -- Default S_Oi value

	if STPi=2 then            -- Save DAT_I to restore after interrupt
      tmpdati:=unsigned(DAT_I);
    end if;

    if rfif='0' then
      dati:= unsigned(DAT_I); -- Get and capture value from data bus
    else
      dati:= tmpdati;         -- restore data bus after interrupt
      rfif:= '0';
    end if;

    if (RST_I='1') then
      STPi<= 1;               -- First step is 1 (cal and jmp valid only if >0)
      A_Oi<= 0;               -- Default Address Value
      W_Oi<='1';              -- Default W_Oi value on reset

    -- Multisubroutine support
      STPS_P:=STPS'high;      -- Default Step Stack pointer value

    -- Interrupt Support
      IEi <='0';              -- Default Interrupt disable
      reti:= 0;
      rfif:='0';

    elsif (ACK_I='1') or (S_Oi='0') then
      case STPi is

-- /SBA: User Program ==========================================================
                
        When 001=> SBAjump(Init);            -- Reset Vector (001)
        When 002=> SBAjump(INT);             -- Interrupt Vector (002)
                
------------------------------ ROUTINES ----------------------------------------
-- /L:UARTSendChar
        When 003=> SBAread(UART1);               -- Read UART Status
        When 004=> UARTFlg := dati(TXRDY);       -- Read TXRDY flag
        When 005=> if UARTFlg ='0' then          -- Test TXRDY
                     SBAread(UART1);             -- if not continue read UART Status
                     SBAjump(UARTSendChar+1);
                   else
                     SBAwrite(UART0,RSTmp);      -- Write UART Tx
                     SBAret;                     -- Return
                   end if;
                
-- /L:UARTGetChar
        When 006=> SBAread(UART0);               -- Read UART Status
        When 007=> UARTFlg := dati(RXRDY);       -- Read RXRDY flag
                   RSTmp:= dati(7 downto 0);     -- Read possible char in to RSTmp
        When 008=> if UARTFlg ='0' then          -- Test RXRDY
                     SBAread(UART0);             -- Continue read UART Status
                     SBAjump(UARTGetChar+1);
                   else
                     SBAret;
                   end if;
                
-- /L:UARTSendBCD
        When 009=> if Sign='1' then
                     RSTmp:=chr2uns('-');
                   else
                     RSTmp:=chr2uns(' ');
                   end if;
                   SBAcall(UARTSendChar);
        When 010=> RSTmp:=hex(x"0" & bcd_out(19 downto 16)); SBAcall(UARTSendChar);
        When 011=> RSTmp:=hex(x"0" & bcd_out(15 downto 12)); SBAcall(UARTSendChar);
        When 012=> RSTmp:=hex(x"0" & bcd_out(11 downto 08)); SBAcall(UARTSendChar);
        When 013=> RSTmp:=hex(x"0" & bcd_out(07 downto 04)); SBAcall(UARTSendChar);
        When 014=> RSTmp:=hex(x"0" & bcd_out(03 downto 00)); SBAcall(UARTSendChar);
        When 015=> SBAret;
                
-- /L:Bin2BCD
        When 016=> bcd_out := (others=>'0');
                   if bin_in=0 then SBAret; end if;
        When 017=> bcd_out(2 downto 0) := bin_in(15 downto 13); -- shl 3
                   bin_in := bin_in(12 downto 0) & "000";
        When 018=> for j in 0 to 12 loop
                     for i in 0 to 3 loop -- for nibble 0 to 3
                       if bcd_out(3+4*i downto 4*i)>4 then -- is nibble > 4?
                         bcd_out(3+4*i downto 4*i):=bcd_out(3+4*i downto 4*i)+3; -- add 3 to nibble
                       end if;
                     end loop; -- last nibble do not need adjust (65535)
                     bcd_out := bcd_out(18 downto 0) & bin_in(15); --shl
                     bin_in := bin_in(14 downto 0) & '0';
                   end loop;
                   SBAret;
                
------------------------------ INTERRUPT ---------------------------------------
-- /L:INT
        When 019=> timerf:='1';
                   SBAread(TMRCFG);
        When 020=> SBAreti;
------------------------------ MAIN PROGRAM ------------------------------------
                
-- /L:Init
        When 021=> counter:=0; timerf:='0';T0:=(others=>'0');T1:=(others=>'0');
        When 022=> SBAwrite(TMRCHS,0);          -- Select timer 0
        When 023=> SBAwrite(TMRDATL,x"25A0");   -- Write to LSW, (100'000,000 = 5F5E100)
        When 024=> SBAwrite(TMRDATH,x"0026");   -- Write to MSW
        When 025=> SBAwrite(TMRCFG,"0X11");     -- Disable output, Enable timer interrupt
        When 026=> SBAinte(true);               -- Enable interrupts
                
-- /L:SendMsg
        When 027=> Idx:=0;
        When 028=> RSTmp:=chr2uns(vMsg(Idx)); SBAcall(UARTSendChar);
        When 029=> if Idx<vMsg'length-1 then inc(Idx); SBAjump(SendMsg+1); end if;
        When 030=> RSTmp:=x"0A"; SBAcall(UARTSendChar);
                
-- /L:LoopMain
        When 031=> if (timerf='1') then
                     SBAjump(SendTemperatureData);
                     timerf:='0';
                     inc(counter);
                   end if;
                
                
                
-- /L:EndLoopMain
        When 032=> SBAjump(LoopMain);
                
                
-- /L:SendTemperatureData
        When 033=> RSTmp:=chr2uns('@'); SBAcall(UARTSendChar);       -- Start of frame
        When 034=> RSTmp:=x"24"; SBAcall(UARTSendChar);              -- Frame Size 36
        When 035=> RSTmp:=chr2uns('D'); SBAcall(UARTSendChar);       -- Data Frame
                
-- Send counter
        When 036=> bin_in:=to_unsigned(counter,bin_in'length); Sign:='0';
                   SBAcall(Bin2BCD);
                   SBAwrite(GPIO,counter);
        When 037=> SBACall(UARTSendBCD);
        When 038=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);
                
        When 039=> SBAread(TC1R0);                                   -- Reference Juntion Temperature
        When 040=> T:=Resize(25*signed(dati(15 downto 4)),T'length); -- 400xT
                   T:=Resize(T(15 downto 2),T'length);               -- T/4
                   Sign:=dati(15);
                
        When 041=> bin_in:=unsigned(T); SBAcall(Bin2BCD);            -- Unfiltered
        When 042=> SBACall(UARTSendBCD);
        When 043=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);
                
        When 044=> T0:=("0000" & T0(15 downto 0) & "00") - T0;       -- Filtered
        When 045=> T0:= T0 + ("000" & T & "000");
                   T0:= resize(T0(21 downto 2),T0'length);
                   bin_in:=unsigned(T0(18 downto 3)); SBAcall(Bin2BCD);
        When 046=> SBACall(UARTSendBCD);
        When 047=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);
                
        When 048=> SBAread(TC1R1);                                   -- Thermocuple temperature
        When 049=> T:=Resize(25*signed(dati(15 downto 2)),T'length); -- 100xT
                   Sign:=dati(15);
                
        When 050=> bin_in:=unsigned(T); SBAcall(Bin2BCD);            -- Unfiltered
        When 051=> SBACall(UARTSendBCD);
        When 052=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);
                
        When 053=> T1:=("0000" & T1(15 downto 0) & "00") - T1;       -- Filtered
        When 054=> T1:= T1 + ("0000" & T & "00");
                   T1:= resize(T1(21 downto 2),T1'length);
                   bin_in:=unsigned(T1(17 downto 2)); SBAcall(Bin2BCD);
        When 055=> SBACall(UARTSendBCD);
                
        When 056=> RSTmp:=x"0A"; SBAcall(UARTSendChar);
        When 057=> SBAjump(LoopMain);
                
-- /SBA: End User Program ------------------------------------------------------

        When others=> jmp:=1; 
      end case;

      if IFi='1' then
        if jmp/=0 then reti:=jmp; else reti:=NSTPi; end if;
        tiei := IEi;
        IEi <= '0';
        STPi <= 2;      -- Always jump to Step 002 (Interrupt vector) (TODO: Could be INT?)
      else
        if jmp/=0 then STPi<=jmp; else STPi<=NSTPi; end if;
      end if;

    end if;
  end if;
end process;

IntProcess : process(RST_I,INT_I,IEi)
begin
  if RST_I='1' then
    IFi<='0';
  elsif (INT_I='1') and (IEi='1') then
    IFi<='1';
  else
    IFi<='0';
  end if;
end process IntProcess;


NSTPi <= STPi + 1;      -- Step plus one (Next STeP)
STB_O <= S_Oi;
WE_O  <= W_Oi;
ADR_O <= std_logic_vector(to_unsigned(A_Oi,ADR_O'length));
DAT_O <= std_logic_vector(D_Oi);

end SBA_Liofilizador_SBAController_Arch;

