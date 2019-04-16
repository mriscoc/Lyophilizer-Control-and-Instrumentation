-- /SBA: Program Details =======================================================
-- Project Name: SBA_Liofilizador
-- Title: Control Principal SBA
-- Version: 0.1.1
-- Date: 2019/04/02
-- Project Author: Miguel A. Risco Castillo
-- Description: Sistema de control e instrumentación para el Liofilizador
-- /SBA: End Program Details ---------------------------------------------------

-- /SBA: User Registers and Constants ==========================================

  variable counter : natural range 0 to 65535;  -- Simple counter
  variable capture : natural range 0 to 65535;  -- Capture data at timer interrupt
  variable Idx     : natural;                   -- General purpose index
  variable T       : unsigned(12 downto 0);     -- Temperature register 13 bits
  variable Sign    : std_logic;                 -- Sign bit
  variable TCR0    : unsigned(15 downto 0);     -- PMODTC1 register 0
  variable TCR1    : unsigned(15 downto 0);     -- PMODTC1 register 1
  constant TXRDY   : integer:=14;               -- TXRDY Flag is bit 14
  constant RXRDY   : integer:=15;               -- RXRDY Flag is bit 15
  variable UARTFlg : std_logic;                 -- aux bit for UART flags
  variable RSTmp   : unsigned(7 downto 0);      -- Temporal register for UART

  type tarrchar is array (natural range <>) of character;
  constant vMsg    : tarrchar (0 to 18):="Liofilizador v0.1.1";

  variable bin_in  : unsigned(15 downto 0);    -- 16 bit input register
  variable bcd_out : unsigned(19 downto 0);    -- 20 bit output register

-- /SBA: End User Registers and Constants --------------------------------------

-- /SBA: User Program ==========================================================

=> SBAjump(Init);            -- Reset Vector (001)
=> SBAjump(INT);             -- Interrupt Vector (002)

------------------------------ ROUTINES ----------------------------------------
-- /L:UARTSendChar
=> SBAread(UART1);               -- Read UART Status
=> UARTFlg := dati(TXRDY);       -- Read TXRDY flag
=> if UARTFlg ='0' then          -- Test TXRDY
     SBAread(UART1);             -- if not continue read UART Status
     SBAjump(UARTSendChar+1);
   else
     SBAwrite(UART0,RSTmp);      -- Write UART Tx
     SBAret;                     -- Return
   end if;

-- /L:UARTGetChar
=> SBAread(UART0);               -- Read UART Status
=> UARTFlg := dati(RXRDY);       -- Read RXRDY flag
   RSTmp:= dati(7 downto 0);     -- Read possible char in to RSTmp
=> if UARTFlg ='0' then          -- Test RXRDY
     SBAread(UART0);             -- Continue read UART Status
     SBAjump(UARTGetChar+1);
   else
     SBARet;
   end if;

-- /L:Bin2BCD
=> bcd_out := (others=>'0');
   if bin_in=0 then SBAret; end if;
=> bcd_out(2 downto 0) := bin_in(15 downto 13); -- shl 3
   bin_in := bin_in(12 downto 0) & "000";
=> for j in 0 to 12 loop
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
=> capture:=counter;
   SBAread(TMRCFG);
=> SBAreti;
------------------------------ MAIN PROGRAM ------------------------------------

-- /L:Init
=> counter:=1; capture:=0;
=> SBAwrite(TMRCHS,0);          -- Select timer 0
=> SBAwrite(TMRDATL,x"4B40");   -- Write to LSW, (100'000,000 = 5F5E100)
=> SBAwrite(TMRDATH,x"004C");   -- Write to MSW
=> SBAwrite(TMRCFG,"0X11");     -- Disable output, Enable timer interrupt
=> SBAinte(true);               -- Enable interrupts

-- /L:SendMsg
=> Idx:=0;
=> RSTmp:=chr2uns(vMsg(Idx)); SBAcall(UARTSendChar);
=> if Idx<vMsg'length-1 then inc(Idx); SBAjump(SendMsg+1); end if;
=> RSTmp:=x"0D"; SBAcall(UARTSendChar);
=> RSTmp:=x"0A"; SBAcall(UARTSendChar);

-- /L:LoopMain
=> if (capture=0) then
     SBAjump(LoopMain);
   else
     capture:=0;
     inc(counter);
   end if;

=> SBAread(TC1R0);
=> TCR0:=dati;
=> SBAread(TC1R1);
=> TCR1:=dati;
=> SBAwrite(GPIO,TCR0);
=> T:=TCR1(14 downto 2); Sign:=TCR1(15);
=> bin_in:=T&"00"; SBAcall(Bin2BCD);
=> RSTmp:=hex(x"0" & bcd_out(19 downto 16)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(15 downto 12)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(11 downto 08)); SBAcall(UARTSendChar);
=> RSTmp:=chr2uns('.'); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(07 downto 04)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(03 downto 00)); SBAcall(UARTSendChar);
=> RSTmp:=x"0D"; SBAcall(UARTSendChar);
=> RSTmp:=x"0A"; SBAcall(UARTSendChar);

=> SBAjump(LoopMain);

-- /SBA: End User Program ------------------------------------------------------

