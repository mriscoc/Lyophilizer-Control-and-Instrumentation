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
  variable timerf  : std_logic;                 -- timer interrupt flag
  variable Idx     : natural;                   -- General purpose index
  variable T       : signed(15 downto 0);       -- Temperature register
  variable Sign    : std_logic;                 -- Sign bit
  variable T0      : signed(21 downto 0);       -- Filtered Temperature Ch0
  variable T1      : signed(21 downto 0);       -- Filtered Temperature Ch1
  constant TXRDY   : integer:=14;               -- TXRDY Flag is bit 14
  constant RXRDY   : integer:=15;               -- RXRDY Flag is bit 15
  variable UARTFlg : std_logic;                 -- aux bit for UART flags
  variable RSTmp   : unsigned(7 downto 0);      -- Temporal register for UART
  variable R0      : unsigned(15 downto 0);     -- General purpose register
  variable SetTH   : signed(15 downto 0);       -- Temperature set point High
  variable SetTL   : signed(15 downto 0);       -- Temperature set point Low
  variable DOUT    : unsigned(8 downto 0);      -- GPIO Data out
  alias    LB      : std_logic is DOUT(0);      -- Breath LED0
  alias    L1      : std_logic is DOUT(1);      -- LED1
  alias    TCTRL   : std_logic is DOUT(8);      -- Temperature Control
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
     SBAret;
   end if;

-- /L:UARTSendBCD
=> if Sign='1' then
     RSTmp:=chr2uns('-');
   else
     RSTmp:=chr2uns(' ');
   end if;
   SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(19 downto 16)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(15 downto 12)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(11 downto 08)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(07 downto 04)); SBAcall(UARTSendChar);
=> RSTmp:=hex(x"0" & bcd_out(03 downto 00)); SBAcall(UARTSendChar);
=> SBAret;

-- /L:Bin2BCD
=> sign:=bin_in(15);
   if (sign='1') then
     bin_in:=(not bin_in) + 1;
   end if;
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

-- /L:SendMsg
=> Idx:=0;
=> RSTmp:=chr2uns(vMsg(Idx)); SBAcall(UARTSendChar);
=> if Idx<vMsg'length-1 then inc(Idx); SBAjump(SendMsg+1); end if;
=> RSTmp:=x"0A"; SBAcall(UARTSendChar);
=> SBAret;

------------------------------ INTERRUPT ---------------------------------------
-- /L:INT
=> timerf:='1';
   LB:= not LB;
   SBAread(TMRCFG);
=> SBAreti;
------------------------------ MAIN PROGRAM ------------------------------------

-- /L:Init
=> counter:=0; timerf:='0';
   T0:=(others=>'0');T1:=(others=>'0');
   DOUT:=(others=>'0');
   TCTRL:='0';
   SetTH:=to_signed(-38,SetTH'length);
   SetTL:=to_signed(-40,SetTH'length);
=> SBAwrite(TMRCHS,0);          -- Select timer 0
=> SBAwrite(TMRDATL,x"4B40");   -- Write to LSW, (5'000,000 = 4C4B40)
=> SBAwrite(TMRDATH,x"004C");   -- Write to MSW
=> SBAwrite(TMRCFG,"0X11");     -- Disable output, Enable timer interrupt
=> SBAinte(true);               -- Enable interrupts
=> SBAcall(SendMsg);

-- /L:LoopMain
=> if (timerf='1') then
     SBAjump(SendTemperatureData);
     timerf:='0';
     inc(counter);
   end if;
   SBAwrite(GPIO,DOUT);

=> SBARead(UART0);               -- Read UART Status
=> UARTFlg := dati(15);          -- Read RxRdy flag
   RSTmp  := dati(7 downto 0);   -- Read Char in to RSTmp
=> if ((UARTFlg ='1') and (RSTmp = chr2uns('@'))) then
     SBAjump(GetData);           -- If is Start of header goto GetData
   end if;

-- /L:EndLoopMain
=> SBAjump(LoopMain);

-- /L:SendTemperatureData
=> RSTmp:=chr2uns('@'); SBAcall(UARTSendChar);       -- Start of frame "@#"
=> RSTmp:=chr2uns('#'); SBAcall(UARTSendChar);
=> RSTmp:=x"24"; SBAcall(UARTSendChar);              -- Frame Size = 36
=> RSTmp:=chr2uns('D'); SBAcall(UARTSendChar);       -- Data Frame

-- Send counter
=> bin_in:=to_unsigned(counter,bin_in'length); Sign:='0';
   SBAcall(Bin2BCD);
=> SBACall(UARTSendBCD);
=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);

=> SBAread(TC1R0);                                   -- Get Reference Juntion Temperature
=> T:=Resize(25*signed(dati(15 downto 4)),T'length); -- 25x8xT = 400xT
   T:=Resize(T(15 downto 2),T'length);               -- /4 = 100xT

=> bin_in:=unsigned(T); SBAcall(Bin2BCD);            -- Unfiltered
=> SBACall(UARTSendBCD);
=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);

=> T0:=Resize(T0(15 downto 0) & "00",T0'length) - T0;-- Filtered
=> T0:= T0 + Resize(T & "000",T0'length);
   T0:= resize(T0(21 downto 2),T0'length);
=> T:=T0(18 downto 3);
   bin_in:=T; SBAcall(Bin2BCD);
=> SBACall(UARTSendBCD);

=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);

=> SBAread(TC1R1);                                   -- Thermocuple temperature
=> T:=Resize(25*signed(dati(15 downto 2)),T'length); -- 25x4xT=100xT

=> bin_in:=unsigned(T); SBAcall(Bin2BCD);            -- Unfiltered
=> SBACall(UARTSendBCD);
=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);

=> T1:=Resize(T1(15 downto 0) & "00",T1'length) - T1;-- Filtered
=> T1:= T1 + Resize(T & "00",T1'length);
   T1:= resize(T1(21 downto 2),T1'length);
=> T:=T1(17 downto 2);
   bin_in:=T; SBAcall(Bin2BCD);
=> SBACall(UARTSendBCD);

=> if T>SetTH then TCTRL:='0'; end if;               -- Calefactor Control
   if T<SetTL then TCTRL:='1'; end if;
   L1:=TCTRL;

=> RSTmp:=x"0A"; SBAcall(UARTSendChar);              -- End of Frame
=> SBAjump(LoopMain);

-- Process data frame
-- /L:GetData
=> SBACall(UARTGetChar);
=> if (RsTmp/=chr2uns('#')) then SBAjump(LoopMain); end if;
=> SBACall(UARTGetChar);
=> Case RsTmp is
     when x"56"  => SBAjump(SendVersion);            -- 'V'  Request Version
     when x"43"  => SBAjump(SetConfig);              -- 'C'  Set Configuration
     when OTHERS => SBAjump(LoopMain);
   end case;

-- /L:SetConfig
=> SBACall(UARTGetChar);                             -- Get Temperature Set point H
=> R0(15 downto 12):=hex2uns(RSTmp)(3 downto 0);
   SBACall(UARTGetChar);
=> R0(11 downto  8):=hex2uns(RSTmp)(3 downto 0);
   SBACall(UARTGetChar);
=> R0( 7 downto  4):=hex2uns(RSTmp)(3 downto 0);
   SBACall(UARTGetChar);
=> R0( 3 downto  0):=hex2uns(RSTmp)(3 downto 0);
   SetTH:=signed(R0(15 downto 0));
   SBACall(UARTGetChar);                             -- Get Temperature Set point L
=> R0(15 downto 12):=hex2uns(RSTmp)(3 downto 0);
   SBACall(UARTGetChar);
=> R0(11 downto  8):=hex2uns(RSTmp)(3 downto 0);
   SBACall(UARTGetChar);
=> R0( 7 downto  4):=hex2uns(RSTmp)(3 downto 0);
   SBACall(UARTGetChar);
=> R0( 3 downto  0):=hex2uns(RSTmp)(3 downto 0);
   SetTL:=signed(R0(15 downto 0));
   SBAjump(LoopMain);

-- /L:SendVersion
=> RSTmp:=chr2uns('@'); SBAcall(UARTSendChar);       -- Start of frame "@#"
=> RSTmp:=chr2uns('#'); SBAcall(UARTSendChar);
=> RSTmp:=to_unsigned(1+vMsg'length,RSTmp'length);   -- Frame Size
   SBAcall(UARTSendChar);
=> SBAcall(SendMsg);
=> RSTmp:=x"0A"; SBAcall(UARTSendChar);              -- End of frame

=> SBAjump(LoopMain);


-- /SBA: End User Program ------------------------------------------------------

