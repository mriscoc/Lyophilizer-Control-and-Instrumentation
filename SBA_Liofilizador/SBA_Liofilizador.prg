-- /SBA: Program Details =======================================================
-- Project Name: SBA_Liofilizador
-- Title: Control Principal SBA
-- Version: 0.2.3
-- Date: 2019/06/20
-- Project Author: Miguel A. Risco Castillo
-- Description: Sistema de control e instrumentación para el Liofilizador
-- /SBA: End Program Details ---------------------------------------------------

-- /SBA: User Registers and Constants ==========================================

  variable counter : natural range 0 to 65535;  -- Simple counter
  variable timerf  : std_logic;                 -- timer interrupt flag
  variable Idx     : natural;                   -- General purpose index
  variable T       : signed(15 downto 0);       -- Temperature register
  variable Sign    : std_logic;                 -- Sign bit
  variable T1      : signed(19 downto 0);       -- Accumulator Filter Ch1
  variable T2      : signed(19 downto 0);       -- Accumulator Filter Ch2
  variable T3      : signed(19 downto 0);       -- Accumulator Filter Ch3
  constant beta    : integer:=3;                -- EMA beta factor (srl 3 = 1/8)
  variable W       : signed(23 downto 0);       -- Hx711 Weight variable
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
  alias    LLO     : std_logic is DOUT(2);      -- LED low temperature
  alias    LOK     : std_logic is DOUT(3);      -- LED temperature ok
  alias    LHI     : std_logic is DOUT(4);      -- LED high temperature
  alias    TCTRL   : std_logic is DOUT(8);      -- Temperature Control
  type tarrchar is array (natural range <>) of character;
  constant vMsg    : tarrchar (0 to 18):="Liofilizador v0.2.3";

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
   T3:=(others=>'0');T2:=(others=>'0');T1:=(others=>'0');
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
=> SBAwrite(ADFMAX,x"8003");    -- Continous mode, K type thermocuple

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

-- EMA Filter / Discrete IIR LPF, beta=0.125, FixedPoint mul=8 (1=8*0.125)
-- T(i) = beta*T + (1-beta)*T(i-1)
-- 8*T(i) = 8*0.125*T + 8*T(i-1) - 8*0.125*T(i-1) = T + 8*T(i-1) - T(i-1)
-- Change var: T1(i)=8*T(i), T1(i-1)=8*T(i-1)
-- T1(i) = T + T1(i-1) - T1(i-1)/8
-- T(i) = T1(i)/8
=> T1:= T + T1 - (T1 srl beta);                      -- EMA Filter (beta=1/8)
=> T:=Resize(T1 srl beta,T'length);                  -- T(i) = T1(i)/8
   bin_in:=unsigned(T); SBAcall(Bin2BCD);            -- Filtered
=> SBACall(UARTSendBCD);

=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);       -- Value separator

=> SBAread(TC1R1);                                   -- Thermocuple temperature
=> T:=Resize(25*signed(dati(15 downto 2)),T'length); -- 25x4xT=100xT

=> T2:= T + T2 - (T2 srl beta);                      -- EMA Filter (beta=1/8)
=> T:=Resize(T2 srl beta,T'length);                  -- T(i) = T2(i)/8
   bin_in:=unsigned(T); SBAcall(Bin2BCD);            -- Filtered
=> SBACall(UARTSendBCD);

=> LHI := '0';
   LOK := '1';
   LLO := '0';
   if T>SetTH then
   begin
     TCTRL:='0';                                     -- Calefactor Control
     LHI := '1';
     LOK := '0';
   end if;
   if T<SetTL then
     TCTRL:='1';
     LOK := '0';
     LLO := '1';
   end if;
   L1:=TCTRL;

=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);       -- Value separator

=> SBAread(ADFMAX);
=> T:=signed(dati);

=> T3:= T + T3 - (T3 srl beta);                      -- EMA Filter (beta=1/8)
=> T:=Resize(T3 srl beta,T'length);                  -- T(i) = T1(i)/8
=> bin_in:=unsigned(T); SBAcall(Bin2BCD);
=> SBAcall(UARTSendBCD);

=> RSTmp:=chr2uns(';'); SBAcall(UARTSendChar);       -- Value separator

=> SBAread(HXR0);                                    -- Read Hx711 LSW
=> R0:=unsigned(dati);
=> SBAread(HXR1);                                    -- Read Hx711 MSW
=> W:=-619264 - signed(dati(7 downto 0) & R0);       -- Ajuste
   bin_in:=unsigned(W(23 downto 8));
   SBAcall(Bin2BCD);
=> SBAcall(UARTSendBCD);

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

