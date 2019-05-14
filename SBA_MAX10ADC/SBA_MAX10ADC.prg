-- /SBA: Program Details =======================================================
-- Project Name: SBA_MAX10ADC
-- Title: Test Project for MAX10ADC
-- Version: 0.1.1
-- Date: 2019/05/14
-- Project Author: Miguel A. Risco Castillo
-- Description: Test Project for MAX10  ADC internal module. Use Quartus IP 
-- Catalog for the instantiation of the ADC
-- /SBA: End Program Details ---------------------------------------------------

-- /SBA: User Registers and Constants ==========================================

   constant ms         : positive:= positive(real(sysfreq)/real(1000)+0.499)-1;
   variable DlyReg_1ms : positive; -- Constant Delay of 1ms
   variable Dlytmp_ms  : positive; -- Delay register in ms
   variable ADREG      : std_logic_vector(15 downto 0);

-- /SBA: End User Registers and Constants --------------------------------------

-- /SBA: User Program ==========================================================

=> SBAjump(Init);            -- Reset Vector (001)
=> SBAjump(INT);             -- Interrupt Vector (002)

------------------------------ ROUTINES ----------------------------------------
-- /L:Delay_ms
=> DlyReg_1ms:=ms;
=> if DlyReg_1ms/=0 then
     dec(DlyReg_1ms);
     SBAjump(Delay_ms+1);
   end if;
=> if Dlytmp_ms/=0 then
     dec(Dlytmp_ms);
     SBAjump(Delay_ms);
   else
     SBARet;
   end if;

------------------------------ INTERRUPT ---------------------------------------
-- /L:INT
=> SBAWait;                  -- Start your interrupt routine here
=> SBAreti;
------------------------------ MAIN PROGRAM ------------------------------------

-- /L:Init
=> Dlytmp_ms:=200;
   SBAcall(Delay_ms);

-- /L:MainLoop
=> SBAread(ADR0);
=> ADREG:=dati;
=> SBAwrite(GPIO,ADREG);
=> Dlytmp_ms:=500;
   SBAcall(Delay_ms);
=> SBAjump(MainLoop);

-- /SBA: End User Program ------------------------------------------------------

