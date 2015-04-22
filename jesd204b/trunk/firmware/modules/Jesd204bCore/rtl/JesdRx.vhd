-------------------------------------------------------------------------------
-- Title      : JesdRx module 
-------------------------------------------------------------------------------
-- File       : 
-- Author     : Uros Legat  <ulegat@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory (Cosylab)
-- Created    : 2015-04-14
-- Last update: 2015-04-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Receiver JESD204b standard.
-- 
-------------------------------------------------------------------------------
-- Copyright (c) 2014 SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.StdRtlPkg.all;
use work.Jesd204bPkg.all;

entity JesdRx is
   generic (
      TPD_G            : time                := 1 ns;

      -- Number of bytes in a frame
      F_G : positive := 2;
      
      -- Number of frames in a multi frame
      K_G : positive := 32;
           
      --Transceiver word size (GTP,GTX,GTH)
      GT_WORD_SIZE_G : positive := 4;
      
      --JESD204B class (0 and 1 supported)
      SUB_CLASS_G : positive := 1
   );
   port (
     
   -- JESD
      -- Clocks and Resets   
      devClk_i       : in    sl;    
      devRst_i       : in    sl;
      
      -- SYSREF for subcalss 1 fixed latency
      sysRef_i       : in    sl;
      
      -- Control and status register records
      r_ctrlReg_i    : in    ctrlRegType;
      r_statReg_o    : out   statRegType;
      
      -- Data and character inputs from GT (transceivers)
      dataRx_i       : in    slv((GT_WORD_SIZE_G*8)-1 downto 0);       
      chariskRx_i    : in    slv(GT_WORD_SIZE_G-1 downto 0);
      
      
      -- Local multi frame clock
      lmfc_i         : in    sl;
      
      -- All of the RX modules are ready for synchronisation
      nSyncAll_i       : in    sl;

      -- One or more RX modules requested synchronisation
      nSyncAny_i       : in    sl;
      
      -- Synchronisation request output 
      nSync_o        : out   sl;

      -- Synchronisation process is complete and data is valid
      dataValid_o    : out   sl;
      sampleData_o   : out   slv((GT_WORD_SIZE_G*8)-1 downto 0)  
    );
end JesdRx;


architecture rtl of JesdRx is

-- Register
   type RegType is record
      bufWeD1 : sl;
   end record RegType;

   constant REG_INIT_C : RegType := (
      bufWeD1  => '0'
   );

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;



-- Internal signals

-- Control signals from FSM
signal s_nSync      : sl;
signal s_readBuff   : sl;
signal s_alignFrame : sl;
signal s_ila        : sl;
signal s_dataValid  : sl;
      
-- Buffer control
signal s_bufRst   : sl;
signal s_bufWe    : sl;
signal s_bufRe    : sl;

-- Datapath
signal s_charAndData     : slv(((GT_WORD_SIZE_G*8)+GT_WORD_SIZE_G)-1 downto 0);
signal s_charAndDataBuff : slv(s_charAndData'range);

-- Statuses
signal s_bufOvf  : sl;
signal s_bufUnf  : sl;
signal s_bufFull : sl;
signal s_linkErr : sl;


begin
   
   -- Input assignment
   s_charAndData  <= chariskRx_i & dataRx_i;
   
   -- Buffer control
   s_bufRst     <= devRst_i or not s_nSync or not r_ctrlReg_i.enable;
   s_bufWe      <= not s_bufRst and not s_bufFull;
   s_bufRe      <= r.bufWeD1 and s_readBuff;

   -- Buffer samples between first data and LMFC
   -- Min size one LMFC period
   RX_buffer_fifo_INST: entity work.FifoSync
   generic map (
      TPD_G          =>  TPD_G,
      RST_POLARITY_G =>  '1',
      RST_ASYNC_G    =>  false,
      BRAM_EN_G      =>  true,
      FWFT_EN_G      =>  false,
      USE_DSP48_G    =>  "no",
      ALTERA_SYN_G   =>  false,
      ALTERA_RAM_G   =>  "M9K",
      PIPE_STAGES_G  =>  0,
      DATA_WIDTH_G   =>  (GT_WORD_SIZE_G*8) + GT_WORD_SIZE_G,
      ADDR_WIDTH_G   =>  ((K_G * F_G)/GT_WORD_SIZE_G),
      INIT_G         =>  "0",
      FULL_THRES_G   =>  1,
      EMPTY_THRES_G  =>  1)
   port map (
      rst          => s_bufRst,
      clk          => devClk_i,
      wr_en        => s_bufWe,    -- Always write when enabled
      rd_en        => s_bufRe,    -- Hold read while sync not in sync with LMFC
      din          => s_charAndData,
      dout         => s_charAndDataBuff,
      data_count   => open,
      wr_ack       => open,
      valid        => open,
      overflow     => s_bufOvf,
      underflow    => s_bufUnf,
      prog_full    => open,
      prog_empty   => open,
      almost_full  => open,
      almost_empty => open,
      full         => s_bufFull,
      not_full     => open,
      empty        => open
   );

   -- Synchronisation FSM
   syncFSM_INST: entity work.syncFSM
   generic map (
      TPD_G          => TPD_G,
      F_G            => F_G,
      K_G            => K_G,
      GT_WORD_SIZE_G => GT_WORD_SIZE_G,
      SUB_CLASS_G    => SUB_CLASS_G)
   port map (
      clk          => devClk_i,
      rst          => devRst_i,
      enable_i     => r_ctrlReg_i.enable,
      sysRef_i     => sysRef_i,
      dataRx_i     => dataRx_i,
      chariskRx_i  => chariskRx_i,
      lmfc_i       => lmfc_i,
      nSyncAll_i   => nSyncAll_i,
      nSyncAny_i   => nSyncAny_i,
      linkErr_i    => s_linkErr, --TODO register link errors
      nSync_o      => s_nSync,
      readBuff_o   => s_readBuff,
      alignFrame_o => s_alignFrame,
      ila_o        => s_ila,
      dataValid_o  => s_dataValid
   );
   
   -- DFF
  comb : process (r, devRst_i, s_bufWe) is
      variable v : RegType;
   begin
      v.bufWeD1 := s_bufWe;

      if (devRst_i = '1') then
         v := REG_INIT_C;
      end if;

      rin <= v;
   end process comb;

   seq : process (devClk_i) is
   begin
      if (rising_edge(devClk_i)) then
         r <= rin after TPD_G;
      end if;
   end process seq;

   -- Output assignment
   nSync_o      <= s_nSync or not r_ctrlReg_i.enable;
   dataValid_o  <= s_dataValid;
   sampleData_o <= s_charAndDataBuff((GT_WORD_SIZE_G*8)-1 downto 0); -- TODO just temporary connection for first tests

   
end rtl;