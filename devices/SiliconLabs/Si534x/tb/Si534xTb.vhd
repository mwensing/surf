-------------------------------------------------------------------------------
-- File       : Si534xTb.vhd
-- Company    : Bergische Universitaet Wuppertal
-- Created    : 2017-12-05
-- Last update: 2017-12-05
-------------------------------------------------------------------------------
-- Description:
-- Testbench for Si534x controller with AXI interface
-------------------------------------------------------------------------------
-- This file is part of 'SLAC Firmware Standard Library'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'SLAC Firmware Standard Library', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.StdRtlPkg.ALL;
use work.AxiLitePkg.ALL;

entity Si534xTb is
end Si534xTb;

architecture tb of Si534xTb is
    constant TPD_C : time := 1.0 ns;
    constant AXI_CLK_PERIOD_C : real := 10.0e-9;
    constant AXI_ERROR_RESP_C  : slv(1 downto 0) := AXI_RESP_SLVERR_C;

    -- SPI interface
    signal spiSCK : sl;
    signal spiMOSI : sl;
    signal spiMISO : sl;
    signal spiCS_n : sl;

    -- status/reset
    signal intr_n : sl;
    signal losXAXB : sl;
    signal lol_n : sl;
    signal rst_n : sl;

    -- AXI-lite interface
    signal axilClk : sl;
    signal axilRst : sl;
    signal axilWriteMaster : AxiLiteWriteMasterType;
    signal axilWriteSlave : AxiLiteWriteSlaveType;
    signal axilReadMaster : AxiLiteReadMasterType;
    signal axilReadSlave : AxiLiteReadSlaveType;

    -- interrupt output
    signal intOut : sl;
begin

-- clock and reset generator
clk_reset_gen: entity work.ClkRst
    generic map (
        CLK_PERIOD_G => AXI_CLK_PERIOD_C * 1000.0 ms,
        RST_START_DELAY_G => 0.0 ns,
        RST_HOLD_TIME_G => 100.0 ns,
        SYNC_RESET_G => true
    )
    port map (
        clkP => axilClk,
        clkN => open,
        rst => axilRst,
        rstL => open
    );

-- keep status inputs at fixed value
intr_n <= '1';
lol_n <= '1';
losXAXB <= '0';

-- route back data
spiMISO <= spiMOSI;

-- dut
dut: entity work.Si534x
    generic map (
        TPD_G => TPD_C,
        AXI_CLK_PERIOD_G => AXI_CLK_PERIOD_C,
        AXI_ERROR_RESP_G => AXI_ERROR_RESP_C
    )
    port map (
        axilClk => axilClk,
        axilRst => axilRst,
        axilWriteMaster => axilWriteMaster,
        axilWriteSlave => axilWriteSlave,
        axilReadMaster => axilReadMaster,
        axilReadSlave => axilReadSlave,
        spiMISO => spiMISO,
        spiMOSI => spiMOSI,
        spiSCK => spiSCK,
        spiCS_n => spiCS_n,
        intr_n => intr_n,
        lol_n => lol_n,
        losXAXB => losXAXB,
        intOut => intOut
    );

-- stiumulus
process
    variable tmp : slv(31 downto 0);
begin
    -- set address and perform a read
    wait for 200 ns;
    axiLiteBusSimWrite(axilClk, axilWriteMaster, axilWriteSlave, x"00000008", x"000055aa", true);
    wait for 200 ns;
    axiLiteBusSimWrite(axilClk, axilWriteMaster, axilWriteSlave, x"00000000", x"00000001", true);


    -- poll busy
    loop
        wait for 200 ns;
        axiLiteBusSimRead(axilClk, axilReadMaster, axilReadSlave, x"00000000", tmp, true);

        if tmp(2) = '0' then
            exit;
        end if;
    end loop;

    -- set address/data and perform a write
    wait for 200 ns;
    axiLiteBusSimWrite(axilClk, axilWriteMaster, axilWriteSlave, x"00000008", x"000055aa", true);
    wait for 200 ns;
    axiLiteBusSimWrite(axilClk, axilWriteMaster, axilWriteSlave, x"0000000C", x"00000012", true);
    wait for 200 ns;
    axiLiteBusSimWrite(axilClk, axilWriteMaster, axilWriteSlave, x"00000000", x"00000002", true);

    -- poll busy
    loop
        wait for 200 ns;
        axiLiteBusSimRead(axilClk, axilReadMaster, axilReadSlave, x"00000000", tmp, true);

        if tmp(2) = '0' then
            exit;
        end if;
    end loop;

    -- wait forever
    wait;
end process;


end architecture;