-------------------------------------------------------------------------------
-- File       : Si534xSpiTb.vhd
-- Company    : Bergische Universitaet Wuppertal
-- Created    : 2017-12-05
-- Last update: 2017-12-05
-------------------------------------------------------------------------------
-- Description:
-- Testbench for Si534xSpi module
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

entity Si534xSpiTb is
end Si534xSpiTb;

architecture tb of Si534xSpiTb is
    constant TPD_C : time := 1.0 ns;
    constant CLK_PERIOD_C : time := 10.0 ns;

    -- clock and reset
    signal clk : sl;
    signal rst : sl;

    -- SPI interface
    signal spiSCK : sl;
    signal spiMOSI : sl;
    signal spiMISO : sl;
    signal spiCS_n : sl;

    -- command interface
    signal regAddress : slv(15 downto 0);
    signal regReadValue : slv(7 downto 0);
    signal regWriteValue : slv(7 downto 0);
    signal regRead : sl;
    signal regWrite : sl;
    signal regBusy : sl;

begin

-- clock and reset generator
clk_reset_gen: entity work.ClkRst
    generic map (
        CLK_PERIOD_G => CLK_PERIOD_C,
        RST_START_DELAY_G => 0.0 ns,
        RST_HOLD_TIME_G => 100.0 ns,
        SYNC_RESET_G => true
    )
    port map (
        clkP => clk,
        clkN => open,
        rst => rst,
        rstL => open
    );

-- DUT
dut: entity work.Si534xSpi
    generic map (
        TPD_G => TPD_C,
        CLK_PERIOD_G => 10.0e-9
    )
    port map (
        clk => clk,
        rst => rst,
        spiSCK => spiSCK,
        spiMISO => spiMISO,
        spiMOSI => spiMOSI,
        spiCS_n => spiCS_n,
        regAddress => regAddress,
        regReadValue => regReadValue,
        regWriteValue => regWriteValue,
        regRead => regRead,
        regWrite => regWrite,
        regBusy => regBusy
    );

-- simple return for MISO
spiMISO <= spiMOSI;

-- stimulus
process begin
    -- default values
    regAddress <= x"0000";
    regWriteValue <= x"00";
    regWrite <= '0';
    regRead <= '0';

    -- start a write
    wait for 200 ns;        -- wait a bit
    wait until rising_edge(clk);
    regAddress <= x"55AA";
    regWriteValue <= x"12";
    regWrite <= '1';
    wait until rising_edge(clk);
    regWrite <= '0';

    -- wait until ready
    wait until regBusy = '0';

    -- start a read
    wait for 200 ns;        -- wait a bit
    wait until rising_edge(clk);
    regAddress <= x"55AA";
    regRead <= '1';
    wait until rising_edge(clk);
    regRead <= '0';

    -- wait until ready
    wait until regBusy = '0';

    -- finish
    wait;
end process;



end architecture;