-------------------------------------------------------------------------------
-- File       : Si534xSpi.vhd
-- Company    : Bergische Universitaet Wuppertal
-- Created    : 2017-12-05
-- Last update: 2017-12-05
-------------------------------------------------------------------------------
-- Description:
-- Si534x SPI interface wrapper
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

entity Si534xSpi is
    generic (
        TPD_G         : time    := 1 ns;
        CLK_PERIOD_G  : real    := 8.0E-9
    );
    port (
        -- clock and reset
        clk : in sl;
        rst : in sl;

        -- SPI interface
        spiSCK : out sl;
        spiMOSI : out sl;
        spiMISO : in sl;
        spiCS_n : out sl;

        -- command interface
        regAddress : in slv(15 downto 0);
        regReadValue : out slv(7 downto 0);
        regWriteValue : in slv(7 downto 0);
        regRead : in sl;
        regWrite : in sl;
        regBusy : out sl
    );
end Si534xSpi;

architecture rtl of Si534xSpi is

    type StateType is (IDLE_S, READ1_S, READ2_S, READ3_S, READ4_S, WRITE1_S, WRITE2_S, WAIT_S);

    type RegType is record
        state : StateType;
        stateReturn : StateType;
        spiDataSize : slv(4 downto 0);
        spiWrData : slv(23 downto 0);
        spiWrEn : sl;
        regReadValue : slv(7 downto 0);
    end record;

    constant REG_INIT_C : RegType := (
        state => IDLE_S,
        stateReturn => IDLE_S,
        spiDataSize => "00000",
        spiWrData => x"000000",
        spiWrEn => '0',
        regReadValue => x"00"
    );

    signal r : RegType := REG_INIT_C;
    signal rin : RegType;

    signal spiRdEn : sl;
    signal spiRdData : slv(23 downto 0);

begin

spiMaster: entity work.SpiMaster
    generic map (
        TPD_G => TPD_G,
        NUM_CHIPS_G => 1,
        DATA_SIZE_G => 24,
        CPHA_G => '0',
        CPOL_G => '0',
        CLK_PERIOD_G => CLK_PERIOD_G,
        SPI_SCLK_PERIOD_G => 1.0e-6
    )
    port map (
        clk => clk,
        sRst => rst,
        chipSel => "0",
        wrEn => r.spiWrEn,
        wrData => r.spiWrData,
        rdEn => spiRdEn,
        rdData => spiRdData,
        spiCsL(0) => spiCS_n,
        spiSclk => spiSCK,
        spiSdi => spiMOSI,
        spiSdo => spiMISO
    );

comb: process (regAddress, regWriteValue, regRead, regWrite, spiRdEn, spiRdData, rst, r)
    variable v : RegType;
begin
    -- save registers
    v := r;

    -- state machine
    case v.state is
        when IDLE_S =>
            -- check read/write command
            if regRead = '1' then
                v.state := READ1_S;
            elsif regWrite = '1' then
                v.state := WRITE1_S;
            end if;

        when WRITE1_S =>
            v.spiWrData := "11100000" & "00000001" & regAddress(15 downto 8);
            v.spiDataSize := toSlv(24-1, log2(v.spiDataSize'length));
            if spiRdEn = '0' then
                v.spiWrEn := '0';
                v.state := WAIT_S;
                v.stateReturn := WRITE2_S;
            else
                v.spiWrEn := '1';
                v.state := WRITE1_S;
            end if;

        when WRITE2_S =>
            v.spiWrData := "11100000" & regAddress(7 downto 0) & regWriteValue;
            v.spiDataSize := toSlv(24-1, log2(v.spiDataSize'length));
            if spiRdEn = '0' then
                v.spiWrEn := '0';
                v.state := WAIT_S;
                v.stateReturn := IDLE_S;
            else
                v.spiWrEn := '1';
                v.state := WRITE2_S;
            end if;

        when READ1_S =>
            v.spiWrData := "11100000" & "00000001" & regAddress(15 downto 8);
            v.spiDataSize := toSlv(24-1, log2(v.spiDataSize'length));
            if spiRdEn = '0' then
                v.spiWrEn := '0';
                v.state := WAIT_S;
                v.stateReturn := READ2_S;
            else
                v.spiWrEn := '1';
                v.state := READ1_S;
            end if;

        when READ2_S =>
            v.spiWrData := "00000000" & regAddress(7 downto 0) & "00000000";
            v.spiDataSize := toSlv(16-1, log2(v.spiDataSize'length));
            if spiRdEn = '0' then
                v.spiWrEn := '0';
                v.state := WAIT_S;
                v.stateReturn := READ3_S;
            else
                v.spiWrEn := '1';
                v.state := READ2_S;
            end if;


        when READ3_S =>
            v.spiWrData := "100000000000000000000000";
            v.spiDataSize := toSlv(16-1, log2(v.spiDataSize'length));
            v.spiWrEn := '1';
            if spiRdEn = '0' then
                v.spiWrEn := '0';
                v.state := WAIT_S;
                v.stateReturn := READ4_S;
            else
                v.spiWrEn := '1';
                v.state := READ3_S;
            end if;

        when READ4_S =>
            v.regReadValue := spiRdData(15 downto 8);
            v.state := IDLE_S;

        when WAIT_S =>
            v.spiWrEn := '0';
            if spiRdEn = '1' then
                v.state := v.stateReturn;
            end if;
    end case;

    -- reset
    if rst = '1' then
        v := REG_INIT_C;
    end if;

    -- update registers
    rin <= v;

    -- outputs
    if r.state = IDLE_S then
        regBusy <= '0';
    else
        regBusy <= '1';
    end if;
    regReadValue <= r.regReadValue;
end process;

seq: process (clk) begin
    if rising_edge(clk) then
        r <= rin after TPD_G;
    end if;
end process;

end architecture;
