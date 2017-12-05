-------------------------------------------------------------------------------
-- File       : Si534x.vhd
-- Company    : Bergische Universitaet Wuppertal
-- Created    : 2017-12-05
-- Last update: 2017-12-05
-------------------------------------------------------------------------------
-- Description:
-- Si534x controller with AXI interface
-- Controls Si534x chip with SPI interface
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

entity Si534x is
    generic (
        TPD_G             : time                  := 1 ns;

        -- AXI-lite clock frequency and error behaviour
        AXI_CLK_PERIOD_G  : real                  := 8.0E-9;
        AXI_ERROR_RESP_G  : slv(1 downto 0)       := AXI_RESP_SLVERR_C
    );
    port (
        -- SPI interface
        spiSCK : out sl;
        spiMOSI : out sl;
        spiMISO : in sl;
        spiCS_n : out sl;

        -- status/reset
        intr_n : in sl := '1';
        losXAXB : in sl := '0';
        lol_n : in sl := '1';
        rst_n : out sl;

        -- AXI-lite interface
        axilClk : in sl;
        axilRst : in sl;
        axilWriteMaster : in AxiLiteWriteMasterType;
        axilWriteSlave : out AxiLiteWriteSlaveType;
        axilReadMaster : in AxiLiteReadMasterType;
        axilReadSlave : out AxiLiteReadSlaveType;

        -- interrupt output
        intOut : out sl
    );
end Si534x;

architecture rtl of Si534x is
    type StateType is (IDLE_S, READ_WAIT_S, WRITE_WAIT_S);

    type RegType is record
        axilWriteSlave : AxiLiteWriteSlaveType;
        axilReadSlave : AxiLiteReadSlaveType;
        state : StateType;
        regAddress : slv(15 downto 0);
        regValue : slv(7 downto 0);
        regRead : sl;
        regWrite : sl;
        manualRst : sl;
    end record;

    constant REG_INIT_C : RegType := (
        axilWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C,
        axilReadSlave => AXI_LITE_READ_SLAVE_INIT_C,
        state => IDLE_S,
        regAddress => (others => '0'),
        regValue => (others => '0'),
        regRead => '0',
        regWrite => '0',
        manualRst => '0'
    );

    signal regBusy : sl := '0';
    signal regReadValue : slv(7 downto 0) := (others => '0');

    -- synchronized inputs
    signal intr_sync : sl;
    signal losXAXB_sync : sl;
    signal lol_sync : sl;


    signal r : RegType := REG_INIT_C;
    signal rin : RegType;
begin

-- synchronizer
intr_syncer: entity work.Synchronizer
    generic map (
        TPD_G => TPD_G,
        OUT_POLARITY_G => '0'
    )
    port map (
        clk => axilClk,
        rst => axilRst,
        dataIn => intr_n,
        dataOut => intr_sync
    );

losXAXB_syncer: entity work.Synchronizer
    generic map (
        TPD_G => TPD_G,
        OUT_POLARITY_G => '1'
    )
    port map (
        clk => axilClk,
        rst => axilRst,
        dataIn => losXAXB,
        dataOut => losXAXB_sync
    );

lol_syncer: entity work.Synchronizer
    generic map (
        TPD_G => TPD_G,
        OUT_POLARITY_G => '0'
    )
    port map (
        clk => axilClk,
        rst => axilRst,
        dataIn => lol_n,
        dataOut => lol_sync
    );

-- output interrupt
intOut <= intr_sync;

-- instance for SPI interface
spi: entity work.Si534xSpi
    generic map (
        TPD_G => TPD_G,
        CLK_PERIOD_G => AXI_CLK_PERIOD_G
    )
    port map (
        clk => axilClk,
        rst => axilRst,
        spiSCK => spiSCK,
        spiMOSI => spiMOSI,
        spiMISO => spiMISO,
        spiCS_n => spiCS_n,
        regAddress => r.regAddress,
        regWriteValue => r.regValue,
        regReadValue => regReadValue,
        regWrite => r.regWrite,
        regRead => r.regRead,
        regBusy => regBusy
    );

-- combinatorial process
comb: process (axilRst, axilWriteMaster, axilReadMaster, r, regBusy, regReadValue, intr_sync, losXAXB_sync, lol_sync)
    variable v : RegType;
    variable axilEp : AxiLiteEndpointType;
begin
    -- get current register value
    v := r;

    -- AXI-lite registers
    axiSlaveWaitTxn(axilEp, axilWriteMaster, axilReadMaster, v.axilWriteSlave, v.axilReadSlave);
    axiSlaveRegister(axilEp, x"000", 0, v.regRead);
    axiSlaveRegister(axilEp, x"000", 1, v.regWrite);
    axiSlaveRegisterR(axilEp, x"000", 2, regBusy);
    axiSlaveRegisterR(axilEp, x"000", 8, intr_sync);
    axiSlaveRegisterR(axilEp, x"000", 9, losXAXB_sync);
    axiSlaveRegisterR(axilEp, x"000", 10, lol_sync);
    axiSlaveRegister(axilEp, x"000", 16, v.manualRst);
    axiSlaveRegister(axilEp, x"008", 0, v.regAddress);
    axiSlaveRegister(axilEp, x"00C", 0, v.regValue);
    axiSlaveDefault(axilEp, v.axilWriteSlave, v.axilReadSlave, AXI_ERROR_RESP_G);

    -- state machine
    case v.state is
        when IDLE_S =>
            if r.regRead = '1' then
                v.state := READ_WAIT_S;
                v.regRead := '0';
            elsif r.regWrite = '1' then
                v.state := WRITE_WAIT_S;
                v.regWrite := '0';
            end if;

        when READ_WAIT_S =>
            if regBusy = '0' then
                v.regValue := regReadValue;
                v.state := IDLE_S;
            end if;

        when WRITE_WAIT_S =>
            if regBusy = '0' then
                v.state := IDLE_S;
            end if;
    end case;

    -- update next register value signal
    rin <= v;

    -- outputs
    axilWriteSlave <= r.axilWriteSlave;
    axilReadSlave <= r.axilReadSlave;
    rst_n <= not (axilRst or r.manualRst);
end process;

-- sequential process
seq: process (axilClk) begin
    if rising_edge(axilClk) then
        r <= rin after TPD_G;
    end if;
end process;

end architecture;