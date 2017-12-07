library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_TEXTIO.ALL;
use STD.TEXTIO.ALL;
use work.StdRtlPkg.ALL;

entity Si534xInitRom is
	generic (
		TPD_G : time := 1 ns;
		ADDR_WIDTH_G : integer := 10;
		INIT_FILE_G : string
	);
	port (
		clk : in sl;
		addr : in slv(ADDR_WIDTH_G-1 downto 0);
		data : out slv(31 downto 0)
	);
end Si534xInitRom;

architecture rtl of Si534xInitRom is
	type RomType is array (0 to (2**ADDR_WIDTH_G)-1) of slv(31 downto 0);

	impure function InitRomFromFile(fileName : in string) return RomType is
		FILE romFile : text is in fileName;
		variable romFileLine : line;
		variable rom : RomType;
	begin
		for I in RomType'range loop
			readline(romFile, romFileLine);
			hread(romFileLine, rom(I));
		end loop;

		return rom;
	end function;

	signal rom : RomType := InitRomFromFile(INIT_FILE_G);
begin

-- output process
process (clk) begin
	if rising_edge(clk) then
		data <= rom(to_integer(unsigned(addr))) after TPD_G;
	end if;
end process;

end architecture;
