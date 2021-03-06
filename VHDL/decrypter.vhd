----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:19:15 02/01/2017 
-- Design Name: 
-- Module Name:    decrypter - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity decrypter is
    Port ( ciphertext : in  STD_LOGIC_VECTOR (63 downto 0);
           start : in  STD_LOGIC;
           plaintext : out  STD_LOGIC_VECTOR (63 downto 0);
           done : out  STD_LOGIC;
			  reset: in STD_LOGIC;
           clk : in  STD_LOGIC);
end decrypter;

architecture Behavioral of decrypter is

signal i:integer range 32 downto 0:=0;
signal do_calc:STD_LOGIC:='0';
signal sum: UNSIGNED (31 downto 0);
constant delta:UNSIGNED:=x"9E3779B9";
signal v0:STD_LOGIC_VECTOR (31 downto 0);
signal v1:STD_LOGIC_VECTOR (31 downto 0);
signal output:STD_LOGIC_VECTOR (63 downto 0):=(others=>'0');
signal calc_over:STD_LOGIC:='0';
constant k0:UNSIGNED:="10101010101010101010101010101010";
constant k1:UNSIGNED:="10101010101010101010101010101010";
constant k2:UNSIGNED:="10101010101010101010101010101010";
constant k3:UNSIGNED:="10101010101010101010101010101010";
begin
plaintext<=output;
done<=calc_over;
	process(reset,start, clk)
	variable counter_update:integer:=0;
	variable newV0:UNSIGNED (31 downto 0):=(others=>'0');
	variable newV1:UNSIGNED (31 downto 0):=(others=>'0');
	begin
		if(reset='1') then
			i<=0;
			do_calc<='0';
			calc_over<='0';
			output<=(others=>'0');
			sum<=delta;
			v0<=(others=>'0');
			v1<=(others=>'0');
		--- Setting signals in order to begin encryption.
		elsif(Rising_edge(clk)) then
			if(start='1') then
				do_calc<='1';
				i<=0;
				calc_over<='0';
				output<=(others=>'0');
				sum<=x"C6EF3720";
				v0<=(others=>'0');
				v1<=(others=>'0');
			else
				-- Decryption begins when start goes to zero.
				if(do_calc='1' and i<33) then
					if(i=0) then
						v0<=ciphertext(31 downto 0);
						v1<=ciphertext(63 downto 32);
					else
						newV0:=unsigned(v0);
						newV1:=unsigned(v1);
						newV1:=newV1-(((newV0(27 downto 0)&"0000")+k2) XOR (newV0+sum) XOR (("00000"& newV0(31 downto 5))+k3));
						newV0:=newV0-(((newV1(27 downto 0)&"0000")+k0) XOR (newV1+sum) XOR (("00000"& newV1(31 downto 5))+k1));
						v0<=std_logic_vector(newV0);
						v1<=std_logic_vector(newV1);
						sum<=sum-delta;
					end if;
				end if;
				if(i=32) then
					do_calc<='0';
				end if;
				if(do_calc='1' and i=32) then
					output<=std_logic_vector(newV1(31 downto 0)) & std_logic_vector(newV0(31 downto 0)); 
					calc_over<='1';
				end if;
				counter_update:=i;
				if(counter_update=32) then
					i<=0;
				else
					i<=counter_update+1;
				end if;
			end if;
		end if;
	end process;

end Behavioral;

