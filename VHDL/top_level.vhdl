--
-- Copyright (C) 2009-2012 Chris McClelland
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top_level is
	Generic (
		N : integer range 99999999 downto 0 := 50000000;
		M : integer range 99999999 downto 0 := 10000000);
	port(
		-- FX2LP interface ---------------------------------------------------------------------------
		fx2Clk_in      : in    std_logic;                    -- 48MHz clock from FX2LP
		fx2Addr_out    : out   std_logic_vector(1 downto 0); -- select FIFO: "00" for EP2OUT, "10" for EP6IN
		fx2Data_io     : inout std_logic_vector(7 downto 0); -- 8-bit data to/from FX2LP

		-- When EP2OUT selected:
		fx2Read_out    : out   std_logic;                    -- asserted (active-low) when reading from FX2LP
		fx2OE_out      : out   std_logic;                    -- asserted (active-low) to tell FX2LP to drive bus
		fx2GotData_in  : in    std_logic;                    -- asserted (active-high) when FX2LP has data for us

		-- When EP6IN selected:
		fx2Write_out   : out   std_logic;                    -- asserted (active-low) when writing to FX2LP
		fx2GotRoom_in  : in    std_logic;                    -- asserted (active-high) when FX2LP has room for more data from us
		fx2PktEnd_out  : out   std_logic;                    -- asserted (active-low) when a host read needs to be committed early
		-- Onboard peripherals -----------------------------------------------------------------------
		sseg_out       : out   std_logic_vector(7 downto 0); -- seven-segment display cathodes (one for each segment)
		anode_out      : out   std_logic_vector(3 downto 0); -- seven-segment display anodes (one for each digit)
		
		------ Till this point no interference --------------
		
		led_out        : out   std_logic_vector(7 downto 0); -- eight LEDs
		sw_in          : in    std_logic_vector(7 downto 0);  -- eight switches
		-- Push Buttons; Need to be assigned locations in mapping file.
		reset		   : in    std_logic;					 
		start		   : in    std_logic;
		next_data_in   : in    std_logic;
		done		   : in    std_logic	            
		);
end entity;

architecture structural of top_level is
	
	component debouncer
        port(clk: in STD_LOGIC;
            button: in STD_LOGIC;
            button_deb: out STD_LOGIC);
    end component;
	
	component encrypter
        port(clk: in STD_LOGIC;
            reset : in  STD_LOGIC;
            plaintext: in STD_LOGIC_VECTOR (63 downto 0);
            start: in STD_LOGIC;
            ciphertext: out STD_LOGIC_VECTOR (63 downto 0);
            done: out STD_LOGIC);
    end component;
    
    component decrypter
        port(clk: in STD_LOGIC;
            reset : in  STD_LOGIC;
            ciphertext: in STD_LOGIC_VECTOR (63 downto 0);
            start: in STD_LOGIC;
            plaintext: out STD_LOGIC_VECTOR (63 downto 0);
            done: out STD_LOGIC);
    end component;
    
	type StateType is (
		Ready,
		Get_User_Input,
		Communicating_With_Backend,
		Loading_Cash,
		Dispensing_Cash
	);	
	type SubStateType is (
		-- 	Communicating_With_Backend
		Encryption,
		Writing_To_Backend,
		Reading_From_Backend,
		Decryption,
		Response_9,
		--  Dispensing_Cash
		ATM_Cash_Status,
		Suff_ATM_Cash,
		Insuff_ATM_Cash,
		Insuff_balance
	);
	
	-- Channel read/write interface -----------------------------------------------------------------
	signal chanAddr  : std_logic_vector(6 downto 0);  -- the selected channel (0-127)

	-- Host >> FPGA pipe:
	signal h2fData   : std_logic_vector(7 downto 0);  -- data lines used when the host writes to a channel
	signal h2fValid  : std_logic;                     -- '1' means "on the next clock rising edge, please accept the data on h2fData"
	--signal h2fReady  : std_logic:='1';                -- channel logic can drive this low to say "I'm not ready for more data yet"

	-- Host << FPGA pipe:
	signal f2hData,f2hData_next   : std_logic_vector(7 downto 0):=(others=>'0');  -- data lines used when the host reads from a channel
	--signal f2hValid  : std_logic:='1';                -- channel logic can drive this low to say "I don't have data ready for you"
	signal f2hReady  : std_logic;                     -- '1' means "on the next clock rising edge, put your next byte of data on f2hData"
	-- ----------------------------------------------------------------------------------------------

	-- Needed so that the comm_fpga_fx2 module can drive both fx2Read_out and fx2OE_out
	signal fx2Read   : std_logic;

	-- Reset signal so host can delay startup
	signal fx2Reset  : std_logic;

	-- Registers to store number of notes of 4 denominations
	signal n2000,n2000_next	 : unsigned(7 downto 0):=(others=>'0');
	signal n1000,n1000_next	 : unsigned(7 downto 0):=(others=>'0');
	signal n500,n500_next	 : unsigned(7 downto 0):=(others=>'0');
	signal n100,n100_next    : unsigned(7 downto 0):=(others=>'0');
	
	-- Registers to store number of notes of the 4 denominations to be dispensed.
	signal n2000_count,n2000_count_next :	unsigned(7 downto 0):=(others=>'0');
	signal n1000_count,n1000_count_next :	unsigned(7 downto 0):=(others=>'0');
	signal n500_count,n500_count_next   :	unsigned(7 downto 0):=(others=>'0');
	signal n100_count,n100_count_next   :	unsigned(7 downto 0):=(others=>'0');
	
	
	--Signals to facilitate reading of bytes from input and backend;
	signal i,i_next          : integer range 7 downto 0:=0;
	signal next_data_prev    : std_logic:='0';
	signal data_read,data_read_next         : std_logic_vector(63 downto 0):=(others=>'0');
	                    
	-- Timer and signals for blinking of the leds
	signal timer,timer_next 					:integer range 0 to N-1:=0;
	signal blink_count,blink_count_next 		:integer range 0 to 10:=0; -- Max blinks required are 10.
	signal dispensed_count,dispensed_count_next :integer range 0 to 20:=0; -- For blinking in substates of Dispensing_Cash, in Suff_ATM_Cash, max no. of notes to be dispensed be 20. 
	signal disp_wait_prev,disp_wait_next 		:std_logic:='0';-- Used in dispensing cash state. 
	signal blink_2T,blink_2T_next 				:std_logic:='0';-- Used for inter blink gap of 2T.
	signal l0,l0_next							:std_logic:='0';-- controlling led_out(0).
	signal l1,l1_next							:std_logic:='0';-- controlling led_out(0).
	signal l2,l2_next							:std_logic:='0';-- controlling led_out(0).
	signal l3,l3_next							:std_logic:='0';-- controlling led_out(0).
	signal l4,l4_next							:std_logic:='0';-- controlling led_out(0).
	signal l5,l5_next							:std_logic:='0';-- controlling led_out(0).
	signal l6,l6_next							:std_logic:='0';-- controlling led_out(0).
	signal l7,l7_next							:std_logic:='0';-- controlling led_out(0). 
	
	-- Signals for maintaining and updating the state
	signal state, state_next          : StateType                    := Ready;
	signal substate, substate_next	 : SubStateType                 := Encryption;
	
	-- Signals for encryption 
	signal start_encrypt,start_encrypt_next :std_logic:='0';
	signal ciphertext_out :std_logic_vector(63 downto 0):=(others=>'0');
	signal encryption_over :std_logic;
	signal e_wait_prev,e_wait_next :std_logic:='0';
	
	-- Signals for decryption 
	signal start_decrypt,start_decrypt_next :std_logic:='0';
	signal plaintext_out :std_logic_vector(63 downto 0):=(others=>'0');
	signal decryption_over :std_logic;
	signal d_wait_prev,d_wait_next :std_logic:='0';
	
	-- Signal to store response from backend with channels 10-17
	signal encrypted_response,encrypted_response_next : std_logic_vector(63 downto 0):=(others=>'0');
	signal garbage,garbage_next : std_logic_vector(7 downto 0):=(others=>'0');
signal debounced_next_data_in :std_logic;
signal debounced_reset :std_logic;
signal debounced_start :std_logic;
signal debounced_done  :std_logic;

	--dummy signal
	--signal zeroes :std_logic_vector(31 downto 0) :=(others=>'0');
begin
led_out(0)<=l0;led_out(1)<=l1;led_out(2)<=l2;led_out(3)<=l3;led_out(4)<=l4;led_out(5)<=l5;led_out(6)<=l6;led_out(7)<=l7;

debouncer1: debouncer
              port map (clk => fx2Clk_in,
                        button => next_data_in,
                        button_deb => debounced_next_data_in);
debouncer2: debouncer
              port map (clk => fx2Clk_in,
                        button => reset,
                        button_deb => debounced_reset);
debouncer3: debouncer
              port map (clk => fx2Clk_in,
                        button => start,
                        button_deb => debounced_start);
debouncer4: debouncer
              port map (clk => fx2Clk_in,
                        button => done,
                        button_deb => debounced_done);
encrypt: encrypter
              port map (clk => fx2Clk_in,
                        reset => debounced_reset,
                        plaintext => data_read,
                        start => start_encrypt,
                        ciphertext => ciphertext_out,
                        done => encryption_over);
decrypt: decrypter
              port map (clk => fx2Clk_in,
                        reset => debounced_reset,
                        ciphertext => encrypted_response,
                        start => start_decrypt,
                        plaintext => plaintext_out,
                        done => decryption_over);

	process(fx2Clk_in)
	begin
		if ( rising_edge(fx2Clk_in) ) then
			if ( debounced_reset = '1' ) then
				-- State maintaining signals.
				state<=Ready;
				substate<=Encryption;
				-- Notes in ATM counters.
				n2000<=(others=>'0');
				n1000<=(others=>'0');
				n500<=(others=>'0');
				n100<=(others=>'0');
				-- Notes to be dispensed counters.
				n2000_count<=(others=>'0');
				n1000_count<=(others=>'0');
				n500_count<=(others=>'0');
				n100_count<=(others=>'0');
				-- Get User Input State.
				i<=0;
				next_data_prev<='0';
				data_read<=(others=>'0');
				-- Timer.
				timer<=0;
				blink_count<=0;
				dispensed_count<=0;
				disp_wait_prev<='0';
				blink_2T<='0';
				l0<='0';l1<='0';l2<='0';l3<='0';l4<='0';l5<='0';l6<='0';l7<='0';
				-- Encryption and Decryption.
				e_wait_prev<='0';
				f2hData<=(others=>'0');
				encrypted_response<=(others=>'0');
				start_encrypt<='0';
				garbage<=(others=>'0');
				d_wait_prev<='0';
				start_decrypt<='0';
			else
				state <= state_next;
				substate <= substate_next;
				n2000<=n2000_next;
				n1000<=n1000_next;
				n500<=n500_next;
				n100<=n100_next;
				n2000_count<=n2000_count_next;
				n1000_count<=n1000_count_next;
				n500_count<=n500_count_next;
				n100_count<=n100_count_next;
				i<=i_next;
				next_data_prev<=debounced_next_data_in;
				data_read<=data_read_next;
				timer<=timer_next;
				blink_count<=blink_count_next;
				dispensed_count<=dispensed_count_next;
				disp_wait_prev<=disp_wait_next;
				blink_2T<=blink_2T_next;
				l0<=l0_next;l1<=l1_next;l2<=l2_next;l3<=l3_next;l4<=l4_next;l5<=l5_next;l6<=l6_next;l7<=l7_next;
				e_wait_prev<=e_wait_next;
				f2hData<=f2hData_next;
				encrypted_response<=encrypted_response_next;
				garbage<=garbage_next;
				d_wait_prev<=d_wait_next;
				start_encrypt<=start_encrypt_next;
				start_decrypt<=start_decrypt_next;
			end if;
		end if;
	end process;
	
	-- Next state logic
	process(state, substate, chanAddr, f2hReady,start_encrypt, start_decrypt, data_read, d_wait_prev, h2fValid, h2fData, f2hData, encrypted_response, garbage, n2000, n1000, n500, n100, n2000_count, n1000_count, n500_count, n100_count, timer, blink_count, disp_wait_prev, dispensed_count, blink_2T, l0, l1, l2, l3, l4, l5, l6, l7, debounced_start, i, debounced_next_data_in, sw_in, next_data_prev,	e_wait_prev, encryption_over, decryption_over, ciphertext_out, plaintext_out, debounced_done) 	
	begin
		state_next<=state;
		substate_next<=substate;
		n2000_count_next<=n2000_count;
		n1000_count_next<=n1000_count;
		n500_count_next<=n500_count;
		n100_count_next<=n100_count;		
		n2000_next<=n2000;
		n1000_next<=n1000;
		n500_next<=n500;
		n100_next<=n100;
		i_next<=i;
		data_read_next<=data_read;
		timer_next<=timer;	
		blink_count_next<=blink_count;
		dispensed_count_next<=dispensed_count;
		disp_wait_next<=disp_wait_prev;
		blink_2T_next<=blink_2T;
		l0_next<=l0;l1_next<=l1;l2_next<=l2;l3_next<=l3;l4_next<=l4;l5_next<=l5;l6_next<=l6;l7_next<=l7;
		e_wait_next<=e_wait_prev;
		f2hData_next<=f2hData;
		encrypted_response_next<=encrypted_response;
		garbage_next<=garbage;
		d_wait_next<=d_wait_prev;
		start_encrypt_next<=start_encrypt;
		start_decrypt_next<=start_decrypt;
		case state is
			when Ready =>
				if(chanAddr="0000000") then
					if(f2hReady='1') then
						f2hData_next<="00000000";
					end if;
				end if;
				i_next<=0;e_wait_next<='0';d_wait_next<='0';substate_next<=Encryption;d_wait_next<='0';	
				if(debounced_start='1') then
					timer_next<=0;
					state_next<=Get_User_Input;
				end if;
			when Get_User_Input =>
				if(timer=N-1)then
					timer_next<=0;
				elsif(timer=N-2) then
					timer_next<=timer+1;
					l0_next<='1';
				elsif(timer=M) then
					l0_next<='0';
					timer_next<=timer+1;
				else
					timer_next<=timer+1;
				end if;
				if(debounced_next_data_in='1' and next_data_prev='0') then
					-- read into signal using i;
						data_read_next(0+(i)*8)<=sw_in(0);
						data_read_next(1+(i)*8)<=sw_in(1);
						data_read_next(2+(i)*8)<=sw_in(2);
                  data_read_next(3+(i)*8)<=sw_in(3);
						data_read_next(4+(i)*8)<=sw_in(4);
						data_read_next(5+(i)*8)<=sw_in(5);
						data_read_next(6+(i)*8)<=sw_in(6);
						data_read_next(7+(i)*8)<=sw_in(7);
					if(i/=7) then 
						i_next<=i+1;
						if(i=0) then
							l1_next<='1';l2_next<='0';l3_next<='0';
						elsif(i=1) then
							l1_next<='0';l2_next<='1';l3_next<='0';
						elsif(i=2) then
							l1_next<='1';l2_next<='1';l3_next<='0';
						elsif(i=3) then
							l1_next<='0';l2_next<='0';l3_next<='1';
						elsif(i=4) then
							l1_next<='1';l2_next<='0';l3_next<='1';
						elsif(i=5) then
							l1_next<='0';l2_next<='1';l3_next<='1';
						elsif(i=6) then
							l1_next<='1';l2_next<='1';l3_next<='1';
						end if;
					else
						l0_next<='0';l1_next<='0';l2_next<='0';l3_next<='0';
						timer_next<=0;
						state_next<=Communicating_With_Backend;
					end if;
				end if;
			when Communicating_With_Backend =>
				if(timer=N-1)then
					timer_next<=0;
				elsif(timer=N-2) then
					timer_next<=timer+1;
					l0_next<='1';l1_next<='1';
				elsif(timer=M)then
					timer_next<=timer+1;
					l0_next<='0';l1_next<='0';
				else
					timer_next<=timer+1;
				end if;
				case substate is 
					when Encryption =>
						if(e_wait_prev<='0') then
							start_encrypt_next<='1';
							e_wait_next<='1';
						else
							start_encrypt_next<='0';
							if(encryption_over='1') then
								substate_next<=Writing_To_Backend;
							end if;				
						end if;	
					when Writing_To_Backend =>
						case chanAddr is
							when "0000000"=>
								if(n2000>=unsigned(data_read(39 downto 32)) and n1000>=unsigned(data_read(47 downto 40)) and n500>=unsigned(data_read(55 downto 48)) and n100>= unsigned(data_read(63 downto 56))) then
									if (f2hReady='1') then 
										f2hData_next<="00000001";
									end if;
								else
									if (f2hReady='1') then 
										f2hData_next<="00000010";
									end if;
								end if;
							when "0000001"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(7 downto 0);
								end if;
							when "0000010"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(15 downto 8);
								end if;
							when "0000011"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(23 downto 16);
								end if;
							when "0000100"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(31 downto 24);
								end if;
							when "0000101"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(39 downto 32);
								end if;
							when "0000110"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(47 downto 40);
								end if;
							when "0000111"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(55 downto 48);
								end if;
							when "0001000"=>
								if (f2hReady='1') then 
										f2hData_next<=ciphertext_out(63 downto 56);
										substate_next<=Reading_From_Backend;
								end if;
							when others =>
								if (f2hReady='1') then 
										f2hData_next<="00000000";
								end if;	
						end case;
					when Reading_From_Backend => -- 10 to 17 are read first, then 9 in substate StateChange.
						 case chanAddr is
							when "0001010" =>
								if(h2fValid='1') then
									encrypted_response_next(7 downto 0)<=h2fData;
								end if;
							when "0001011" =>
								if(h2fValid='1') then
									encrypted_response_next(15 downto 8)<=h2fData;
								end if;
							when "0001100" =>
								if(h2fValid='1') then
									encrypted_response_next(23 downto 16)<=h2fData;
								end if;
							when "0001101" =>
								if(h2fValid='1') then
									encrypted_response_next(31 downto 24)<=h2fData;
								end if;
							when "0001110" =>
								if(h2fValid='1') then
									encrypted_response_next(39 downto 32)<=h2fData;
								end if;
							when "0001111" =>
								if(h2fValid='1') then
									encrypted_response_next(47 downto 40)<=h2fData;
								end if;
							when "0010000" =>
								if(h2fValid='1') then
									encrypted_response_next(55 downto 48)<=h2fData;
								end if;
							when "0010001" =>
								if(h2fValid='1') then
									encrypted_response_next(63 downto 56)<=h2fData;
									substate_next<=Decryption;
								end if;
							when others =>
								if (h2fValid='1') then 
									garbage_next<=h2fData;
								end if;	
						end case;
					when Decryption =>
						if(d_wait_prev<='0') then
							start_decrypt_next<='1';
							d_wait_next<='1';
						else
							start_decrypt_next<='0';
							if(decryption_over='1') then
								substate_next<=Response_9;
							end if;				
						end if;
					when Response_9 =>
						if(chanAddr="0001001") then
							if(h2fValid='1') then
								if(h2fData/="00000000") then
									if(h2fData="00000001")then
										state_next<=Dispensing_Cash;
										substate_next<=ATM_Cash_Status;
									elsif(h2fData="00000010") then
										state_next<=Dispensing_Cash;
										substate_next<=Insuff_balance;
									elsif(h2fData="00000011") then
										state_next<=Loading_Cash;
									elsif(h2fData="00000100") then
										state_next<=Ready;
									end if;
									-- Exiting communication with backend state.
									l0_next<='0';l1_next<='0';
									timer_next<=0;
								end if;
							end if;
						end if;
					when others =>
						l0_next<='0';l1_next<='0';
						timer_next<=0;
						state_next<=Ready;	
					end case;
			when Loading_Cash =>
				if(chanAddr="0000000") then
					if(f2hReady='1') then
						f2hData_next<="00000011";
					end if;
				end if;
				if(blink_count<=5) then -- Last blink will not be visible.
					if(timer=N-1)then
						timer_next<=0;
					elsif(timer=N-2) then
						timer_next<=timer+1;
						l0_next<='1';l1_next<='1';l2_next<='1';
						blink_count_next<=blink_count+1;
					elsif(timer=M) then
						timer_next<=timer+1;
						l0_next<='0';l1_next<='0';l2_next<='0';
					else
						timer_next<=timer+1;
					end if;
				else
					l0_next<='0';l1_next<='0';l2_next<='0';
				end if;
				n2000_next<=unsigned(plaintext_out(7 downto 0));
				n1000_next<=unsigned(plaintext_out(15 downto 8));
				n500_next<=unsigned(plaintext_out(23 downto 16));
				n100_next<=unsigned(plaintext_out(31 downto 24));
				if(debounced_done='1' and blink_count=6) then
					state_next<=Ready;
					timer_next<=0;
					blink_count_next<=0;
				end if;
			when Dispensing_Cash =>
				if(chanAddr="0000000") then
					if(f2hReady='1') then
						f2hData_next<="00000011";
					end if;
				end if;
				if(blink_count<=5) then
					if(timer=N-1)then
						timer_next<=0;
					elsif(timer=N-2) then
						timer_next<=timer+1;
						l0_next<='1';l1_next<='1';l2_next<='1';l3_next<='1';
						blink_count_next<=blink_count+1;
					elsif(timer=M) then
						timer_next<=timer+1;
						l0_next<='0';l1_next<='0';l2_next<='0';l3_next<='0';
					else
						timer_next<=timer+1;
					end if;
				else
					l0_next<='0';l1_next<='0';l2_next<='0';l3_next<='0';
				end if;
				case substate is
					when ATM_Cash_Status =>
						-- Byte 1 is 2000 notes, 2 is 1000 notes, 3 is 500 and 4 is 100.
						if(n2000>=unsigned(plaintext_out(7 downto 0)) and n1000>=unsigned(plaintext_out(15 downto 8)) and n500>=unsigned(plaintext_out(23 downto 16)) and n100>= unsigned(plaintext_out(31 downto 24))) then
							substate_next<=Suff_ATM_Cash;
						else
							substate_next<=Insuff_ATM_Cash;
						end if;
					when Suff_ATM_Cash =>
						if(disp_wait_prev='0') then
							n2000_count_next<=unsigned(plaintext_out(7 downto 0));
							n1000_count_next<=unsigned(plaintext_out(15 downto 8));
							n500_count_next<=unsigned(plaintext_out(23 downto 16));
							n100_count_next<=unsigned(plaintext_out(31 downto 24));
							n2000_next<=n2000-unsigned(plaintext_out(7 downto 0));
							n1000_next<=n1000-unsigned(plaintext_out(15 downto 8));
							n500_next<=n500-unsigned(plaintext_out(23 downto 16));
							n100_next<=n100-unsigned(plaintext_out(31 downto 24));
							disp_wait_next<='1';
						else
							if(dispensed_count<n2000_count) then
								if(timer=N-1)then
									timer_next<=0;
								elsif(timer=N-2) then
									timer_next<=timer+1;
									if(blink_2T='1')then
										dispensed_count_next<=dispensed_count+1;
										l4_next<='1';
										blink_2T_next<='0';
									else
										blink_2T_next<='1';
									end if;
								elsif(timer=M) then
									timer_next<=timer+1;
									l4_next<='0';
								else
									timer_next<=timer+1;
								end if;
							elsif(dispensed_count<n2000_count+n1000_count)then
								if(timer=N-1)then
									timer_next<=0;
								elsif(timer=N-2) then
									timer_next<=timer+1;
									if(blink_2T='1')then
										dispensed_count_next<=dispensed_count+1;
										l5_next<='1';
										blink_2T_next<='0';
									else
										blink_2T_next<='1';
									end if;
								elsif(timer=M) then
									l4_next<='0';
									l5_next<='0';
									timer_next<=timer+1;
								else
									timer_next<=timer+1;
								end if;
							elsif(dispensed_count<n2000_count+n1000_count+n500_count)then
								if(timer=N-1)then
									timer_next<=0;
								elsif(timer=N-2) then
									timer_next<=timer+1;
									if(blink_2T='1')then
										dispensed_count_next<=dispensed_count+1;
										l6_next<='1';
										blink_2T_next<='0';
									else
										blink_2T_next<='1';
									end if;
								elsif(timer=M) then
									l5_next<='0';
									l6_next<='0';
									timer_next<=timer+1;
								else
									timer_next<=timer+1;
								end if;
							elsif(dispensed_count<=n2000_count+n1000_count+n500_count+n100_count)then
							--- Here one extra note of 100 will be dispensed, but because of blinking for very less time it will not be detected.
								if(timer=N-1)then
									timer_next<=0;
								elsif(timer=N-2) then
									timer_next<=timer+1;
									if(blink_2T='1')then
										dispensed_count_next<=dispensed_count+1;
										l7_next<='1';
										blink_2T_next<='0';
									else
										blink_2T_next<='1';
									end if;
								elsif(timer=M) then
									l6_next<='0';
									l7_next<='0';
									timer_next<=timer+1;
								else
									timer_next<=timer+1;
								end if;
							else
								l7_next<='0';
							end if;
							if(debounced_done='1' and blink_count=6 and dispensed_count=n2000_count+n1000_count+n500_count+n100_count+1) then
								state_next<=Ready;
								dispensed_count_next<=0;
								blink_count_next<=0;
								timer_next<=0;
							end if;
						end if;
					when Insuff_ATM_Cash =>
						if(dispensed_count<=3) then
							if(timer=N-1)then
								timer_next<=0;
							elsif(timer=N-2) then
								timer_next<=timer+1;
								l4_next<='1';l5_next<='1';l6_next<='1';l7_next<='1';
								dispensed_count_next<=dispensed_count+1;
							elsif(timer=M) then
								timer_next<=timer+1;
								l4_next<='0';l5_next<='0';l6_next<='0';l7_next<='0';
							else
								timer_next<=timer+1;
							end if;
						else
							l4_next<='0';l5_next<='0';l6_next<='0';l7_next<='0';
						end if;
						if(debounced_done='1' and blink_count=6 and dispensed_count=4) then
							state_next<=Ready;
							dispensed_count_next<=0;
							blink_count_next<=0;
							timer_next<=0;
						end if;	
					when Insuff_Balance =>
						if(dispensed_count<=6) then
							if(timer=N-1)then
								timer_next<=0;
							elsif(timer=N-2) then
								timer_next<=timer+1;
								l4_next<='1';l5_next<='1';l6_next<='1';l7_next<='1';
								dispensed_count_next<=dispensed_count+1;
							elsif(timer=M) then
								timer_next<=timer+1;
								l4_next<='0';l5_next<='0';l6_next<='0';l7_next<='0';
							else
								timer_next<=timer+1;
							end if;
						else
							l4_next<='0';l5_next<='0';l6_next<='0';l7_next<='0';
						end if;
						if(debounced_done='1' and blink_count=6 and dispensed_count=7) then
							state_next<=Ready;
							dispensed_count_next<=0;
							blink_count_next<=0;
							timer_next<=0;
						end if;	
					when others=>
						state_next<=Ready;
						blink_count_next<=0;
						timer_next<=0;
				end case;
			end case;
	end process;		
	-- CommFPGA module
	fx2Read_out <= fx2Read;
	fx2OE_out <= fx2Read;
	fx2Addr_out(0) <=  -- So fx2Addr_out(1)='0' selects EP2OUT, fx2Addr_out(1)='1' selects EP6IN
		'0' when fx2Reset = '0'
		else 'Z';
	comm_fpga_fx2 : entity work.comm_fpga_fx2
		port map(
			clk_in         => fx2Clk_in,
			reset_in       => '0',
			reset_out      => fx2Reset,
			
			-- FX2LP interface
			fx2FifoSel_out => fx2Addr_out(1),
			fx2Data_io     => fx2Data_io,
			fx2Read_out    => fx2Read,
			fx2GotData_in  => fx2GotData_in,
			fx2Write_out   => fx2Write_out,
			fx2GotRoom_in  => fx2GotRoom_in,
			fx2PktEnd_out  => fx2PktEnd_out,

			-- DVR interface -> Connects to application module
			chanAddr_out   => chanAddr,
			h2fData_out    => h2fData,
			h2fValid_out   => h2fValid,
			h2fReady_in    => '1',
			f2hData_in     => f2hData,
			f2hValid_in    => '1',
			f2hReady_out   => f2hReady
		);
end architecture;	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
