----------------------------------------------------------------------------------
-- Company:        IIHE - ULB
-- Engineer:       Ryo Yonamine (ryo.yonamine@cern.ch)
-- 
-- Create Date:    04/29/2016 
-- Design Name:    OptoHybrid v2
-- Module Name:    prom - Behavioral 
-- Project Name:   OptoHybrid v2
-- Target Devices: xc6vlx130t-1ff1156
-- Tool versions:  ISE  P.20131013
-- Description: 
--
-- Store VFAT2 parameters into PROM (Platform Flash XL) and also
-- send the stored parameters to VFAT2 when requested. 
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

library work;
use work.types_pkg.all;
use work.wb_pkg.all;

entity prom is
  port(

    ref_clk_i       : in std_logic;
    clk_50MHz_i     : in std_logic;

    -- Wishbone PROM slave to control this module
    wb_slv_req_i    : in wb_req_t;
    wb_slv_res_o    : out wb_res_t;

    -- Wishbone PROM master to communicate with vfat2 
    wb_mst_req_o    : out wb_req_t;
    wb_mst_res_i    : in wb_res_t;
   
    -- PROM Address Inputs
    flash_address_o : buffer std_logic_vector(22 downto 0);

    -- PROM Data Inputs/Outputs
    flash_data_io   : inout  std_logic_vector(15 downto 0);

    -- PROM Data/Address Bus control bits
    ---- flash_ctl_o(3) : flash_chip_enable_b_o
    ---- flash_ctl_o(2) : flash_latch_enable_b_o
    ---- flash_ctl_o(1) : flash_out_enable_b_o
    ---- flash_ctl_o(0) : flash_write_enable_b_o
    flash_ctl_o  : buffer  std_logic_vector(3 downto 0);
    --flash_chip_enable_b_o  : buffer  std_logic;
    --flash_latch_enable_b_o : buffer  std_logic;
    --flash_out_enable_b_o   : buffer  std_logic;
    --flash_write_enable_b_o : buffer  std_logic;

    trig : out std_logic_vector(58 downto 0)
  );
end prom;

architecture Behavioral of prom is

  -- Signals for comunicating another process
  signal com_send_cmd1 : std_logic;
  signal com_send_cmd_end1 : std_logic;
  signal com_send_cmd2 : std_logic;
  signal com_send_cmd_end2 : std_logic;

  signal com_addr_data1 : std_logic_vector(23 downto 0);
  signal com_cmd_data1 : std_logic_vector(15 downto 0);
  signal com_out_data : std_logic_vector(15 downto 0);
  signal com_out_data_tmp : std_logic_vector(15 downto 0);
  signal com_addr_data2 : std_logic_vector(23 downto 0);
  signal com_cmd_data2 : std_logic_vector(15 downto 0);
  signal com_addr_data3 : std_logic_vector(23 downto 0);

  -- Commands
  constant CMD_CLEAR_REGISTER : std_logic_vector(15 downto 0) := X"0050";
  constant CMD_READ_SIGNATURE : std_logic_vector(15 downto 0) := X"0090";
  constant CMD_READ_STATUS    : std_logic_vector(15 downto 0) := X"0070";
  constant CMD_BLOCK_UNLOCK1  : std_logic_vector(15 downto 0) := X"0060";
  constant CMD_BLOCK_UNLOCK2  : std_logic_vector(15 downto 0) := X"00D0";
  constant CMD_READ_ARRAY     : std_logic_vector(15 downto 0) := X"00FF";
  constant CMD_WRITE_ARRAY    : std_logic_vector(15 downto 0) := X"0040";

begin

 trig(22 downto 0)  <= flash_address_o;
 trig(38 downto 23) <= flash_data_io;
 trig(42 downto 39) <= flash_ctl_o;
 trig(58 downto 43) <= com_out_data;

 process (ref_clk_i)

    type main_states is (s_init,s0,s1,s2,s_end);
    variable main_state : main_states := s_init;

    --type cmd_states is (s_init,s1,s2_1,s2,s3,s4_1,s4_2,s4,s5,s6,s7,s_end);
    --type cmd_states is (s_init,s1,s2,s3,s6,s7,s8,s9,s_end);
    type cmd_states is (s_init,s1,s2,s3,s6,s_end);
    variable cmd_state : cmd_states := s_init;
    -- vfat2_counter =[0:23]
    --variable vfat2_counter : unsigned(4 downto 0) := "10000"; -- vfat 16
    variable vfat2_counter : unsigned(4 downto 0) := "10001"; -- vfat 17
    variable req_we        : std_logic; 
    variable req_data      : std_logic_vector(7 downto 0);
    --variable req_register  : std_logic_vector(7 downto 0) := X"08"; -- ChipID<0>
    --variable req_register  : std_logic_vector(7 downto 0) := X"07"; -- IComp 
    --variable req_register  : std_logic_vector(7 downto 0) := X"09"; -- ChipID<1>
    variable req_register  : std_logic_vector(7 downto 0);

    variable counter : integer := 0;

  begin
    if rising_edge(ref_clk_i) then
       
      -- do something on request

         case main_state is
           when s_init => -- waiting for a request
             if (wb_slv_req_i.stb = '1') then
               main_state := s0;
             else
               -- reset
               wb_slv_res_o <= (ack => '0', stat => (others => '0'), data => (others => '0'));
             end if;
           when s0 =>
             -- recieve data from master (test_controller)
             --req_register := wb_slv_req_i.addr(7 downto 0);
             req_register := wb_slv_req_i.data(7 downto 0); -- cmd for vfat must be specified to address
             req_data := X"00"; --dummy data to communicate with vfat
             req_we := wb_slv_req_i.we;
            
             main_state := s1;
           when s1 => -- sending a command
             -- Now this becomes a master to vfat chips
             --wb_mst_req_o <= (stb => '1', we => req_we, addr => WB_ADDR_I2C & "00000000000" & std_logic_vector(vfat2_counter) & req_register, data => x"000000" & req_data);
             -- initialization
             com_send_cmd1 <= '0';
             com_send_cmd2 <= '0';
             cmd_state := s_init;
             main_state := s2;
           when s2 => -- waiting the response
              -- Reset the strobe and waiting for responses from vfats
              --wb_mst_req_o.stb <= '0';
              --if (wb_mst_res_i.ack = '1') then
                --trig(15 downto 8) <= wb_mst_res_i.data(7 downto 0);
                case cmd_state is
                   when s_init =>
                     if com_send_cmd_end1 = '1' then -- go next step
                       cmd_state := s1;
                       com_send_cmd1 <= '0'; --must be reset beforehand
                       com_send_cmd2 <= '0'; --must be reset beforehand
                     else -- send run signal
                       com_send_cmd1 <= '1';
                       com_addr_data1 <= X"000000"; -- Check status register
                       com_addr_data2 <= X"000000"; -- Check status register
                       com_cmd_data1  <= CMD_READ_ARRAY;
                     end if;
                   when s1 => -- 1st command 
                     if com_send_cmd_end1 = '1' then -- go next step
                       cmd_state := s2;
                       com_send_cmd1 <= '0'; --must be reset beforehand
                       com_send_cmd2 <= '0'; --must be reset beforehand
                     else -- send run signal
                       com_send_cmd1 <= '1';
                       com_addr_data1 <= X"7FC000"; -- Check status register
                       com_addr_data2 <= X"7FC000"; -- Check status register
                       com_cmd_data1  <= CMD_READ_ARRAY;
                     end if;
                   when s2 => -- 2nd command 
                     if com_send_cmd_end1 = '1' then -- go next step
                       cmd_state := s3;
                       com_send_cmd1 <= '0'; --must be reset beforehand
                       com_send_cmd2 <= '0'; --must be reset beforehand
                     else -- send run signal
                       com_send_cmd1 <= '1';
                       com_addr_data1 <= X"780000"; -- Check status register
                       com_addr_data2 <= X"780000"; -- Check status register
                       com_cmd_data1  <= CMD_READ_ARRAY;
                     end if;
                   when s3 => -- 3rd command
                     if com_send_cmd_end1 = '1' then -- go next step
                       cmd_state := s6;
                       com_send_cmd1 <= '0'; --must be reset beforehand
                       com_send_cmd2 <= '0'; --must be reset beforehand
                     else -- send run signal
                       com_send_cmd1 <= '1';
                       com_addr_data1 <= X"700000"; -- Check status register
                       com_addr_data2 <= X"700000"; -- Check status register
                       com_cmd_data1  <= CMD_READ_ARRAY;
                     end if;
--                   when s4_1 => -- 4th command
--                      cmd_state := s4_2;
--                   when s4_2 => -- 4th command
--                       if com_send_cmd_end1 = '1' then -- go next step
--                         if com_out_data = X"0080" then -- succeed in clear register
--                           cmd_state := s5;
--                           com_send_cmd1 <= '0'; --must be reset beforehand
--                         else -- Failed to clear register. Need to clear register again
--                           cmd_state := s4_1;
--                           com_send_cmd1 <= '0'; --must be reset beforehand
--                         end if;
--                       else -- send run signal
--                         com_send_cmd1 <= '1';
--                         com_addr_data1 <= X"000000"; -- Check status register
--                         com_addr_data2 <= X"000000"; -- Check status register
--                         com_cmd_data1  <= CMD_READ_STATUS;
--                       end if;
--                   when s4 => -- 4th command
--                     if com_send_cmd_end2 = '1' then -- go next step
--                       cmd_state := s4_2;
--                       com_send_cmd2 <= '0'; --must be reset beforehand
--                     else -- send run signal
--                       com_send_cmd2 <= '1';
--                       com_addr_data1 <= X"000000";
--                       com_addr_data2 <= X"000000"; -- Check status register
--                       com_cmd_data1  <= CMD_WRITE_ARRAY;
--                       --com_cmd_data2  <= X"0101";
--                       com_cmd_data2  <= X"00" & wb_mst_res_i.data(7 downto 0); --must be 16 bits
--                     end if;
--                   when s5 => -- 5th command
--                     if com_send_cmd_end1 = '1' then -- go next step
--                       cmd_state := s6;
--                       com_send_cmd1 <= '0'; --must be reset beforehand
--                     else -- send run signal
--                       com_send_cmd1 <= '1';
--                       com_addr_data1 <= X"000000"; -- Check status register
--                       com_addr_data2 <= X"000000"; -- Check status register
--                       com_cmd_data1  <= CMD_READ_ARRAY;
--                     end if;
                   when s6 => -- 6th command
--                     if com_send_cmd_end1 = '1' then -- go next step
--                       cmd_state := s7;
--                       com_send_cmd1 <= '0'; --must be reset beforehand
--                     else -- send run signal
--                       com_send_cmd1 <= '1';
--                       com_addr_data1 <= X"000000"; -- Check lock/unlock status
--                       com_addr_data2 <= X"000002"; -- Check lock/unlock status
--                       com_cmd_data1  <= CMD_READ_SIGNATURE;
--                     end if;
--                   when s7 =>
--                     if com_send_cmd_end2 = '1' then -- go next step
--                       cmd_state := s8;
--                       com_send_cmd2 <= '0'; --must be reset beforehand
--                     else -- send run signal
--                       com_send_cmd2 <= '1';
--                       com_addr_data1 <= X"000000"; 
--                       com_addr_data2 <= X"000000"; 
--                       com_cmd_data1  <= CMD_BLOCK_UNLOCK1;
--                       com_cmd_data2  <= CMD_BLOCK_UNLOCK2;
--                     end if;
--                   when s8 =>
--                      if com_send_cmd_end2 = '1' then -- go next step
--                        cmd_state := s9;
--                        com_send_cmd2 <= '0'; --must be reset beforehand
--                      else -- send run signal
--                        com_send_cmd2 <= '1';
--                        com_addr_data1 <= X"000000";
--                        com_addr_data2 <= X"000000"; -- Check status register
--                        com_cmd_data1  <= CMD_WRITE_ARRAY;
--                        com_cmd_data2  <= X"0101";
--                      end if;
--                   when s9 =>
--                     if com_send_cmd_end1 = '1' then -- go next step
                       cmd_state := s_end;
--                       com_send_cmd1 <= '0'; --must be reset beforehand
--                       com_send_cmd2 <= '0'; --must be reset beforehand
--                     else -- send run signal
--                       com_send_cmd1 <= '1';
--                       com_addr_data1 <= X"000000"; -- Check status register
--                       com_addr_data2 <= X"000000"; -- Check status register
--                       com_cmd_data1  <= CMD_READ_ARRAY;
--                     end if;
                   when s_end =>
                     main_state := s_end;
                end case;
              --end if;                    
           when s_end =>
main_state := s_init; -- must be stand-by for next command.
             -- tell the parent that the request is acknowledged. 
             wb_slv_res_o <= (ack => '1', stat => WB_NO_ERR, data => (others => '0'));
         end case;
    end if;
  end process;

 -- sub-routine
 process (ref_clk_i)
 --process (clk_50MHz_i)
   type states is (s_init,s1,s2,s3,s4,s5,s6,s7,s8,s_end);
   variable state : states := s_init;

   type states2 is (s_init,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s_end);
   variable state2 : states2 := s_init;

   variable counter : integer := 0;
 begin
    --if falling_edge(ref_clk_i) then
    if rising_edge(ref_clk_i) then
      if com_send_cmd1 = '1' then
        case state is
          -- Writej operation (376F)
          when s_init => -- Set address and prepare to latch
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= "0011"; --X"3"
            counter := 0;
            state := s1;
          when s1 => -- Latch address
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            if counter >= 2 then
              flash_ctl_o <= "0111"; --X"7"
              state := s2;
            else
              flash_ctl_o <= "0011"; --X"3"
              counter := counter + 1;
            end if;
          when s2 => -- Set data and prepare to write
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= "0110"; --X"6" 
            state := s3;
          when s3 => -- Write data
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= X"F";
            state := s4;
          -- Read operation (375F)
          when s4 => -- Set address and prepare to latch
            flash_address_o <= com_addr_data2(22 downto 0);
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0011"; --X"3"
            state := s5;
          when s5 => -- Latch address
            flash_address_o <= com_addr_data2(22 downto 0);
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0111"; --X"7"
            state := s6;
          when s6 => 
            flash_address_o <= com_addr_data2(22 downto 0);
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0101"; --X"5"
            counter := 0;
            state := s7;
          when s7 => -- Need more than 7 clock cycles from s4
            flash_address_o <= com_addr_data2(22 downto 0);
            flash_data_io <= (others => 'Z'); 
--            if counter >= 2 then
            if counter >= 3 then
              flash_ctl_o <= "0101"; --X"5"
              state := s8;
            else
              flash_ctl_o <= "0101"; --X"5"
              counter := counter + 1;
            end if;
          when s8 => -- Read data
            flash_ctl_o <= X"F"; 
            flash_data_io <= (others => 'Z'); 
            com_out_data <= flash_data_io;
--            flash_address_o <= com_addr_data2(22 downto 0);
--            flash_data_io <= (others => 'Z'); 
            state := s_end;
          when s_end => -- Read data
            com_send_cmd_end1 <= '1';
            com_out_data_tmp <= flash_data_io;
--            flash_ctl_o <= X"F"; 
--            flash_data_io <= (others => 'Z'); 
            flash_data_io <= (others => 'Z'); 
        end case;

      -- Command that requires 2 wirte operation and 1 read operation
      elsif com_send_cmd2 = '1' then
        case state2 is
          -- Fisrt write operation (376F)
          when s_init => -- Set address and prepare to latch
            flash_address_o   <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= "0011"; --X"3"
            state2 := s1;
          when s1 => -- Latch address
            flash_ctl_o <= "0111"; --X"7"
            state2 := s2;
          when s2 => -- Set data and prepare to write
            flash_ctl_o <= "0110"; --X"6" 
            state2 := s3;
          when s3 => -- Write data
            flash_ctl_o <= X"F";
            state2 := s4;
          -- Second write operation (376F)
          when s4 => -- Set address and prepare to latch
            flash_address_o   <= com_addr_data2(22 downto 0);
            flash_data_io   <= com_cmd_data2;
            flash_ctl_o <= "0011"; --X"3"
            state2 := s5;
          when s5 => -- Latch address
            flash_ctl_o <= "0111"; --X"7"
            state2 := s6;
          when s6 => -- Set data and prepare to write
            flash_ctl_o <= "0110"; --X"6" 
            state2 := s7;
          when s7 => -- Write data
            flash_ctl_o <= X"F";
            state2 := s8;
          -- Read operation (375F)
          when s8 => -- Set address and prepare to latch
            flash_address_o <= com_addr_data3(22 downto 0);
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0011"; --X"3"
            state2 := s9;
          when s9 => -- Latch address
            flash_ctl_o <= "0111"; --X"7"
            state2 := s10;
          when s10 => 
            flash_ctl_o <= "0101"; --X"5"
            counter := 0;
            state2 := s11;
          when s11 => -- wait for 7 clock cycles from s8
            if counter >= 3 then
            flash_ctl_o <= "0101"; --X"5"
              state2 := s12;
            else
            flash_ctl_o <= "0101"; --X"5"
              counter := counter + 1;
            end if;
          when s12 => -- Read data
            com_out_data <= flash_data_io;
            state2 := s_end;
          when s_end => -- Read data
            flash_ctl_o <= X"F"; 
            com_send_cmd_end2 <= '1';
        end case;
      else
        state := s_init;
        state2 := s_init;
        com_send_cmd_end1 <= '0';
        com_send_cmd_end2 <= '0';
      end if;
    end if;
  end process;

end Behavioral;

