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

    reset           : out std_logic;
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
  signal com_send_cmd_end1 : std_logic := '0';
  signal com_send_cmd2 : std_logic;
  signal com_send_cmd_end2 : std_logic := '0';
  signal com_send_cmdn : std_logic_vector(15 downto 0);
  signal com_send_cmd_endn : unsigned(16 downto 0) := (others => '0');

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
 --trig(22 downto 0)  <= com_addr_data1(22 downto 0);
 trig(38 downto 23) <= flash_data_io;
 trig(42 downto 39) <= flash_ctl_o;
 --trig(58 downto 43) <= com_out_data;

 process (ref_clk_i)

    type main_states is (s_init,s0,s1,s2,s_end);
    variable main_state : main_states := s_init;

    type cmd_read_states is (s_init,s_end);
    type cmd_block_read_states is (s_init,s_end);
    type cmd_write_states is (s_init,s1,s2,s_end);
    variable cmd_write_state : cmd_write_states := s_init;
    variable cmd_read_state : cmd_read_states := s_init;
    variable cmd_block_read_state : cmd_block_read_states := s_init;
    variable req_we        : std_logic; 
    variable req_data      : std_logic_vector(15 downto 0);
    variable req_address  : std_logic_vector(23 downto 0);
    variable block_read_counter : unsigned(16 downto 0) := (others => '0');

    variable counter : integer := 0;

  begin
    if rising_edge(ref_clk_i) then
       
         case main_state is
           when s_init => -- waiting for a request
 trig(50 downto 43) <= X"01";
             if (wb_slv_req_i.stb = '1') then
               main_state := s0;
               reset <= '0';
             else
               -- reset
               wb_slv_res_o <= (ack => '0', stat => (others => '0'), data => (others => '0'));
               reset <= '1';
             end if;
           when s0 =>
 trig(50 downto 43) <= X"02";
             req_address := wb_slv_req_i.addr(23 downto 0); 
             req_data := wb_slv_req_i.data(15 downto 0); 
             req_we := wb_slv_req_i.we;
            
             main_state := s1;
           when s1 => -- sending a command
 trig(50 downto 43) <= X"03";
             -- initialization
             com_send_cmd1 <= '0';
             com_send_cmd2 <= '0';
             com_send_cmdn <= X"0000";
             block_read_counter := (others => '0');
             cmd_write_state := s_init;
             cmd_read_state := s_init;
             main_state := s2;
           when s2 => -- waiting the response
 trig(50 downto 43) <= X"04";
 trig(54 downto 51) <= req_address(23 downto 20);
             if req_address(23 downto 20) < X"8" then -- one-by-one read/write command
               if (req_we='1') then
                 -- Write data
                 case cmd_write_state is
                    when s_init =>
                      if com_send_cmd_end1 = '1' then -- go next step
                        cmd_write_state := s1;
                        com_send_cmd1 <= '0'; --must be reset beforehand
                      else -- send run signal
                        com_send_cmd1 <= '1';
                        com_addr_data1 <= req_address(23 downto 16) & X"0000"; -- Check lock/unlock status
                        com_addr_data2 <= req_address(23 downto 16) & X"0002"; -- Check lock/unlock status
                        com_cmd_data1  <= CMD_READ_SIGNATURE;
                      end if;
                    when s1 =>
                      if com_send_cmd_end2 = '1' then -- go next step
                        cmd_write_state := s2;
                        com_send_cmd2 <= '0'; --must be reset beforehand
                      else -- send run signal
                        com_send_cmd2 <= '1';
                        com_addr_data1 <= req_address; 
                        com_addr_data2 <= req_address; 
                        com_cmd_data1  <= CMD_BLOCK_UNLOCK1;
                        com_cmd_data2  <= CMD_BLOCK_UNLOCK2;
                      end if;
                    when s2 =>
                       if com_send_cmd_end2 = '1' then -- go next step
                         cmd_write_state := s_end;
                         wb_slv_res_o <= (ack => '1', stat => WB_NO_ERR, data => (others => '0'));
                         com_send_cmd2 <= '0'; --must be reset beforehand
                       else -- send run signal
                         com_send_cmd2 <= '1';
                         com_addr_data1 <= req_address;
                         com_addr_data2 <= req_address; -- Check status register
                         com_cmd_data1  <= CMD_WRITE_ARRAY;
                         com_cmd_data2  <= req_data;
                       end if;
                    when s_end =>
                      main_state := s_end;
                 end case;
               else
                 -- Read data 
                 case cmd_read_state is
                    when s_init =>
                      if com_send_cmd_end1 = '1' then -- go next step
                        cmd_read_state := s_end;
                        wb_slv_res_o <= (ack => '1', stat => WB_NO_ERR, data => x"0000" & com_out_data);
                        com_send_cmd1 <= '0'; --must be reset beforehand
                   --     com_send_cmd2 <= '0'; --must be reset beforehand
                      else -- send run signal
                        com_send_cmd1 <= '1';
                        com_addr_data1 <= req_address(23 downto 16) & X"0000"; 
                        com_addr_data2 <= req_address; -- Check status register
                        com_cmd_data1  <= CMD_READ_ARRAY;
                      end if;
                    when s_end =>
                      main_state := s_end;
                 end case;
               end if;
             elsif req_address(23 downto 20) = X"8" then -- Block read/write command
               if (req_we='0') then --Block read 
                 case cmd_block_read_state is
                    when s_init =>
                      if com_send_cmd_endn = block_read_counter + 1 then 
                        block_read_counter := block_read_counter + 1;
                        wb_slv_res_o <= (ack => '1', stat => WB_NO_ERR, data => x"0000" & com_out_data);
                        if com_send_cmd_endn > unsigned(com_send_cmdn) then
                          com_send_cmdn <= X"0000"; -- stop sub-process 
                          cmd_block_read_state := s_end;
                        end if;
                      else
                        if req_address(11 downto 4) = X"7F" then
                          com_send_cmdn <= X"3FFF"; -- For reading 1 block (0 to 3FFF)
                        else
                          com_send_cmdn <= X"FFFF"; -- For reading 1 block (0 to FFFF)
                        end if;
                        com_addr_data1 <= req_address(11 downto 0) & X"000"; 
                        com_addr_data2 <= req_address(11 downto 0) & X"000"; 
                        com_cmd_data1  <= CMD_READ_ARRAY;
                      end if;
                    when s_end =>
                      main_state := s_end;
                 end case;
               end if;
             elsif req_address(23 downto 20) = X"9" then -- Block read/write command
                wb_slv_res_o <= (ack => '1', stat => WB_NO_ERR, data => x"1234abcd");
             end if;
           when s_end =>
             main_state := s_init; -- must be stand-by for next command.
             -- tell the parent that the request is acknowledged. 
         end case;
    end if;
  end process;

 -- sub-routine
 process (ref_clk_i)
   type states is (s_init,s1,s2,s3,s4,s5,s6,s7,s8,s_end);
   variable state : states := s_init;

   --type states2 is (s_init,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s_end);
   type states2 is (s_init,s1,s2,s3,s4,s5,s6,s_end);
   variable state2 : states2 := s_init;

   type states3 is (s_init,s1,s2,s3,s4,s5,s6,s7,s8,s9);
   variable state3 : states3 := s_init;

   variable counter : integer := 0;
   variable com_send_cmdn_counter : unsigned(15 downto 0);
   variable com_addr_data : unsigned(23 downto 0);
   variable block_read_counter : unsigned(15 downto 0);
 begin
    if rising_edge(ref_clk_i) then
      if com_send_cmd1 = '1' then -- 1-write+1-read command
        case state is
          -- Write operation (376F)
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
      elsif com_send_cmd2 = '1' then -- 2-write command
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
            state2 := s_end;
--            state2 := s7;
--          when s7 => -- Write data
--            flash_ctl_o <= X"F";
--            state2 := s8;
--          -- Read operation (375F)
--          when s8 => -- Set address and prepare to latch
--            flash_address_o <= com_addr_data3(22 downto 0);
--            flash_data_io <= (others => 'Z'); 
--            flash_ctl_o <= "0011"; --X"3"
--            state2 := s9;
--          when s9 => -- Latch address
--            flash_ctl_o <= "0111"; --X"7"
--            state2 := s10;
--          when s10 => 
--            flash_ctl_o <= "0101"; --X"5"
--            counter := 0;
--            state2 := s11;
--          when s11 => -- wait for 7 clock cycles from s8
--            if counter >= 3 then
--            flash_ctl_o <= "0101"; --X"5"
--              state2 := s12;
--            else
--            flash_ctl_o <= "0101"; --X"5"
--              counter := counter + 1;
--            end if;
--          when s12 => -- Read data
--            com_out_data <= flash_data_io;
--            state2 := s_end;
          when s_end => -- Read data
            flash_ctl_o <= X"F"; 
            com_send_cmd_end2 <= '1';
        end case;
      elsif com_send_cmdn /= X"0000" then -- 1-write+n-read command
        case state3 is
          -- Write operation (376F)
          when s_init => -- Set address and prepare to latch
            com_send_cmdn_counter := unsigned(com_send_cmdn);
            block_read_counter := X"0000";
            com_addr_data := unsigned(com_addr_data2);
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= "0011"; --X"3"
            counter := 0;
            state3 := s1;
          when s1 => -- Latch address
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            if counter >= 2 then
              flash_ctl_o <= "0111"; --X"7"
              state3 := s2;
            else
              flash_ctl_o <= "0011"; --X"3"
              counter := counter + 1;
            end if;
          when s2 => -- Set data and prepare to write
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= "0110"; --X"6" 
            state3 := s3;
          when s3 => -- Write data
            flash_address_o <= com_addr_data1(22 downto 0);
            flash_data_io   <= com_cmd_data1;
            flash_ctl_o <= X"F";
            state3 := s4;
          -- Read operation (375F)
          when s4 => -- Set address and prepare to latch
            flash_address_o <= std_logic_vector(com_addr_data(22 downto 0));
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0011"; --X"3"
            state3 := s5;
          when s5 => -- Latch address
            flash_address_o <= std_logic_vector(com_addr_data(22 downto 0));
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0111"; --X"7"
            state3 := s6;
          when s6 => 
            flash_address_o <= std_logic_vector(com_addr_data(22 downto 0));
            flash_data_io <= (others => 'Z'); 
            flash_ctl_o <= "0101"; --X"5"
            counter := 0;
            state3 := s7;
          when s7 => -- Need more than 7 clock cycles from s4
            flash_address_o <= std_logic_vector(com_addr_data(22 downto 0));
            flash_data_io <= (others => 'Z'); 
            if counter >= 3 then
              flash_ctl_o <= "0101"; --X"5"
              state3 := s8;
            else
              flash_ctl_o <= "0101"; --X"5"
              counter := counter + 1;
            end if;
          when s8 => -- Read data
            flash_ctl_o <= X"F"; 
            flash_data_io <= (others => 'Z'); 
            com_out_data <= flash_data_io;
            state3 := s9;
          when s9 => -- continue to read if necessary
            com_send_cmd_endn <= com_send_cmd_endn + 1; 
            com_out_data_tmp <= flash_data_io;
            flash_data_io <= (others => 'Z'); 
            if ( unsigned(com_send_cmdn) - com_send_cmd_endn ) > 0 then
              state3 := s4;
              com_addr_data := com_addr_data + 1;
            end if;
        end case;
      else
        state := s_init;
        state2 := s_init;
        state3 := s_init;
        com_send_cmd_end1 <= '0';
        com_send_cmd_end2 <= '0';
        com_send_cmd_endn <= (others => '0');
      end if;
    end if;
  end process;

end Behavioral;

