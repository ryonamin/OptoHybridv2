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

    -- Wishbone PROM slave to control this module
    wb_slv_req_i    : in wb_req_t;
    wb_slv_res_o    : out wb_res_t;

    -- Wishbone PROM master to communicate with vfat2 
    wb_mst_req_o    : out wb_req_t;
    wb_mst_res_i    : in wb_res_t;
   
    --trig : out std_logic_vector(23 downto 0)
    trig : out std_logic_vector(15 downto 0)
  );
end prom;

architecture Behavioral of prom is

begin
 process (ref_clk_i)

    type states is (s_init,s0,s1,s2,s_end);
    variable state : states := s_init;

    -- vfat2_counter =[0:23]
    --variable vfat2_counter : unsigned(4 downto 0) := "10000"; -- vfat 16
    variable vfat2_counter : unsigned(4 downto 0) := "10001"; -- vfat 17
    --variable req_we        : std_logic := '0'; 
    variable req_we        : std_logic; 
    --variable req_data      : std_logic_vector(7 downto 0) := X"00";
    variable req_data      : std_logic_vector(7 downto 0);
    --variable req_register  : std_logic_vector(7 downto 0) := X"08"; -- ChipID<0>
    --variable req_register  : std_logic_vector(7 downto 0) := X"07"; -- IComp 
    --variable req_register  : std_logic_vector(7 downto 0) := X"09"; -- ChipID<1>
    variable req_register  : std_logic_vector(7 downto 0);

    variable counter : integer := 0;

  begin
    if rising_edge(ref_clk_i) then
      --if com_get_started = '1' then
       
      -- do something on request

         case state is
           when s_init => -- waiting for a request
             if (wb_slv_req_i.stb = '1') then
               state := s0;
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
            
             state := s1;
           when s1 => -- sending a command
             --trig(7 downto 0) <= req_data;
             -- Now this becomes a master to vfat chips
             wb_mst_req_o <= (stb => '1', we => req_we, addr => WB_ADDR_I2C & "00000000000" & std_logic_vector(vfat2_counter) & req_register, data => x"000000" & req_data);
             trig(0) <= '0';
             trig(1) <= '0';
             state := s2;
           when s2 => -- waiting the response
              -- Reset the strobe and waiting for responses from vfats
              wb_mst_req_o.stb <= '0';
              trig(0) <= '1';
              if (wb_mst_res_i.ack = '1') then
                trig(1) <= '1';
                trig(15 downto 8) <= wb_mst_res_i.data(7 downto 0);
                --trig(11 downto 8) <= wb_mst_res_i.stat;
                state := s_end;
              end if;                    
           when s_end =>
             trig(0) <= '0';
             trig(1) <= '0';
             -- tell the parent that the request is acknowledged. 
             wb_slv_res_o <= (ack => '1', stat => WB_NO_ERR, data => (others => '0'));
         end case;

--      if--then
--        case state is
--          when s_init =>
--
--            -- initialization. nothing for now...
--
--            trig(15 downto 12) <= X"0"; --monitor state
--            state := s1;
--            
--          when s1 => -- Request test
--            trig(12) <= '1';
--            
--            wb_mst_req_o <= (stb => '1', we => req_we, addr => WB_ADDR_I2C & "00000000000" & std_logic_vector(vfat2_counter) & req_register, data => x"000000" & req_data);
--
--            state := s2; 
--            counter := 0;
--          when s2 =>  -- Read response test
--            trig(13) <= '1'; --monitor state
--            counter := counter + 1;
--            -- Reset the strobe
--            wb_mst_req_o.stb <= '0';
--            -- On acknowledgment
--            trig(14) <= wb_mst_res_i.ack; --monitor state
--            if (wb_mst_res_i.ack = '1') then
--              trig(7 downto 0) <= wb_mst_res_i.data(7 downto 0);
--              trig(11 downto 8) <= wb_mst_res_i.stat;
--              -- Increment the counter
--              vfat2_counter := vfat2_counter + 1;
--              -- Change state
--              state := s_end;
--            end if;                    
--          when s_end =>
--            trig(15) <= '1'; --monitor state
--            com_get_started_end <= '1';
--        end case;
--      else
--        state := s_init;
--        com_get_started_end <= '0';
--      end if;
--       
    end if;
  end process;

end Behavioral;

