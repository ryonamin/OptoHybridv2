library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
library work;
use work.types_pkg.all;
use work.wb_pkg.all;

entity test_controller is
  port(
    --reset : out std_logic;
    ref_clk_i       : in std_logic;
    clk_50MHz_i       : in std_logic;

    -- Wishbone dummy master to communicate with PROM 
    --wb_mst_req_o    : out wb_req_t;
    wb_mst_req_o    : buffer wb_req_t;
    wb_mst_res_i    : in wb_res_t;
   
    trig : out std_logic_vector(7 downto 0)
  );
end test_controller;

architecture Behavioral of test_controller is
  signal start_flag : std_logic;
begin
  trig(0) <= start_flag;
--  process(ref_clk_i)
--    -- Run every 100_000_000 clock cycles. This is just for test runs.
--    variable counter : integer := 0;
--  begin
--    if falling_edge(ref_clk_i) then
--      if counter >= 100_000_000 then -- Reset 
--        start_flag <= '0';
--        reset <= '1';
--        counter := 0; 
--      else --running
--        counter := counter + 1;
--        reset <= '0';
--        start_flag <= '1';
--      end if;
--    end if;
--  end process;

  process(ref_clk_i)
    variable cmd : std_logic_vector(3 downto 0) := X"0"; --command to PROM (write/read etc.)
    variable req_addr : std_logic_vector(23 downto 0) := X"000000"; -- prom address to read/write 
    variable req_data : std_logic_vector(15 downto 0) := X"0000"; --dummy signal (VFAT2 chipID<0>)
    type states is (s_init,s0,s1,s2,s_end);
    variable state : states := s_init;
  begin
    if rising_edge(ref_clk_i) then
      if start_flag = '1' then
        case state is
          when s_init => 
            wb_mst_req_o <= (stb => '0', we => '0', addr => (others => '0'), data => (others => '0'));
            state := s0;
          when s0 => 
            -- request format
            -- stb  : 1 bit
            -- we   : 1 bit
            -- addr : 32 bit
            -- data : 14 bit --> 20bit?
           
--            req_addr := X"780000"; 
--            req_data := X"24"; 
--            wb_mst_req_o <= (stb => '1', we => '1', addr => WB_ADDR_PROM & req_addr, data => x"0000" & req_data);
            state := s1;
          when s1 => 
--            wb_mst_req_o.stb <= '0';
--            if wb_mst_res_i.ack = '1' then
              state := s2;
--            end if;
          when s2 => 
            req_addr := X"780000"; 
            req_data := X"0000"; 
            wb_mst_req_o <= (stb => '1', we => '0', addr => WB_ADDR_PROM & req_addr, data => x"0000" & req_data);
            state := s_end;
          when s_end => 
            wb_mst_req_o.stb <= '0';
        end case;
      else -- reset
        state := s_init;
      end if;
    end if;
  end process;
    
end Behavioral;

