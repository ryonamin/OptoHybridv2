library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
library work;
use work.types_pkg.all;
use work.wb_pkg.all;

entity test1 is
  port(
    --clk_50MHz_i     : in std_logic;
    ref_clk_i     : in std_logic;
    wb_mst_req_o    : out wb_req_t;
    wb_mst_res_i    : in wb_res_t;
    trig            : out std_logic_vector(23 downto 0)
  );
end test1;

architecture Behavioral of test1 is

  -- Signals for comunicating another process
  signal com_get_started : std_logic := '0';
  signal com_get_started_end : std_logic;

begin


  trig(23) <= com_get_started;  

  -- Main process for periodic running the main process
  process(ref_clk_i)
    -- Run every 100_000_000 clock cycles. This is just for test runs.
    variable counter : integer := 0;
  begin
    if rising_edge(ref_clk_i) then
      if counter >= 100_000_000 then -- Reset 
        if com_get_started_end = '1' then
          com_get_started <= '0';
        end if;
        -- wait for 1 clk cycle
        if com_get_started = '0' then
          counter := 0; 
        end if;
      else --running
        counter := counter + 1;
        com_get_started <= '1';
      end if;
    end if;
  end process;
        
  -- main stream
  process (ref_clk_i)
    type states is (s_init,s1,s2,s_end);
    variable state : states := s_init;
    -- vfat2_counter =[0:23]
    variable vfat2_counter : unsigned(4 downto 0) := "10000";
    variable req_we        : std_logic := '0'; 
    variable req_data      : std_logic_vector(7 downto 0) := X"00";
    --variable req_register  : std_logic_vector(7 downto 0) := X"08"; -- ChipID<0>
    --variable req_register  : std_logic_vector(7 downto 0) := X"07"; -- IComp 
    variable req_register  : std_logic_vector(7 downto 0) := X"09"; -- ChipID<1>
    variable counter : integer := 0;
  begin
    if falling_edge(ref_clk_i) then
      if com_get_started = '1' then
        case state is
          when s_init =>

            -- initialization. nothing for now...

            trig(15 downto 12) <= X"0"; --monitor state
            state := s1;
            
          when s1 => -- Request test
            trig(12) <= '1';
            
            wb_mst_req_o <= (stb => '1', we => req_we, addr => WB_ADDR_I2C & "00000000000" & std_logic_vector(vfat2_counter) & req_register, data => x"000000" & req_data);

            state := s2; 
            counter := 0;
          when s2 =>  -- Read response test
            trig(13) <= '1'; --monitor state
            counter := counter + 1;
            -- Reset the strobe
            wb_mst_req_o.stb <= '0';
            -- On acknowledgment
            trig(14) <= wb_mst_res_i.ack; --monitor state
            if (wb_mst_res_i.ack = '1') then
              trig(7 downto 0) <= wb_mst_res_i.data(7 downto 0);
              trig(11 downto 8) <= wb_mst_res_i.stat;
              -- Increment the counter
              vfat2_counter := vfat2_counter + 1;
              -- Change state
              state := s_end;
            end if;                    
          when s_end =>
            trig(15) <= '1'; --monitor state
            com_get_started_end <= '1';
        end case;
      else
        state := s_init;
        com_get_started_end <= '0';
      end if;
       
    end if;
  end process;
end Behavioral;

