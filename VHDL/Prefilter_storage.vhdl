
library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Prefilter_package.all;
--! @brief Pre-filter state variable storage
--!
--! Stores in a RAM based barrel shifter two state variables
--!
entity Prefilter_Storage is
  generic (
    --! Size of the RAM = N_notes * N_octaves - 3 in run mode.
    --! Set arbitrary to 29 for debug and and alone mode.
    ram_locations_size : positive := 29
    );
  port (
    CLK        : in  std_logic;
    RST        : in  std_logic;
    reg_sync   : in  std_logic;
    SV_sin_in  : in  reg_type;
    SV_cos_in  : in  reg_type;
                                        --! Sine output.\n
                                        --! After the rising edge of the master clock:
                                        --! * when the reg_sync is high, the data is valid in parallel mode.
                                        --! * when the reg_sync is low, the data is shifted by arithm_size.
                                        --! the MSB is filled up with '-'
    SV_sin_out : out reg_type;
                                        --! Cosine output.\n
                                        --! After the rising edge of the master clock:
                                        --! * when the reg_sync is high, the data is valid in parallel mode.
                                        --! * when the reg_sync is low, the data is shifted by arithm_size.
                                        --! the MSB is filled up with '-'
    SV_cos_out : out reg_type
    );
end entity Prefilter_Storage;

--! This architecture manages the multiplexing
--! of reg_size bits of sine and reg_size bits of cosine into
--! words of ram_data_size of a RAM.\n
--! Since there is no Arithmetic, ram_data_size is greater than arithm size.
--! Then there is time between two reg_sync to handle the multiplexing.
--! That avoid the compiler to try to do it and to reduce the master clock frequency.\n
--! If the FGPA or the ASIC allows a direct reg_size * 2 data RAM,
--! another architecture can be written.\n
--! To keep a standard inter-modules interface, we build reg_type registers
--! shifted by arithm size between the reg_sync (active)
architecture arch of Prefilter_Storage is
  --! Is the number of blocs of ram_data_size to store an arithm_size vector.\n
  --! The reg_size may not be a multiple of the ram_data_size.
  --! Then the number of blocs should be celled, in the case of a non integer.
  constant ram_bloc_size            : positive := (reg_size + ram_data_size - 1)/ ram_data_size;
  -- This should be improved with a function to compute
  -- the number of bits for the ram
  constant ram_addr_size            : positive := 7;
  signal sc_io_regs                 : std_logic_vector(2 * ram_data_size * ram_bloc_size - 1 downto 0);
  signal din, dout                  : std_logic_vector(ram_data_size - 1 downto 0);
  signal write_read_enable          : std_logic;
  -- RAM global counter
  signal ram_pos                    : std_logic_vector(ram_addr_size - 1 downto 0);
  -- This should be improved with a function to compute
  -- the number of bits for the ram
  -- It should have states for:
  -- * sin and cosine => * 2
  -- * a hold state at the end and at the beginning
  -- * a sequence: set the address, the din and the enable,
  --
  -- TODO TODO

  constant multiplex_bits : positive := 4;
  signal multiplex_state  : std_logic_vector(multiplex_bits - 1 downto 0);
  type ram_t is array(0 to 2**ram_addr_size - 1) of std_logic_vector(ram_data_size - 1 downto 0);
  --! The RAM of ram_addr_size X ram_data_size
  signal the_ram          : ram_t;
begin
  assert false report "for the prefilter, a RAM " & integer'image(2**ram_addr_size) & "X" & integer'image(ram_data_size) & " has been built"
    severity note;
  assert 2**ram_addr_size >= 2 * ram_locations_size * ram_bloc_size report "Internal error" severity failure;
  assert ram_data_size * ram_bloc_size >= reg_size report "Internal error" severity failure;

  main_proc : process(CLK)
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          -- Load the internal registers from the input
          sc_io_regs(sc_io_regs'low + reg_size - 1 downto sc_io_regs'low) <= SV_sin_in;
          sc_io_regs(sc_io_regs'low + sc_io_regs'length / 2 + reg_size - 1 downto
                     sc_io_regs'low + sc_io_regs'length / 2) <= SV_cos_in;
          -- Load the output shift registers from the internal registers
          SV_sin_out <= sc_io_regs(sc_io_regs'low + reg_size - 1 downto sc_io_regs'low);
          SV_cos_out <= sc_io_regs(sc_io_regs'low + sc_io_regs'length / 2 + reg_size - 1 downto
                                     sc_io_regs'low + sc_io_regs'length / 2);
          multiplex_state <= (others => '0');
        else
          -- Shift the output registers
          SV_sin_out(SV_sin_out'high - arithm_size downto SV_sin_out'low) <=
            SV_sin_out(SV_sin_out'high downto SV_sin_out'low + arithm_size);
          SV_cos_out(SV_cos_out'high - arithm_size downto SV_sin_out'low) <=
            SV_cos_out(SV_cos_out'high downto SV_sin_out'low + arithm_size);
          -- No new data is coming using a serial mode
          -- The new data is loaded using parallel mode on the reg_sync
          -- Please note, the client can NOT use the reg_sync to set some
          -- variables such as the sign
          -- However, it is not a problem as this entity is intended
          -- to the IIR filter only
          SV_sin_out(SV_sin_out'high downto SV_sin_out'high - arithm_size + 1) <= (others => '-');
          SV_cos_out(SV_cos_out'high downto SV_cos_out'high - arithm_size + 1) <= (others => '-');
          -- There are ram_bloc_state read_modify write to do    
          -- * 2 as there is 2 RAM addr, data and enable states
          -- * 2 as there is the sin and the cosine
          MPS : if to_integer(unsigned(multiplex_state)) /= (2 * ram_bloc_size * 2) then
            -- To be compatible with many RAMs, the strategy is
            -- * set the address, the din and the R and W to disable on even multiplex state
            -- * set the R and the W to enable on the odd multiplex state
            if multiplex_state(multiplex_state'low) = '0' then
              sc_io_regs(sc_io_regs'low + ram_data_size - 1 downto sc_io_regs'low) <=
                the_ram(to_integer(unsigned(ram_pos)));
              the_ram(to_integer(unsigned(ram_pos))) <=
                sc_io_regs(sc_io_regs'low + ram_data_size - 1 downto sc_io_regs'low);
            else
              sc_io_regs(sc_io_regs'high - ram_data_size downto sc_io_regs'low) <=
                sc_io_regs(sc_io_regs'high downto sc_io_regs'low + ram_data_size);
              sc_io_regs(sc_io_regs'high downto sc_io_regs'high - ram_data_size + 1) <=
                sc_io_regs(sc_io_regs'low + ram_data_size - 1 downto sc_io_regs'low);

              if unsigned(ram_pos) = to_unsigned(2 * ram_bloc_size * ram_locations_size - 1, ram_pos'length) then
                ram_pos <= (others => '0');
              else
                ram_pos <= std_logic_vector(unsigned(ram_pos) + 1);
              end if;
            end if;
            multiplex_state   <= std_logic_vector(unsigned(multiplex_state) + 1);
            write_read_enable <= multiplex_state(multiplex_state'low);
          else
            --Irrelevant for the logic.
            -- However some implementations set the RAM as standby
            write_read_enable <= '0';
          end if MPS;
        end if REGSYNC_IF;
      else
        ram_pos         <= (others => '0');
        multiplex_state <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

--Prefilter_Storage_RAM_instanc : Prefilter_Storage_RAM generic map(
--  ram_addr_size => ram_addr_size)
--  port map (
--    CLK               => CLK,
--    write_read_enable => write_read_enable,
--    ram_pos           => ram_pos,
--    din               => din,
--    dout              => dout
--    );
end architecture arch;
