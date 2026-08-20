

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;
--! @brief Pre-filter state variable storage
--!
--! Stores in a RAM based barrel shifter two state variables
--!
entity Prefilter_RAM_Storage is
  generic (
    --! Size of the RAM = N_notes * N_octaves - the latency in run mode.
      Prefilter_latency : positive
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    --! Void as it runs as a barrel shifter
    meta_data_in  : in  meta_data_t;
    --! Void as it runs as a barrel shifter
    meta_data_out : in  meta_data_t;
    --! The photo is taken during the register sync.\n
    --! BE CAREFULL it should be connected to the input side of the filter
    scz_in     : in  reg_sin_cos_z;
    --! Output cosine register.\n
    --! To keep a standard inter-modules interface, thees reg_type registers
    --! are shifted by arithm size between the reg_sync (active).\n
    --! BE CAREFULL it should be connected to the output side of the filter
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_RAM_Storage;

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
architecture arch of Prefilter_RAM_Storage is
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
  assert false report "For the prefilter, a RAM " & integer'image(2**ram_addr_size) & "X" & integer'image(ram_data_size) & " has been built"
    severity note;
  assert 2**ram_addr_size >= 2 * ( N_octaves * N_notes - Prefilter_latency ) * ram_bloc_size report "Internal error" severity failure;
  assert ram_data_size * ram_bloc_size >= reg_size report "Internal error" severity failure;

  main_proc : process(CLK)
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          -- Load the internal registers from the input
          sc_io_regs(sc_io_regs'low + reg_size - 1 downto sc_io_regs'low) <= scz_in.the_sin;
          sc_io_regs(sc_io_regs'low + sc_io_regs'length / 2 + reg_size - 1 downto
                     sc_io_regs'low + sc_io_regs'length / 2) <= scz_in.the_cos;
          -- Load the output shift registers from the internal registers
          scz_out.the_sin <= sc_io_regs(sc_io_regs'low + reg_size - 1 downto sc_io_regs'low);
          scz_out.the_cos <= sc_io_regs(sc_io_regs'low + sc_io_regs'length / 2 + reg_size - 1 downto
                                     sc_io_regs'low + sc_io_regs'length / 2);
          multiplex_state <= (others => '0');
        else
          -- Shift the output registers
          scz_out.the_sin(scz_out.the_sin'high - arithm_size downto scz_out.the_sin'low) <=
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'low + arithm_size);
          scz_out.the_cos(scz_out.the_cos'high - arithm_size downto scz_out.the_sin'low) <=
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_sin'low + arithm_size);
          -- No new data is coming using a serial mode
          -- The new data is loaded using parallel mode on the reg_sync
          -- Please note, the client can NOT use the reg_sync to set some
          -- variables such as the sign
          -- However, it is not a problem as this entity is intended
          -- to the IIR filter only
          scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <= (others => '-');
          scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <= (others => '-');
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

              if unsigned(ram_pos) = to_unsigned(2 * ram_bloc_size * ( N_octaves * N_notes - Prefilter_latency ) - 1, ram_pos'length) then
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


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;

entity Prefilter_Dummy_Storage is
    generic (
      --! Void as the result is a constant
      Prefilter_latency : positive;
      default_value     : reg_type := ( others => '0' )
      );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      --! Void
      meta_data_in  : in  meta_data_t;
      meta_data_out : in  meta_data_t;
      --! Void
      scz_in        : in  reg_sin_cos_z;
      --! Data (constant) to be sent
      scz_out       : out reg_sin_cos_z
      );
end entity Prefilter_Dummy_Storage;


architecture arch of Prefilter_Dummy_Storage is

begin
  assert false report "For the prefilter, a dummy storage is used" severity note;
  scz_out.the_sin <= default_value;
  scz_out.the_cos <= default_value;

end architecture arch;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;


entity Prefilter_Direct_Storage is
  generic (
    --! Void as it is not a barrel shifter
    Prefilter_latency : positive
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    --! The meta data of which state variable should be read
    meta_data_in  : in  meta_data_t;
    --! The meta data of which state variable should be written back.
    meta_data_out : in  meta_data_t;
    --! Data to be written back
    scz_in        : in  reg_sin_cos_z;
    --! Data to be read
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_Direct_Storage;

architecture arch of Prefilter_Direct_Storage is
  type mem_array is array (0 to N_octaves * N_notes - 1) of reg_type;
  signal sine_memory : mem_array;
  signal cosine_memory : mem_array;
begin  -- architecture arch of Prefilter_Direct_Storage
  assert false report "For the prefilter, a direct " & integer'image(N_octaves * N_notes) &
    " registers has been built"
    severity note;
  main_proc : process(CLK)
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          scz_out.the_sin <=
            sine_memory( to_integer( unsigned( meta_data_in.note )) * N_octaves +
                         to_integer( unsigned( meta_data_in.octave )) );
          scz_out.the_cos <=
            cosine_memory( to_integer( unsigned( meta_data_in.note )) * N_octaves +
                         to_integer( unsigned( meta_data_in.octave )) );
          sine_memory( to_integer( unsigned( meta_data_out.note )) * N_octaves +
                         to_integer( unsigned( meta_data_out.octave )) ) <=
            scz_out.the_sin;
          cosine_memory( to_integer( unsigned( meta_data_out.note )) * N_octaves +
                         to_integer( unsigned( meta_data_out.octave )) ) <=
            scz_out.the_cos;
        else
          scz_out.the_sin(scz_out.the_sin'high - arithm_size downto scz_out.the_sin'low ) <=
          scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'low + arithm_size);
          scz_out.the_cos(scz_out.the_cos'high - arithm_size downto scz_out.the_cos'low ) <=
          scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'low + arithm_size);
        end if REGSYNC_IF;
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;
end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;

entity Prefilter_Barrel_shifter_storage is
    generic (
      Prefilter_latency : positive
      );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      --! Void
      meta_data_in  : in  meta_data_t;
      meta_data_out : in  meta_data_t;
      --! Void
      scz_in        : in  reg_sin_cos_z;
      --! Data (constant) to be sent
      scz_out       : out reg_sin_cos_z
      );
end entity Prefilter_Barrel_shifter_storage;

architecture arch of Prefilter_Barrel_shifter_storage is
  type BS_type is array (N_notes * N_octaves - Prefilter_latency - 1 downto 0) of reg_type;
  signal sin_BS : BS_type;
  signal cos_BS : BS_type;
begin  -- architecture arch
  assert false report "For the prefilter, a direct " & integer'image(N_octaves * N_notes - Prefilter_latency) &
    " registers has been built"
    severity note;
  assert N_notes * N_octaves - Prefilter_latency > 1
    report "The prodcut of the number of note by the number of octaves minus the prefilter latency ("&
    integer'image(N_notes * N_octaves - Prefilter_latency ) &
    ") should be at least 2"
    severity failure;

  scz_out.the_sin <= sin_BS( sin_BS'low );
  scz_out.the_cos <= cos_BS( cos_BS'low );
  main_proc : process(CLK)
    variable temp : reg_type;
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
            sin_BS( sin_BS'high - 1 downto sin_BS'low ) <=
              sin_BS( sin_BS'high downto sin_BS'low + 1 );
            cos_BS( cos_BS'high - 1 downto cos_BS'low ) <=
              cos_BS( cos_BS'high downto cos_BS'low + 1 );
          sin_BS( sin_BS'high ) <= scz_in.the_sin;
          cos_BS( cos_BS'high ) <= scz_in.the_cos;
        else
          temp := sin_BS( sin_BS'low );
          temp( temp'high - 1 downto temp'low ) :=
            temp( temp'high downto temp'low + 1 );
          sin_BS( sin_BS'low ) <= temp;
          temp := cos_BS( cos_BS'low );
          temp( temp'high - 1 downto temp'low ) :=
            temp( temp'high downto temp'low + 1 );
          cos_BS( cos_BS'low ) <= temp;
        end if REGSYNC_IF;
      else
        sin_BS <= ( others => ( others => '0' ));
        cos_BS <= ( others => ( others => '0' ));
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;
  
end architecture arch;
