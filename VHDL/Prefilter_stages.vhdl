library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;


--! @brief Pre-filter IIR compute
--!
--! This computes one prefilter. They should be bundled by pair
--! for the sine and the cosine.\n
--! The state variable (of the infinite impulse response)
--! is given at the reg_sync.
--! the N-1 state variable is outputted as well.
entity Prefilter_IIR_compute is
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    shifts_calc   : in  std_logic_vector(4 downto 0);
    state_var_in  : in  reg_type;
    state_var_out : out reg_type;
    sc_in         : in  reg_type;
    sc_out        : out reg_type
    );
end entity Prefilter_IIR_compute;


architecture rtl of Prefilter_IIR_compute is
  -- The computation should be something like
  -- output( N + 1 ) = 0.1 .input + 0.9 . output( N )
  -- To avoid multiplications, the formula is transformed to:*
  -- output( N + 1 ) = output( N ) + 0.1 . ( input - output( N ) )
  -- Since it is a prefilter, only the inverted power of 2
  -- are used as coefficients.
  --
  -- The computation is in 3 steps:
  -- * input - state variable is computed
  -- * this signal is shifted
  -- * The shifted signal is added to the state variable
  --
  -- Some copy and paste, unused signals are going to be removed
  --
  --

  signal scz_out_s                              : reg_sin_cos_z;
  -- CW or CCW of the vector, means the Z pins CCW or CW
  signal carry_X, carry_Y, carry_Z              : std_logic;
  -- The shifted operand is always reaching the MSB of the shift registers
  -- before the non shifted.
  -- Since the new data is coming after the MSB of the current data,
  -- the high bit should be saved, and used as soon as the current data is over
  signal sign_X, sign_Y                         : std_logic;
  signal remaining_shift_count                  : std_logic_vector(5 downto 0);
  signal is_first                               : std_logic;
  signal Z_shifts_count                         : std_logic_vector(5 downto 0);
  signal debug_catch_X_sync, debug_catch_Y_sync : reg_type;
  signal debug_catch_Z_sync                     : reg_type;
  signal debug_flipflop                         : std_logic := '0';
  signal debug_flipflop_2                       : std_logic := '0';
  signal CCW_not_CW                             : std_logic;
  signal X2_plus_Y2                             : std_logic_vector(31 downto 0);


  signal state_var_int1 : reg_type;
  signal sc_int1 : reg_type;
  signal carry          : std_logic;
begin
  -- To be improved with automatic size
  assert reg_size < 2**remaining_shift_count'length report "Internal error" severity failure;
  assert reg_size mod arithm_size = 0
    report "The size of the registers (" & integer'image(reg_size) &
    ") should be a multiple of the arithm_size (" & integer'image(arithm_size) & ")"
    severity failure;
  assert (reg_size / arithm_size) > 1
    report "The size of the registers (" & integer'image(reg_size) &
    ") should be at least twice of the arithm_size (" & integer'image(arithm_size) & ")"
    severity failure;



  proc_V_minus_I : process(CLK)
    variable carry_vector : std_logic_vector(arithm_size downto 0);
    variable op_V, op_I   : std_logic_vector(arithm_size downto 0);
    variable result       : std_logic_vector(arithm_size downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          -- Nothing special to do here
          -- expecially, the computation is independant of the sign of the operands.
          carry          <= '1';
          -- ... except to store, in parallel a copy of the state variable for
          -- the next step
          state_var_int1 <= state_var_int1;
        else
          carry_vector(carry_vector'low)                              := carry;
          carry_vector(carry_vector'high downto carry_vector'low + 1) := (others => '0');
          op_V(op_V'high)                                             := '0';
          op_V(op_V'high - 1 downto op_V'low) :=
            state_var_int1(state_var_int1'low + arithm_size - 1 downto state_var_int1 'low);
          op_I(op_I'high) := '0';
          op_I(op_I'high - 1 downto op_I'low) :=
            sc_in(sc_in'low + arithm_size - 1 downto sc_in'low);
          result := std_logic_vector(not unsigned(op_V) + unsigned(op_I) + unsigned(carry_vector));


          
        end if REGSYNC_IF;
      else
        remaining_shift_count <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process proc_V_minus_I;
end architecture rtl;


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
                                        --! * when the reg_sync is high, the data is valid in parrallel mode.
                                        --! * when the reg_sync is low, the data is shifted by arithm_size.
                                        --! the MSB is filled up with '-'
    SV_sin_out : out reg_type;
                                        --! Cosine output.\n
                                        --! After the rising edge of the master clock:
                                        --! * when the reg_sync is high, the data is valid in parrallel mode.
                                        --! * when the reg_sync is low, the data is shifted by arithm_size.
                                        --! the MSB is filled up with '-'
    SV_cos_out : out reg_type
    );
end entity Prefilter_Storage;

--! This architecture manages the multiplexing
--! of reg_size bits of sine and reg_size bits os cosine into
--! words of ram_data_size of a RAM.\n
--! Since there is no arithmatics, ram_data_size is greater than arithm size.
--! Then there is time between two reg_sync to handle the multiplexing.
--! That avoid the compiler to try to do it and to reduce the master clock frequency.\n
--! If the FGPA or the ASIC allows a direct reg_size * 2 data RAM,
--! another architecture can be written.
architecture arch of Prefilter_Storage is
  --! Is the number of blocs of ram_data_size to store an arithm_size vector.\n
  --! The reg_size may not be a multiple of the ram_data_size.
  --! Then the number of blocs should be ceiled, in the case of a non integer.
  constant ram_bloc_size            : positive := (reg_size + ram_data_size - 1)/ ram_data_size;
  -- This should be improved with a function to compute
  -- the number of bits for the ram
  constant ram_addr_size            : positive := 7;
  signal sc_io_regs                 : std_logic_vector(2 * ram_data_size * ram_bloc_size - 1 downto 0);
  signal din, dout                  : std_logic_vector(ram_data_size - 1 downto 0);
  signal write_read_enable          : std_logic;
  -- RAM global counter
  signal ram_pos                    : std_logic_vector(ram_addr_size - 1 downto 0);
  --! Output sine and cosine registers.\n
  --! To keep a standard inter-modules interface, we build reg_type registers
  --! shifted by arithm size between the reg_sync (active)
  signal SV_sin_out_s, SV_cos_out_s : reg_type;
  -- This should be improved with a function to compute
  -- the number of bits for the ram
  -- It should have states for:
  -- * sin and cosine => * 2
  -- * a hold state at the end and at the begining
  -- * a sequence: set the adress, the din and the enable,
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

  SV_cos_out <= SV_cos_out_s;
  SV_sin_out <= SV_sin_out_s;


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
          SV_sin_out_s <= sc_io_regs(sc_io_regs'low + reg_size - 1 downto sc_io_regs'low);
          SV_cos_out_s <= sc_io_regs(sc_io_regs'low + sc_io_regs'length / 2 + reg_size - 1 downto
                                     sc_io_regs'low + sc_io_regs'length / 2);
          multiplex_state <= (others => '0');
        else
          -- Shift the output registers
          SV_sin_out_s(SV_sin_out_s'high - arithm_size downto SV_sin_out_s'low) <=
            SV_sin_out_s(SV_sin_out_s'high downto SV_sin_out_s'low + arithm_size);
          SV_cos_out_s(SV_cos_out_s'high - arithm_size downto SV_sin_out_s'low) <=
            SV_cos_out_s(SV_cos_out_s'high downto SV_sin_out_s'low + arithm_size);
          -- No new data is comming using a serial mode
          -- The new data is laoded using parallel mode on the reg_sync
          -- Plese note, the client can NOT use the reg_sync to set some
          -- variables such as the sign
          -- However, it is not a problem as this entity is intended
          -- to the IIR filter only
          SV_sin_out_s(SV_sin_out_s'high downto SV_sin_out_s'high - arithm_size + 1) <= (others => '-');
          SV_cos_out_s(SV_cos_out_s'high downto SV_cos_out_s'high - arithm_size + 1) <= (others => '-');
          -- There are ram_bloc_state read_modify write to do    
          -- * 2 as there is 2 RAZM addr, data and enable states
          -- * 2 as there is the sin and the cosine
          MPS : if to_integer(unsigned(multiplex_state)) /= (2 * ram_bloc_size * 2) then
            -- To be compatible with many RAMs, the strategy is
            -- * set the addresse, the din and the R and W to disable on even multiplex state
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

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;
--! @brief Prefilter stage
--!
--! This is a pair of sine and cosine calculation
--! with their associated memory storage and
--! delay for the metadata.
entity Prefilter_stage is
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : in  meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_stage;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;
--! @brief Prefilter bundle
--!
--! This is the bundle for the whoose wants more stages
entity Prefilter_bundle is
  generic (
    --! Defines the number of stages and their offsets ratios
    stages_offsets : prefilter_stages_offset_list );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : in  meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_bundle;

