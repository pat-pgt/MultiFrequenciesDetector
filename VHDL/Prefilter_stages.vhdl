library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
--  work.InterModule_formats.all,
--  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;


--! @brief Pre-filter meta-data to coeff compute
--!
--! This entity computes:
--! * The delay of the metadata to through out.
--!     The IIR filter has a latency of 3 registers (+1)
--! * The number of shifts needed.
--! It is a separate one as it is required only once for the sine and the cosine.
entity Prefilter_metadata_and_shifts_compute is
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    --
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    shifts_calc   : out shifts_IIR_data);
end entity Prefilter_metadata_and_shifts_compute;

architecture arch of Prefilter_metadata_and_shifts_compute is

begin

end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Prefilter_package.all;
--! @brief Pre-filter IIR compute_diff
--!
--! It works by the same way as the other cordic stages.
--! It is even simpler as it is always a subtraction,
--!   independent from any data bit.
entity Prefilter_IIR_stage_diff is
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    state_var_in  : in  reg_type;
    data_input_in : in  reg_type;
    data_out      : out reg_type
    );
end entity Prefilter_IIR_stage_diff;


architecture rtl of Prefilter_IIR_stage_diff is
  signal data_out_s                 : reg_type;
  signal carry_input_minus_statevar : std_logic;
-- From shifts to final addition
begin
  assert reg_size mod arithm_size = 0
    report "The size of the registers (" & integer'image(reg_size) &
    ") should be a multiple of the arithm_size (" & integer'image(arithm_size) & ")"
    severity failure;
  assert (reg_size / arithm_size) > 1
    report "The size of the registers (" & integer'image(reg_size) &
    ") should be at least twice of the arithm_size (" & integer'image(arithm_size) & ")"
    severity failure;

  data_out <= data_out_s;

  proc_I_minus_SV : process(CLK)
    variable carry_vector : std_logic_vector(arithm_size downto 0);
    variable op_SV, op_I  : std_logic_vector(arithm_size downto 0);
    variable result_ImSV  : std_logic_vector(arithm_size downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          -- Nothing special to do here about configuration,
          --   as the computation is independent of the sign of the operands.
          -- Set carry to 1 as the subtraction is A - not B + 1
          carry_input_minus_statevar <= '1';
        else
          -- Set the variables
          carry_vector(carry_vector'low)                              := carry_input_minus_statevar;
          carry_vector(carry_vector'high downto carry_vector'low + 1) := (others => '0');
          op_SV(op_SV'high)                                           := '0';
          op_SV(op_SV'high - 1 downto op_SV'low) :=
            not state_var_in(state_var_in'low + arithm_size - 1 downto state_var_in'low);
          op_I(op_I'high) := '0';
          op_I(op_I'high - 1 downto op_I'low) :=
            data_input_in(data_input_in'low + arithm_size - 1 downto data_input_in'low);
          -- Do it
          result_ImSV := std_logic_vector(unsigned(op_SV) + unsigned(op_I) + unsigned(carry_vector));
          -- Place the result
          data_out_s(data_out_s'high downto data_out_s'high - arithm_size + 1) <=
            result_ImSV(result_ImSV'high - 1 downto result_ImSV'low);
          carry_input_minus_statevar <= result_ImSV(result_ImSV'high);
          -- And shift for arithm_size
          data_out_s(data_out_s'high - arithm_size downto data_out_s'low) <=
            data_out_s(data_out_s'high downto data_out_s'low + arithm_size);
        end if REGSYNC_IF;
      else
        carry_input_minus_statevar <= '1';
        data_out_s                 <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process proc_I_minus_SV;

end architecture rtl;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.MultiFreqDetect_package.all,
  work.Prefilter_package.all;

--! @brief Pre-filter IIR compute the shifts
--!
--! This is a little bit tricky.\n
--! One could have use a large set of selectors
--!   but it is resource consuming
--!   and may break the small propagation delay of the others.\n 
--! Up to now, the data comes as low endian to high endian.
--! Each module uses the previous shift register for its operands
--! Each module provides its own register for the results
--!   and for the operands of the next one.
--! Then more registers are needed\n
entity Prefilter_IIR_stage_shift is
  port(
    CLK         : in  std_logic;
    RST         : in  std_logic;
    reg_sync    : in  std_logic;
    shifts_calc : in  shifts_IIR_data;
    data_in     : in  reg_type;
    data_out    : out reg_type
    );
end entity Prefilter_IIR_stage_shift;

architecture arch of Prefilter_IIR_stage_shift is
  signal data_out_s       : reg_type;
  signal temporary_shifts : positive := 4;
begin
  data_out <= data_out_s;

  shift_I_minus_SV : process(CLK)

  begin
    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          -- Nothing special to do here about configuration,
          --   as the computation is independent of the sign of the operands.


          -- Temporary code for testing the test
          data_out_s(data_out_s'high - temporary_shifts downto data_out_s'low) <=
            data_in(data_in'high downto data_in'low + temporary_shifts);
          data_out_s(data_out_s'high downto data_out_s'high - temporary_shifts + 1) <=
            (others => data_in(data_in'high));
        else
          data_out_s(data_out_s'high - arithm_size downto data_out_s'low) <=
            data_out_s(data_out_s'high downto data_out_s'low + arithm_size);
          -- There is no high fill u}p as the load is done in parrallel mode
          -- For debug
          data_out_s(data_out_s'high downto data_out_s'high - arithm_size + 1) <= (others => '-');
        end if REGSYNC_IF;
      else
        data_out_s <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process shift_I_minus_SV;

end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Prefilter_package.all;

entity Prefilter_IIR_stage_add is
  port (
    CLK                : in  std_logic;
    RST                : in  std_logic;
    reg_sync           : in  std_logic;
    state_var_in       : in  reg_type;
    data_in            : in  reg_type;
    state_var_data_out : out reg_type
    );
end entity Prefilter_IIR_stage_add;

architecture arch of Prefilter_IIR_stage_add is
  signal carry_final_add      : std_logic;
  signal state_var_data_out_s : reg_type;

begin
  state_var_data_out <= state_var_data_out_s;

  final_add_proc : process(CLK)
    variable carry_vector     : std_logic_vector(arithm_size downto 0);
    variable op_L_SV, op_SHFT : std_logic_vector(arithm_size downto 0);
    variable result_fa        : std_logic_vector(arithm_size downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          -- Nothing special to do here about configuration,
          --   as the computation is independent of the sign of the operands.
          -- Set carry to 0 for the first bloc
          carry_final_add <= '0';
        else
          -- Set the variables
          carry_vector(carry_vector'low)                              := carry_final_add;
          carry_vector(carry_vector'high downto carry_vector'low + 1) := (others => '0');
          op_L_SV(op_L_SV'high)                                       := '0';
          op_L_SV(op_L_SV'high - 1 downto op_L_SV'low) :=
            state_var_in(state_var_in'low + arithm_size - 1 downto state_var_in'low);
          op_SHFT(op_SHFT'high) := '0';
          op_SHFT(op_SHFT'high - 1 downto op_SHFT'low) :=
            data_in(data_in'low + arithm_size - 1 downto data_in'low);
          -- Do it
          result_fa := std_logic_vector(unsigned(op_L_SV) + unsigned(op_SHFT) + unsigned(carry_vector));
          -- Place the result
          state_var_data_out_s(state_var_data_out_s'high downto state_var_data_out_s'high - arithm_size + 1) <=
            result_fa(result_fa'high - 1 downto result_fa'low);
          carry_final_add <= result_fa(result_fa'high);
          -- And shift for arithm_size
          state_var_data_out_s(state_var_data_out_s'high - arithm_size downto state_var_data_out_s'low) <=
            state_var_data_out_s(state_var_data_out_s'high downto state_var_data_out_s'low + arithm_size);
          
        end if REGSYNC_IF;
      else
        carry_final_add      <= '0';
        state_var_data_out_s <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process final_add_proc;

end architecture arch;


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
--! another architecture can be written.
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
  --! Output sine and cosine registers.\n
  --! To keep a standard inter-modules interface, we build reg_type registers
  --! shifted by arithm size between the reg_sync (active)
  signal SV_sin_out_s, SV_cos_out_s : reg_type;
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
          -- No new data is coming using a serial mode
          -- The new data is loaded using parallel mode on the reg_sync
          -- Please note, the client can NOT use the reg_sync to set some
          -- variables such as the sign
          -- However, it is not a problem as this entity is intended
          -- to the IIR filter only
          SV_sin_out_s(SV_sin_out_s'high downto SV_sin_out_s'high - arithm_size + 1) <= (others => '-');
          SV_cos_out_s(SV_cos_out_s'high downto SV_cos_out_s'high - arithm_size + 1) <= (others => '-');
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
  generic (
    the_stage_offset : real                 := 1.0;
    debug_level      : integer range 0 to 2 := 0
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_stage;

architecture arch of Prefilter_stage is
  signal shifts_calc               : shifts_IIR_data;
  signal scz_out_s                 : reg_sin_cos_z;
  signal sin_diff_shift            : reg_type;
  signal cos_diff_shift            : reg_type;
  signal sin_shift_add             : reg_type;
  signal cos_shift_add             : reg_type;
  signal meta_data_diff            : meta_data_t;
  -- Is nice for testing with 3 frequencies
  --   separately from the RAM test
  signal temporary_RAM_S           : reg_type;
  signal temporary_RAM_C           : reg_type;
  constant prefilter_diff_latency  : positive := 1;
  constant prefilter_shift_latency : positive := 1;
  constant prefilter_add_latency   : positive := 1;
  -- The latency of the diff module is already handled
  --   in the shift "detector" component
  signal meta_data_delay : meta_data_list_t(prefilter_diff_latency - 1 +
                                            prefilter_shift_latency +
                                            prefilter_add_latency downto 1);
  signal state_var_delay_s : reg_type_list(prefilter_diff_latency + prefilter_shift_latency downto 1);
  signal state_var_delay_c : reg_type_list(prefilter_diff_latency + prefilter_shift_latency downto 1);
begin
  scz_out       <= scz_out_s;
  meta_data_out <= meta_data_delay(meta_data_delay'low);

  assert state_var_delay_s'length > 1 and state_var_delay_c'length > 1
    report "Internal error, the delay should be at least 2 reg_sync"
    severity failure;

  
  main_proc : process (CLK) is
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        -- The metadata is transfered using parallel mode
        meta_data_delay(meta_data_delay'high - 1 downto meta_data_delay'low) <=
          meta_data_delay(meta_data_delay'high downto meta_data_delay'low + 1);
        meta_data_delay(meta_data_delay'high)     <= meta_data_diff;
        -- The state variable delay line has nothing to do during the sync
        -- only load
        state_var_delay_s(state_var_delay_s'high) <= temporary_RAM_S;
        state_var_delay_c(state_var_delay_c'high) <= temporary_RAM_C;
      else
        -- This is equivalent to write a component to transfer
        --   without any arithmetics, and to place it under a generate
        shifts_delay_RAM_s : for ind in 1 to state_var_delay_s'length loop
          -- Shift the register itself
          state_var_delay_s(state_var_delay_s'low + ind - 1)(
            state_var_delay_s(state_var_delay_s'low + ind - 1)'high - arithm_size downto
            state_var_delay_s(state_var_delay_s'low + ind - 1)'low)  <=
            state_var_delay_s(state_var_delay_s'low + ind - 1)(
              state_var_delay_s(state_var_delay_s'low + ind - 1)'high downto
              state_var_delay_s(state_var_delay_s'low + ind - 1)'low + arithm_size);
          -- And pass the arithm_size low to the high of the next register
          if ind /= 1 then
            state_var_delay_s(state_var_delay_s'low + ind - 2)(
              state_var_delay_s(state_var_delay_s'low + ind - 2)'high downto
              state_var_delay_s(state_var_delay_s'low + ind - 2)'high - arithm_size + 1)  <=
              state_var_delay_s(state_var_delay_s'low + ind - 1)(
                state_var_delay_s(state_var_delay_s'low + ind - 1)'low + arithm_size - 1 downto
                state_var_delay_s(state_var_delay_s'low + ind - 1)'low);
          end if;
        end loop shifts_delay_RAM_s;
        shifts_delay_RAM_c : for ind in 1 to state_var_delay_c'length loop
          -- Shift the register itself
          state_var_delay_c(state_var_delay_c'low + ind - 1)(
            state_var_delay_c(state_var_delay_c'low + ind - 1)'high - arithm_size downto
            state_var_delay_c(state_var_delay_c'low + ind - 1)'low)  <=
            state_var_delay_c(state_var_delay_c'low + ind - 1)(
              state_var_delay_c(state_var_delay_c'low + ind - 1)'high downto
              state_var_delay_c(state_var_delay_c'low + ind - 1)'low + arithm_size);
          if ind /= 1 then
            state_var_delay_c(state_var_delay_c'low + ind - 2)(
              state_var_delay_c(state_var_delay_c'low + ind - 2)'high downto
              state_var_delay_c(state_var_delay_c'low + ind - 2)'high - arithm_size + 1)  <=
              state_var_delay_c(state_var_delay_c'low + ind - 1)(
                state_var_delay_c(state_var_delay_c'low + ind - 1)'low + arithm_size - 1 downto
                state_var_delay_c(state_var_delay_c'low + ind - 1)'low);
          end if;
        end loop shifts_delay_RAM_c;
      end if REGSYNC_IF;
    end if CLK_if;
  end process main_proc;

  --! The full service mode
  --! There are no restriction on the number of notes nor octves
  --normal_RAM_mode : if debug_level = 0 generate
  assert debug_level /= 0 report "Sorry not yet implemented" severity failure;
  --end generate normal_RAM_mode;

  --! The RAM is replaced by a loopback
  --! This is to verify the filter itself without the RAM
  --! The number of notes and octaves should restrict to 3 data.
  force_RAM_3_values : if debug_level = 1 generate
    temporary_RAM_S <= scz_out_s.the_sin;
    temporary_RAM_C <= scz_out_s.the_cos;
  end generate force_RAM_3_values;

--! The highest debug level
  --! The output of the RAM is forced to 0
  --!   in order to check the input goes to the output with
  --!   some right shifts
  --! There are no restriction on the number of notes nor octves
  force_RAM_to_0 : if debug_level = 2 generate
    temporary_RAM_S <= (others => '0');
    temporary_RAM_C <= (others => '0');
  end generate force_RAM_to_0;
  
  sine_IIR_diff : Prefilter_IIR_stage_diff port map(
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    state_var_in  => temporary_RAM_S,
    data_out      => sin_diff_shift,
    data_input_in => scz_in.the_sin);

  sine_IIR_shift : Prefilter_IIR_stage_shift port map(
    CLK         => CLK,
    RST         => RST,
    reg_sync    => reg_sync,
    shifts_calc => shifts_calc,
    data_in     => sin_diff_shift,
    data_out    => sin_shift_add);

  sine_IIR_add : Prefilter_IIR_stage_add port map(
    CLK                => CLK,
    RST                => RST,
    reg_sync           => reg_sync,
    state_var_in       => state_var_delay_s(state_var_delay_s'low),
    data_in            => sin_shift_add,
    state_var_data_out => scz_out_s.the_sin);

  cose_IIR_diff : Prefilter_IIR_stage_diff port map(
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    state_var_in  => temporary_RAM_S,
    data_out      => cos_diff_shift,
    data_input_in => scz_in.the_cos);

  cose_IIR_shift : Prefilter_IIR_stage_shift port map(
    CLK         => CLK,
    RST         => RST,
    reg_sync    => reg_sync,
    shifts_calc => shifts_calc,
    data_in     => cos_diff_shift,
    data_out    => cos_shift_add);

  cose_IIR_add : Prefilter_IIR_stage_add port map(
    CLK                => CLK,
    RST                => RST,
    reg_sync           => reg_sync,
    state_var_in       => state_var_delay_c(state_var_delay_c'low),
    data_in            => cos_shift_add,
    state_var_data_out => scz_out_s.the_cos);

  meta_data_compute : Prefilter_metadata_and_shifts_compute port map (
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    meta_data_in  => meta_data_in,
    meta_data_out => meta_data_diff,
    shifts_calc   => shifts_calc);

end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;
--! @brief Prefilter bundle
--!
--! This is the bundle for the whose wants more stages
entity Prefilter_bundle is
  generic (
    --! Defines the number of stages and their offsets ratios
    stages_offsets : prefilter_stages_offset_list;
    debug_level    : integer range 0 to 2 := 0);
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_bundle;

architecture arch of Prefilter_bundle is
  signal scz_interm       : reg_sin_cos_z_list(stages_offsets'length downto 0);
  signal meta_data_interm : meta_data_list_t(stages_offsets'length downto 0);
begin
  assert stages_offsets'length > 0 report "The number of pre-filter stages should not be 0" severity failure;

  meta_data_interm(meta_data_interm'high) <= meta_data_in;
  meta_data_out                           <= meta_data_interm(meta_data_interm'low);
  scz_interm(scz_interm'high)             <= scz_in;
  scz_out                                 <= scz_interm(scz_interm'low);

  Prefilter_generate : for ind in 0 to stages_offsets'length - 1 generate
    bundle_elem : Prefilter_stage generic map (
      the_stage_offset => stages_offsets(stages_offsets'low - ind),
      debug_level      => debug_level)
      port map (
        CLK           => CLK,
        RST           => RST,
        reg_sync      => reg_sync,
        meta_data_in  => meta_data_interm(meta_data_interm'low + ind + 1),
        meta_data_out => meta_data_interm(meta_data_interm'low + ind),
        scz_in        => scz_interm(scz_interm'low + ind + 1),
        scz_out       => scz_interm(scz_interm'low + ind));
  end generate Prefilter_generate;
  
end architecture arch;
