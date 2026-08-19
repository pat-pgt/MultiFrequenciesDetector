library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
--  work.InterModule_formats.all,
--  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;


--! @brief Pre-filter meta-data to coefficient compute
--!
--! This entity computes:
--! * The delay of the metadata to through out.
--!     The IIR filter has a latency of 3 registers (+1)
--! * The number of shifts needed.
--! It is a separate one as it is required only once for the sine and the cosine.
entity Prefilter_metadata_and_shifts_compute is
  generic (
    the_stage_offset          : real;
    prefilter_not_lightfilter : boolean := true;
    latency                   : positive
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    --
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    shifts_calc   : out std_logic_vector);
end entity Prefilter_metadata_and_shifts_compute;

architecture arch of Prefilter_metadata_and_shifts_compute is
  signal meta_data_list : meta_data_list_t(latency downto 1);
begin
  meta_data_out <= meta_data_list(meta_data_list'high);
  meta_data_proc_proc : process (CLK) is
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        meta_data_list(meta_data_list'high) <= meta_data_in;
        meta_data_list(meta_data_list'high - 1) <=
          meta_data_list(meta_data_list'low + 1);
      end if REGSYNC_IF;
    end if CLK_IF;
  end process meta_data_proc_proc;

  prefilter_mode : if prefilter_not_lightfilter generate
    --! In the real prefilter mode,
    --!   the shifts should be increased each time the frequency is divided by 2
    --! However, there is a not that direct link between the octave and the shifts
    --!   as the note is the threshold to increment/decrement the shifts.
    shifts_proc : process (CLK) is
    begin
      CLK_IF : if rising_edge(CLK) then
        REGSYNC_IF : if reg_sync = '1' then
          -- TODO place the real computation
          shifts_calc <= std_logic_vector(to_unsigned(to_integer(unsigned(meta_data_in.octave)),
                                                      shifts_calc'length));
        else

        end if REGSYNC_IF;
      end if;
    end process shifts_proc;
  end generate prefilter_mode;

  lightfilter_mode : if not prefilter_not_lightfilter generate
    --! In the real lightfilter mode,
    --!   the shifts should be independent to the octave
    --!   as the down-sampling sets each octave on its own sampling rate.
    --! Among all the notes of a given octave,
    --!   the cutoff frequencies are in a 2 ratio.
    --! Then, for this light filter, the shift is a value or the value plus 1.
    shifts_proc : process (CLK) is
    begin
      CLK_IF : if rising_edge(CLK) then
        REGSYNC_IF : if reg_sync = '1' then
          -- TODO place the real computation
          if to_integer(unsigned(meta_data_in.note)) > 2 then
            shifts_calc <= std_logic_vector(to_unsigned(1, shifts_calc'length));
          else
            shifts_calc <= std_logic_vector(to_unsigned(0, shifts_calc'length));
          end if;
        else

        end if REGSYNC_IF;
      end if;
    end process shifts_proc;
  end generate lightfilter_mode;

end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Prefilter_package.all;
--! @brief Pre-filter IIR compute_diff
--!
--! It works by the same way as the other Cordic stages.\n
--! It takes its data from the shift register of the previous stages:
--!   * the filter input
--!   * the state variable storage
--! It places the result in its output shift register.\n
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
          data_out(data_out'high downto data_out'high - arithm_size + 1) <=
            result_ImSV(result_ImSV'high - 1 downto result_ImSV'low);
          carry_input_minus_statevar <= result_ImSV(result_ImSV'high);
          -- And shift for arithm_size
          data_out(data_out'high - arithm_size downto data_out'low) <=
            data_out(data_out'high downto data_out'low + arithm_size);
        end if REGSYNC_IF;
      else
        carry_input_minus_statevar <= '1';
        data_out                   <= (others => '0');
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
--! The shifts architectures of the pre-filter stages
--!   are designed a numbers of shifts
--!   between a minimum (defined in the generics)
--!   and a difference from the input port.
--! A mask shifted at each CLK cycle has been prefered
--!   as a counter system would have been too complex
--!   for the full test coverage especially of the arithm_size.
--! For given values, especially arithm_size, or a power of 2,
--!   specific entities can be written.

entity Prefilter_IIR_stage_shift is
  generic (
    --! Maximum number of shifts
    shifts_max  : positive;
    --! 
    delta_shifts : natural);
  port(
    CLK         : in  std_logic;
    RST         : in  std_logic;
    reg_sync    : in  std_logic;
    shifts_calc : in  std_logic_vector;
    data_in     : in  reg_type;
    data_out    : out reg_type
    );
end entity Prefilter_IIR_stage_shift;

architecture arch of Prefilter_IIR_stage_shift is
  signal sign_bit      : std_logic;
  signal data_selected : std_logic_vector(arithm_size - 1 downto 0);

  signal sign_mask : std_logic_vector(reg_size - shifts_max + 1 + delta_shifts - 1 downto 0);
  signal arithm_sign_mask : std_logic_vector(arithm_size - 1 downto 0);
begin
  assert shifts_max < reg_size
    report "The maximum number of shifts (" & integer'image(shifts_max) & " + " & integer'image(delta_shifts) &
    ") should be lower than the register size"
    severity failure;
  assert shifts_max < ( reg_size + arithm_size ) and shifts_max < ( reg_size - 4 )
    report "The maximum number of shifts (" & integer'image(shifts_max) & " + " & integer'image(delta_shifts) &
    ") should be lower than the register size plus the arithmetic size" &
    "and lower than the register size minus 4." &
    "It is a non sense as the filter would never output anything"
    severity warning;
  assert shifts_max > delta_shifts
    report "The maximum shifts (" & integer'image(shifts_max) &
    ") should be greter than then delta_shifts (" & integer'image( delta_shifts ) & ")"
    severity ERROR;
  
  main_proc : process (CLK) is
    variable sub_mask : std_logic_vector( arithm_size - 1 downto 0 );
    variable sign_vector : std_logic_vector( arithm_size - 1 downto 0 );
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          sign_bit         <= data_in(data_in'high);
          sign_mask <= (others => '0');
        else
          --! Step one: make the selection in the input register
          data_selected <= data_in(data_in'low + shifts_max - to_integer(unsigned(shifts_calc) + arithm_size - 1) downto
                                   data_in'low + shifts_max - to_integer(unsigned(shifts_calc)));
          --! Step two: run the mask for the next clock cycle
          global_shift : for ind_sign_mask in 0 to arithm_size - 1 loop
            if sign_mask'high - arithm_size - ind_sign_mask >= sign_mask'low then
              sign_mask(sign_mask'high - arithm_size - ind_sign_mask) <=
                sign_mask(sign_mask'high - ind_sign_mask);
            end if;
            sign_mask(sign_mask'high downto sign_mask'high - arithm_size + 1) <= (others => '1');
          end loop global_shift;
          --! Step three: 
          sub_mask := sign_mask( sign_mask'low + to_integer(unsigned(shifts_calc)) + arithm_size - 1
                                    downto sign_mask'low + to_integer(unsigned(shifts_calc)));
          sign_vector := (others => sign_bit);
          data_out(data_out'high downto data_out'high - arithm_size + 1 ) <=
            ( data_selected and not sub_mask ) or ( sign_vector and sub_mask );
          --! Step four: run the shifts as usual.
          data_out(data_out'high - arithm_size downto data_out'low) <=
            data_out(data_out'high downto data_out'low + arithm_size);
        end if REGSYNC_IF;
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Prefilter_package.all;

--! @brief Pre-filter IIR compute_add
--!
--! It works by the same way as the other Cordic stages.\n
--! It takes its data from the shift register of the previous stages:
--! * the shifter
--! * the delayed state variable storage
--! It places the result in its output shift register.
--! It is even simpler as it is always a addition,
--!   independent from any data bit.
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
  signal carry_final_add : std_logic;

begin

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
-- (others =>'0');
            state_var_in(state_var_in'low + arithm_size - 1 downto state_var_in'low);
          op_SHFT(op_SHFT'high) := '0';
          op_SHFT(op_SHFT'high - 1 downto op_SHFT'low) :=
            data_in(data_in'low + arithm_size - 1 downto data_in'low);
                                        -- Do it
          result_fa := std_logic_vector(unsigned(op_L_SV) + unsigned(op_SHFT) + unsigned(carry_vector));
                                        -- Place the result
          state_var_data_out(state_var_data_out'high downto state_var_data_out'high - arithm_size + 1) <=
            result_fa(result_fa'high - 1 downto result_fa'low);
          carry_final_add <= result_fa(result_fa'high);
                                        -- And shift for arithm_size
          state_var_data_out(state_var_data_out'high - arithm_size downto state_var_data_out'low) <=
            state_var_data_out(state_var_data_out'high downto state_var_data_out'low + arithm_size);

        end if REGSYNC_IF;
      else
        carry_final_add    <= '0';
        state_var_data_out <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process final_add_proc;

end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
--  work.Meta_data_package.all,
  work.Prefilter_package.all;

entity Prefilter_Delay is
  generic(
    latency : positive);
  port (
    CLK      : in  std_logic;
    RST      : in  std_logic;
    reg_sync : in  std_logic;
    scz_in   : in  reg_sin_cos_z;
    scz_out  : out reg_sin_cos_z
    );
end entity Prefilter_Delay;

architecture arch of Prefilter_Delay is
  signal state_var_delay_s : reg_type_list(latency downto 1);
  signal state_var_delay_c : reg_type_list(latency downto 1);
begin
  scz_out.the_sin <= state_var_delay_s(state_var_delay_s'high);
  scz_out.the_cos <= state_var_delay_c(state_var_delay_c'high);

  main_proc : process (CLK) is
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '0' then
                                        -- This is equivalent to write a component to transfer
                                        --   without any arithmetic's, and to place it under a generate
        shifts_delay_RAM_s : for ind in 1 to state_var_delay_c'length loop
                                        -- Shift all the registers themselves
          state_var_delay_s(state_var_delay_s'low + ind - 1)(
            state_var_delay_s(state_var_delay_s'low + ind - 1)'high - arithm_size downto
            state_var_delay_s(state_var_delay_s'low + ind - 1)'low) <=
            state_var_delay_s(state_var_delay_s'low + ind - 1)(
              state_var_delay_s(state_var_delay_s'low + ind - 1)'high downto
              state_var_delay_s(state_var_delay_s'low + ind - 1)'low + arithm_size);
          if ind /= 1 then
                                        -- Shift the low of the register N to the high of the register N+1
            state_var_delay_s(state_var_delay_s'low + ind - 2)(
              state_var_delay_s(state_var_delay_s'low + ind - 2)'high downto
              state_var_delay_s(state_var_delay_s'low + ind - 2)'high - arithm_size + 1) <=
              state_var_delay_s(state_var_delay_s'low + ind - 1)(
                state_var_delay_s(state_var_delay_s'low + ind - 1)'low + arithm_size - 1 downto
                state_var_delay_s(state_var_delay_s'low + ind - 1)'low);
          else
                                        -- Supply the register with the input
            state_var_delay_s(state_var_delay_s'high)(
              state_var_delay_s(state_var_delay_s'high)'high downto
              state_var_delay_s(state_var_delay_s'high)'high - arithm_size + 1) <=
              scz_in.the_sin(scz_in.the_sin'low +arithm_size - 1 downto scz_in.the_sin'low);
          end if;
        end loop shifts_delay_RAM_s;
        shifts_delay_RAM_c : for ind in 1 to state_var_delay_c'length loop
                                        -- Shift all the registers themselves
          state_var_delay_c(state_var_delay_c'low + ind - 1)(
            state_var_delay_c(state_var_delay_c'low + ind - 1)'high - arithm_size downto
            state_var_delay_c(state_var_delay_c'low + ind - 1)'low) <=
            state_var_delay_c(state_var_delay_c'low + ind - 1)(
              state_var_delay_c(state_var_delay_c'low + ind - 1)'high downto
              state_var_delay_c(state_var_delay_c'low + ind - 1)'low + arithm_size);
          if ind /= 1 then
                                        -- Shift the low of the register N to the high of the register N+1
            state_var_delay_c(state_var_delay_c'low + ind - 2)(
              state_var_delay_c(state_var_delay_c'low + ind - 2)'high downto
              state_var_delay_c(state_var_delay_c'low + ind - 2)'high - arithm_size + 1) <=
              state_var_delay_c(state_var_delay_c'low + ind - 1)(
                state_var_delay_c(state_var_delay_c'low + ind - 1)'low + arithm_size - 1 downto
                state_var_delay_c(state_var_delay_c'low + ind - 1)'low);
          else
                                        -- Supply with the input
            state_var_delay_c(state_var_delay_c'high)(
              state_var_delay_c(state_var_delay_c'high)'high downto
              state_var_delay_c(state_var_delay_c'high)'high - arithm_size + 1) <=
              scz_in.the_cos(scz_in.the_cos'low +arithm_size - 1 downto scz_in.the_cos'low);
          end if;
        end loop shifts_delay_RAM_c;
      end if REGSYNC_IF;
    end if CLK_IF;
  end process main_proc;
end architecture arch;

