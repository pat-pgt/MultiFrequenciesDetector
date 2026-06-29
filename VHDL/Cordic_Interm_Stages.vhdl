library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.Input_modules.arctg_2_angle_reg;

--! @brief Selector for angle and state machine
--!
entity Cordic_IntermStage_Z_Selector is
  generic (
    arithm_size  : integer range 1 to 24;
    reg_size     : integer range 16 to 255;
    extra_shifts : integer range 0 to 7;
    shifts_calc  : integer range 1 to reg_size - extra_shifts - arithm_size - 1
    );
  port (
    CLK                : in  std_logic;
    RST                : in  std_logic;
    reg_sync           : in  std_logic;
    strobe_from_scz_in : in  std_logic;
    Z_extra_select     : out std_logic_vector(StateNumbers_2_BitsNumbers(extra_shifts + 1) - 1 downto 0);
    Z_slice            : out std_logic_vector(arithm_size - 1 downto 0)
    );
end entity Cordic_IntermStage_Z_Selector;

architecture arch of Cordic_IntermStage_Z_Selector is
--  signal Z_index : std_logic_vector(StateNumbers_2_BitsNumbers(extra_shifts + 1));
  type Z_data_t is array (extra_shifts + 1 - 1 downto 0) of std_logic_vector(reg_size - 1 downto 0);

  signal angle_add_or_subtract : Z_data_t;
  signal Z_shifts_count        : std_logic_vector(reg_size - 1 downto 0);
begin
  populate_Z : for ind in 0 to extra_shifts generate
    angle_add_or_subtract(ind) <= arctg_2_angle_reg(shifts_calc);
  end generate populate_Z;

  main_proc : process (CLK) is
  begin

    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          Z_shifts_count <= (others => '0');
          if strobe_from_scz_in = '1' or extra_shifts = 0 then
            Z_extra_select <= (others => '0');
            Z_slice        <= angle_add_or_subtract(0)
                       (angle_add_or_subtract(0)'low + arithm_size - 1 downto
                        angle_add_or_subtract(0)'low);
          elsif unsigned(Z_extra_select) /= to_unsigned(extra_shifts, Z_extra_select'length) then
            Z_extra_select                          <= std_logic_vector(unsigned(Z_extra_select) + 1);
            if to_integer(unsigned(Z_extra_select)) <= extra_shifts then
              -- Since the size may not be a power of 2,
              --   the index is tested against the highest possible index
              Z_slice <= angle_add_or_subtract(to_integer(unsigned(Z_extra_select)) + 1)
                         (angle_add_or_subtract(0)'low + arithm_size - 1 downto
                          angle_add_or_subtract(0)'low);
            else
              -- If it is wrong, the index is the highest possible index.
              -- Since the array is a constant, the compiler should only
              --   handle that in the decoding.
              Z_slice <= angle_add_or_subtract(extra_shifts)
                         (angle_add_or_subtract(0)'low + arithm_size - 1 downto
                          angle_add_or_subtract(0)'low);
            end if;
          end if;
        else
          Z_shifts_count                          <= std_logic_vector(unsigned(Z_shifts_count) + 1);
          if to_integer(unsigned(Z_extra_select)) <= extra_shifts then
            -- Since the size may not be a power of 2,
            --   the index is tested against the highest possible index
            Z_slice <= angle_add_or_subtract(to_integer(unsigned(Z_extra_select)) + 1)
                       (angle_add_or_subtract(0)'low + arithm_size - 1 downto
                        angle_add_or_subtract(0)'low);
          else
            -- If it is wrong, the index is the highest possible index.
            -- Since the array is a constant, the compiler should only
            --   handle that in the decoding.
            Z_slice <= angle_add_or_subtract(extra_shifts)
                       (angle_add_or_subtract(0)'low + arithm_size - 1 downto
                        angle_add_or_subtract(0)'low);
          end if;

        end if REGSYNC_IF;
      end if RST_IF;
    end if CLK_IF;

  end process main_proc;

end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers;

--! @brief Selector for the shifted operand
--!
--! There are many cases with different arithmetic size and extra shifts.
--! The selector is an explicit component.\n
--! All the global constants has been redefined here,
--!   to make the generic atomic test easy.
entity Cordic_IntermStage_ShiftSelector is
  generic (
    arithm_size  : integer range 1 to 24;
    reg_size     : integer range 16 to 255;
    extra_shifts : integer range 0 to 7;
    shifts_calc  : integer range 1 to reg_size - extra_shifts - arithm_size - 1
    );
  port (
    CLK      : in std_logic;
    RST      : in std_logic;
    reg_sync : in std_logic;
    --!
    selector : in std_logic_vector(StateNumbers_2_BitsNumbers(extra_shifts + 1) - 1 downto 0);
    --! The switch between the input and the sign bit could have been done
    --!   using counters and comparators.\n
    --! It is done using a shift register.\n
    --! The reason is to not raise more the propagation delay.\n
    --! The extra resources are not that high as more the shifts are high,
    --!   more the shift register to generate the mask is short.
    mask_sign          : in std_logic_vector(arithm_size - 1 downto 0);
    --! The sign is used to populate for the cases over the shifts
    sincos_sign        : in  std_logic;
    --! The slice of the first shift is taken from the scz_in
    sincos_in_slice    : in  std_logic_vector(arithm_size - 1 downto 0);
    --! The slice of the other shifts is taken from the scz_out,
    --!   which is re-entrant for the extra shifts.\n
    --! The size would have been the arithmetic size plus the extra size,
    --!   but since the first one is taken from the scz_in, one is subtracted
    -- KEEP the low index to 0
    sincos_inout_slice : in  std_logic_vector(arithm_size + extra_shifts - 1 - 1 downto 0);
    --! The size of the output, intended to supply the adder or the subtracter
    --!   is always the arithmetic size. 
    sincos_out_slice   : out std_logic_vector(arithm_size - 1 downto 0)
    );
end entity Cordic_IntermStage_ShiftSelector;


architecture arch of Cordic_IntermStage_ShiftSelector is

begin  -- architecture arch
  assert false
    report "scz_in length: " & integer'image(sincos_in_slice'length) &
    ", scz_out length: " & integer'image(sincos_inout_slice'length) &
    ", arithm out length: " & integer'image(sincos_out_slice'length)
    severity note;

  WITH_EXTRA: if extra_shifts > 0 generate
    
    main_proc: process ( all ) is
      variable selected_arithm : std_logic_vector( arithm_size - 1 downto 0 );
    begin  -- process main_proc
      ALL_ZEROS_IF : if or( selector ) = '0' then
        selected_arithm := sincos_in_slice;
      else
        selected_arithm := sincos_inout_slice( arithm_size - 1 + to_integer(unsigned( selector)) - 1 downto
                                               0 + to_integer(unsigned( selector)) - 1 );
      end if ALL_ZEROS_IF;
      sincos_out_slice <= ( selected_arithm and mask_sign ) or ( sincos_sign and not mask_sign);
    end process main_proc;

  end generate WITH_EXTRA;

  NO_EXTRA: if extra_shifts > 0 generate
    
    sincos_out_slice <= ( sincos_in_slice and mask_sign ) or ( sincos_sign and not mask_sign);

  end generate NO_EXTRA;

  
end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  ieee.math_real.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Input_modules.all,
  work.Cordic_package.all;
--! @brief Cordic intermediary stages
--!
--! This computes one Cordic vector spin with its angle (Z) update\n
--! It is common for both\n
--! * To multiply a value by an angle vector, the angle should converged to 0\n
--! * To convert rectangular coordinates to polar, The Y should convergent to 0.
--! For this case, it is required the angle is already in the -PI/2 +PI/2 zone
--! @anchor Cordic_IntermStage_entity
entity Cordic_IntermStage is
  generic (
    Z_not_Y_to_0 : boolean;
    shifts_calc  : integer range 1 to reg_size - 4;
    extra_shifts : integer range 0 to 7
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
end entity Cordic_IntermStage;


architecture rtl of Cordic_IntermStage is
  signal scz_out_s                              : reg_sin_cos_z;
  -- CW or CCW of the vector, means the Z pins CCW or CW
  signal carry_X, carry_Y, carry_Z              : std_logic;
  -- The shifted operand is always reaching the MSB of the shift registers
  -- before the non shifted.
  -- Since the new data is coming during the calculation, by the MSB side,
  -- the high bit should be saved, and used until the current data is over
  signal sign_X, sign_Y                         : std_logic;
  --! The 2**N divided data is done by picking up in the middle of the shift register.
  --! This counter tell the data is over.
  --! Today, it is a basic counter, it may be changed into something else
  --!   in order to not limit the bandwidth.
  signal remaining_shift_count                  : std_logic_vector(5 downto 0);
  signal is_first                               : std_logic;
  signal Z_shifts_count                         : std_logic_vector(5 downto 0);
  signal debug_catch_X2_plus_Y2                 : real;
  signal debug_catch_X_sync, debug_catch_Y_sync : reg_type;
  signal debug_catch_Z_sync                     : reg_type;
  signal debug_flipflop                         : std_logic := '0';
  signal debug_flipflop_2                       : std_logic := '0';
  constant angle_add_or_subtract                : reg_type  := arctg_2_angle_reg(shifts_calc);
  signal CCW_not_CW                             : std_logic;
  signal X2_plus_Y2                             : std_logic_vector(31 downto 0);
  signal selected_X_for_arithm                  : std_logic_vector(arithm_size - 1 downto 0);
  signal selected_Y_for_arithm                  : std_logic_vector(arithm_size - 1 downto 0);
  --
  signal input_sczin_not_scz_out                : std_logic;
  signal extra_shifts_counter                   : std_logic_vector(2 downto 0);
  signal mask_init                              : std_logic_vector( arithm_size - 1 downto 0 );
  -- KEEP low to 0
  signal mask_register                          : std_logic_vector( reg_size - 1 downto 0);
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
  -- Some code has to be written for the case the arithm_size is not 1
  -- For now, this set a restriction, using this code.
  assert shifts_calc mod arithm_size = 0 report "This is not yet implemented" severity error;
  assert extra_shifts = 0 or not Z_not_Y_to_0 report
    "There are " & integer'image(extra_shifts) & " extra shifts in the Z to 0 set. It can not be supported"
    severity error;
  assert extra_shifts < (N_octaves - 1) report
    "The extra shifts (" & integer'image(extra_shifts) &
    ") should be less than the number of octaves (" & integer'image(N_octaves) &
    ") minus 1" severity error;



  assert extra_shifts = 0
    report "WARNING The extra shifts are selected. They are not yet implemented"
    severity warning;




  scz_out <= scz_out_s;


  main_proc : process(CLK)
    variable carry_in_vector_X            : std_logic_vector(arithm_size downto 0);
    variable carry_in_vector_Y            : std_logic_vector(arithm_size downto 0);
    variable carry_in_vector_Z            : std_logic_vector(arithm_size downto 0);
    variable op_N_X, op_S_X               : std_logic_vector(arithm_size downto 0);
    variable op_N_Y, op_S_Y               : std_logic_vector(arithm_size downto 0);
    variable op_N_Z, op_C_Z               : std_logic_vector(arithm_size downto 0);
    variable result_X, result_Y, result_Z : std_logic_vector(arithm_size downto 0);
    variable out_debug_filling            : std_logic_vector(arithm_size - 1 downto 0);
    variable debug_case                   : std_logic_vector(1 downto 0);
    variable inverter_mask                : unsigned(arithm_size downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_if : if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          if Z_not_Y_to_0 then
            -- If Z is negative, the vector should spin CW
            CCW_not_CW <= not scz_in.angle_z(scz_in.angle_z'high);
          else
            -- If Y is negative, the vector should spin CCW
            CCW_not_CW <= scz_in.the_sin(scz_in.the_sin'high);
          end if;
          sign_X                <= scz_in.the_cos(scz_in.the_cos'high);
          sign_Y                <= scz_in.the_sin(scz_in.the_sin'high);
          meta_data_out         <= meta_data_in;
          remaining_shift_count <= std_logic_vector(to_unsigned(reg_size - shifts_calc, remaining_shift_count'length));
          Z_shifts_count        <= (others => '0');
          is_first              <= '1';
          if shifts_calc < 4 and false then
            --
            debug_catch_X2_plus_Y2 <= real(to_integer(signed(scz_in.the_cos)))**2 + real(to_integer(signed(scz_in.the_sin)))**2;
            debug_catch_X_sync     <= scz_in.the_cos;
            debug_catch_Y_sync     <= scz_in.the_sin;
            debug_catch_Z_sync     <= scz_in.angle_z;
          -- This should become dynamic to not overflow,
          -- to respect the bounds as well, if the reg_size is small (<16)
          -- Be careful, this is the data of the input,
          -- then the previous stage
          --X2_plus_Y2 <= std_logic_vector( to_unsigned(
          --to_integer( signed( scz_in.the_sin( scz_in.the_sin'high downto scz_in.the_sin'high - 14) ))**2+
          --to_integer( signed( scz_in.the_cos( scz_in.the_cos'high downto scz_in.the_cos'high - 14) ))**2,
          --X2_plus_Y2'length ));
          end if;
        else
          -- We need to negate the bits for the subtractions
          -- however, we should not negate the "spare" bit on the left
          inverter_mask(inverter_mask'high - 1 downto 0) := (others => '1');
          inverter_mask(inverter_mask'high)              := '0';
          -- Set the high bit to 0, in order to collect the carry
          op_N_X(op_N_X'high)                            := '0';
          op_S_Y(op_S_Y'high)                            := '0';
          op_N_Y(op_N_Y'high)                            := '0';
          op_S_X(op_S_X'high)                            := '0';
          op_N_Z(op_N_Z'high)                            := '0';
          op_C_Z(op_C_Z'high)                            := '0';
          -- Extract the normal operands
          -- that are going to be added to or subtracted from
          op_N_X(op_N_X'high - 1 downto op_N_X'low) :=
            scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
          op_N_Y(op_N_Y'high - 1 downto op_N_Y'low) :=
            scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
          op_N_Z(op_N_Z'high - 1 downto op_N_Z'low) :=
            scz_in.angle_z(scz_in.angle_z'low + arithm_size - 1 downto scz_in.angle_z'low);
          -- Extract the shifted operands that are going to be added or subtracted
          -- The division is performed by a classic shift register
          --   with a loop on the MSB.
          -- The registers can not have a loop as they are receiving the data
          --   for the next value.
          -- After the counter reached its end, the data is supplied with the sign bit
          --   stored at the reg sync
          if unsigned(remaining_shift_count) = to_unsigned(0, remaining_shift_count'length) then
            --All the arithmetic blocs are over, loop on the sign bit
            op_S_X(op_S_X'high - 1 downto op_S_X'low) := (others => sign_X);
            op_S_Y(op_S_Y'high - 1 downto op_S_Y'low) := (others => sign_Y);
          elsif unsigned(remaining_shift_count) < to_unsigned(arithm_size, remaining_shift_count'length) then
            remaining_shift_count <= (others => '0');
          -- Remaining low bits come from the shift register,
          -- high bits are populated with stored the carry
          -- This should never happened if the arithmetic size is 1
          --
          -- Of course, we can NOT do that
          -- assert arithm_size = 1 report "Internal error" severity error;
          -- For now, there is an assert at the beginning
          else
            remaining_shift_count <=
              std_logic_vector(unsigned(remaining_shift_count) - to_unsigned(arithm_size, remaining_shift_count'length));
            -- Not yet, fill up the calculation bloc with the <arithm_size> low bits
            op_S_X(op_S_X'high - 1 downto op_S_X'low) :=
              scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 + arithm_size * shifts_calc downto
                             scz_in.the_cos'low + arithm_size * shifts_calc);
            op_S_Y(op_S_Y'high - 1 downto op_S_Y'low) :=
              scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 + arithm_size * shifts_calc downto
                             scz_in.the_sin'low + arithm_size * shifts_calc);
          end if;
          -- Extract the constant to be added to or subtracted from Z
          -- A selector is used, rather than a shift register
          if (angle_add_or_subtract'low + (to_integer(unsigned (Z_shifts_count)) + arithm_size) - 1) <= angle_add_or_subtract'high then
            op_C_Z(op_C_Z'high - 1 downto op_C_Z'low) :=
              angle_add_or_subtract(
                angle_add_or_subtract'low + (to_integer(unsigned (Z_shifts_count)) + arithm_size) - 1 downto
                angle_add_or_subtract'low + to_integer(unsigned (Z_shifts_count)));
          else
            -- The electronics should always know what to do.
            -- Then the low bit/group of bits is duplicated in case the shifts
            -- are grater than the Z constant.
            -- This should not occur in run mode. It may occur at the end of a reset
            op_C_Z(op_C_Z'high - 1 downto op_C_Z'low) :=
              angle_add_or_subtract(
                angle_add_or_subtract'high downto
                angle_add_or_subtract'high - arithm_size + 1);
          end if;
          Z_shifts_count                                                             <= std_logic_vector(unsigned(Z_shifts_count) + arithm_size);
          -- Prepare the carry in with a padding in order to add properly
          -- operands of the same size
          carry_in_vector_X(carry_in_vector_X'high downto carry_in_vector_X'low + 1) := (others => '0');
          carry_in_vector_Y(carry_in_vector_Y'high downto carry_in_vector_Y'low + 1) := (others => '0');
          carry_in_vector_Z(carry_in_vector_Z'high downto carry_in_vector_Z'low + 1) := (others => '0');
          -- Now run the spin
          -- Subtracting from means to toggle the vector, to add them and to add 1
          -- Then for additions, the carry in is zero for the first arithm bloc
          -- Then for subtractions, the carry in is one for the first arithm bloc
          -- Spin according with the mode, see above
          CCWCW_IF : if CCW_not_CW = '1' then
            if is_first = '1' then
              carry_in_vector_X(carry_in_vector_X'low) := '1';
              carry_in_vector_Y(carry_in_vector_Y'low) := '0';
            else
              carry_in_vector_X(carry_in_vector_X'low) := carry_X;
              carry_in_vector_Y(carry_in_vector_Y'low) := carry_Y;
            end if;
            result_X := std_logic_vector(unsigned(op_N_X) + (inverter_mask xor unsigned(op_S_Y)) + unsigned(carry_in_vector_X));
            result_Y := std_logic_vector(unsigned(op_N_Y) + unsigned(op_S_X) + unsigned(carry_in_vector_Y));
            -- angle decreases
            if is_first = '1' then
              carry_in_vector_Z(carry_in_vector_Z'low) := '1';
            else
              carry_in_vector_Z(carry_in_vector_Z'low) := carry_Z;
            end if;
            result_Z := std_logic_vector(unsigned(op_N_Z) + (inverter_mask xor unsigned(op_C_Z)) + unsigned(carry_in_vector_Z));
          else
            -- CW
            if is_first = '1' then
              carry_in_vector_X(carry_in_vector_X'low) := '0';
              carry_in_vector_Y(carry_in_vector_Y'low) := '1';
            else
              carry_in_vector_X(carry_in_vector_X'low) := carry_X;
              carry_in_vector_Y(carry_in_vector_Y'low) := carry_Y;
            end if;
            result_X := std_logic_vector(unsigned(op_N_X) + unsigned(op_S_Y) + unsigned(carry_in_vector_X));
            result_Y := std_logic_vector(unsigned(op_N_Y) + (inverter_mask xor unsigned(op_S_X)) + unsigned(carry_in_vector_Y));
            -- angle increases
            if is_first = '1' then
              carry_in_vector_Z(carry_in_vector_Z'low) := '0';
            else
              carry_in_vector_Z(carry_in_vector_Z'low) := carry_Z;
            end if;
            result_Z := std_logic_vector(unsigned(op_N_Z) + unsigned(op_C_Z) + unsigned(carry_in_vector_Z));

          end if CCWCW_IF;
          -- return all the carries for the next loop
          carry_X <= result_X(result_X'high);
          carry_Y <= result_Y(result_Y'high);
          carry_Z <= result_Z(result_Z'high);
          -- put back, at the top the result
          -- step one: shift msb to LSB
          scz_out_s.the_cos(scz_out_s.the_cos'high - arithm_size downto scz_out_s.the_cos'low) <=
            scz_out_s.the_cos(scz_out_s.the_cos'high downto scz_out_s.the_cos'low + arithm_size);
          scz_out_s.the_sin(scz_out_s.the_sin'high - arithm_size downto scz_out_s.the_sin'low) <=
            scz_out_s.the_sin(scz_out_s.the_sin'high downto scz_out_s.the_sin'low + arithm_size);
          scz_out_s.angle_z(scz_out_s.angle_z'high - arithm_size downto scz_out_s.angle_z'low) <=
            scz_out_s.angle_z(scz_out_s.angle_z'high downto scz_out_s.angle_z'low + arithm_size);
          -- step two: populate with the debug or the result
          scz_out_s.the_cos(scz_out_s.the_cos'high downto scz_out_s.the_cos'high - arithm_size + 1) <=
            result_X(result_X'high - 1 downto result_X'low);
          scz_out_s.the_sin(scz_out_s.the_sin'high downto scz_out_s.the_sin'high - arithm_size + 1) <=
            result_Y(result_Y'high - 1 downto result_Y'low);
          scz_out_s.angle_z(scz_out_s.angle_z'high downto scz_out_s.angle_z'high - arithm_size + 1) <=
            result_Z(result_Z'high - 1 downto result_Z'low);
          is_first <= '0';
        end if REGSYNC_IF;
      else
        Z_shifts_count        <= (others => '0');
        remaining_shift_count <= (others => '0');
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

  Cordic_IntermStage_ShiftSelector_instanc_X : Cordic_IntermStage_ShiftSelector
    generic map (
      arithm_size,
      reg_size,
      shifts_calc,
      extra_shifts
      )
    port map (
      CLK,
      RST,
      reg_sync,
      mask_sign       => mask_register( mask_register'low + shifts_calc + arithm_size - 1 downto
                                        mask_register'low + shifts_calc ),
      sincos_sign     => sign_X,
      sincos_in_slice => scz_in.the_cos(scz_in.the_cos'low + shifts_calc + arithm_size - 1 downto
                                        scz_in.the_cos'low + shifts_calc),

      sincos_inout_slice => scz_out.the_cos(scz_out.the_cos'low + shifts_calc + arithm_size - 1 + extra_shifts - 1 + 1
                                            downto
                                            scz_out.the_cos'low + shifts_calc + 1),

      sincos_out_slice => selected_X_for_arithm
      );

  Cordic_IntermStage_ShiftSelector_instanc_Y : Cordic_IntermStage_ShiftSelector
    generic map (
      arithm_size,
      reg_size,
      shifts_calc,
      extra_shifts
      )
    port map (
      CLK,
      RST,
      reg_sync,
      mask_sign       => mask_register( mask_register'low + shifts_calc + arithm_size - 1 downto
                                        mask_register'low + shifts_calc ),
      sincos_sign     => sign_Y,
      sincos_in_slice => scz_in.the_sin(scz_in.the_sin'low + shifts_calc + arithm_size - 1 downto
                                        scz_in.the_sin'low + shifts_calc),

      sincos_inout_slice => scz_out.the_sin(scz_out.the_sin'low + shifts_calc + arithm_size - 1 + extra_shifts - 1 + 1
                                            downto
                                            scz_out.the_sin'low + shifts_calc + 1),

      sincos_out_slice => selected_Y_for_arithm
      );


  assert false
    report "the_cos( "& integer'image(scz_in.the_cos'low + shifts_calc + arithm_size - 1) &
    " downto " &
    integer'image(scz_in.the_cos'low + shifts_calc) &
    ")"
    severity note;
  assert false
    report "the_cos( "& integer'image(scz_out.the_cos'low + shifts_calc + arithm_size - 1 + extra_shifts - 1 + 1) &
    " downto " &
    integer'image(scz_in.the_cos'low + shifts_calc + 1) &
    ")"
    severity note;

end architecture rtl;

