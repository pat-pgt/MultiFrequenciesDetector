library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;


--! @brief Cordic Z to 0 first stage
--!
--! Perform a pre-processing and format the data
--! in order to run the stages.
--! X is provided as a signed data, using a vector
--! of a length between 4 bits and the register size minus 2.
--! The data is left justified to left and the low bits
--! are computed to get a rail to rail input.\n
--! Y is computed in the same way than X.
--! It is used only for tests while providing a PI/2 shifted
--! input.\n
--! Z is provided as an unsigned vector 0 to 2.PI - epsilon.
--! Z is converted into a signed vector PI to -PI.
--! In fact PI/2 to PI and -PI to -PI/2 are never raised.\n
--! Rules applies in this order
--! to spin the vector while subtracting the angle from Z
--! PI to 2.PI: the high bit of Z is cleared, X=-X and Y=-Y\n 
--! PI/2 to PI: the high - 1 bit of Z is cleared, X=-Y and Y=X
--! For the PI/4 to PI/2 zone,
--! the vector has spun by PI/2, the (remaining) angle
--! is taken from the symmetry against PI/2 and negated
--! Then the Z is transmitted while filling up the 3 high bits
--! using the high - 2 bit of the input Z.
--! This preprocessing has two advantages:\n
--! * reduce the Cordic stages
--! * keep some dynamic. The algorithm introduces a cosine constant.
--! 1/cos(arc-tan( 2 )) = 1/cos( 63.43 ) = 2.236
--! 1/cos(arc-tan( 1 )) = 1/cos( 45 ) = 1.414
--! 1/cos(arc-tan( 0.5 )) = 1/cos( 26.56 ) = 1.118.
--! With the preprosessing, the numbers does not grow a lot.
--! Only one (left) additional bit is needed, no bit is wasted.\n
--! For the Z to 0, everything is predictable (at the sync pulse).
--! as it relays on the the three high bits.
--! Three stages are replaced by on stage.
entity Cordic_FirstStage_Z_to_0 is
  generic (
    debug_mode : boolean := false
    );
  port (
    CLK              : in  std_logic;
    RST              : in  std_logic;
    -- Coming from the angle generator,
    -- the flag tells the angle has just changed
    -- For the intermediary stages,
    -- the flag tells the data has stopped in order
    -- to update some signals such as CCW_or_CW
    reg_sync_in      : in  std_logic;
    reg_sync_out     : out std_logic;
    -- unsigned min = 0, max = 2.PI - epsilon
    -- come a serial chunks of arithm_size bit
    angle_z          : in  reg_type;
    meta_data_in     : in  meta_data_t;
    meta_data_out    : out meta_data_t;
    --! signed input vector
    --! The size should be at least 4 to keep the trigonometric properties\n
    --! The size should be not more than the reg_size size minus 1,
    --!   in order to divide by 2 the input
    --!   as Cordic slightly increases the gain (see the Python simulation)
    input_x, input_y :     std_logic_vector;
    --! Output of X, Y and Z.
    scz              : out reg_sin_cos_z
    );
end entity Cordic_FirstStage_Z_to_0;


architecture rtl of Cordic_FirstStage_Z_to_0 is
  signal shift_reg_x, shift_reg_y : reg_type;
  signal shift_reg_z              : reg_type;
begin
  assert input_x'length > 3 report "The size of input_x (" & integer'image(input_x'length) &
    ") should be at least 4" severity failure;
  assert input_y'length > 3 report "The size of input_y (" & integer'image(input_y'length) &
    ") should be at least 4" severity failure;
  assert input_x'length <= reg_size - 1 report "The size of input_x (" & integer'image(input_x'length) &
                           ") should not be greater than reg_size (" & integer'image(reg_size) & ")" severity failure;
  assert input_y'length <= reg_size - 1 report "The size of input_x (" & integer'image(input_y'length) &
                           ") should not be greater than reg_size (" & integer'image(reg_size) & ")" severity failure;

  assert (reg_size - 3) mod arithm_size = 0 report " the register size (" & integer'image(arithm_size) &
    ") minus 3 should be a multiple of the arithmetic size (" & integer'image(arithm_size) & ")" severity failure;

  -- This implementation looks like horrible for the routing
  -- as both X and Y can take theirs entries or the other one while
  -- inverting or not (separately)
  -- In fact, it is implemented for the tests only
  -- during the synthesis, only X in used. Y is set to others => 0
  -- Then the switch is input X, not input, 0 or last state (nice for LUT-4 FPGA
  
  scz.the_cos <= shift_reg_x;
  scz.the_sin <= shift_reg_y;
  scz.angle_z <= shift_reg_z;

  main_proc : process(CLK)
    variable temp_reg_X, temp_reg_Y : reg_type;
    variable high_bits_Z            : std_logic_vector(2 downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      reg_sync_out <= reg_sync_in;
      REGSYNC_IF : if reg_sync_in = '1' then
        -- Sync, parallel load the input registers
        --
        -- Load the registers X and Y, while formatting
        -- Duplicate the high bit, in order to divide by 2
        temp_reg_x(temp_reg_x'high)                                                     := input_x(input_x'high);
        temp_reg_x(temp_reg_x'high - 1 downto temp_reg_x'high - 1 - input_x'length + 1) := input_x;
        if temp_reg_x'length > (input_x'length + 1) then
          -- If input_x is short, duplicate the high bits,
          --   in order to keep the rail to rail
-- for loop
-- end loop
          assert false report "TODO" severity failure;
        end if;
        -- Duplicate the high bit, in order to divide by 2
        temp_reg_y(temp_reg_y'high)                                                     := input_y(input_y'high);
        temp_reg_y(temp_reg_y'high - 1 downto temp_reg_y'high - 1 - input_y'length + 1) := input_y;
        if temp_reg_y'length > (input_y'length + 1) then
          -- If input_x is short, duplicate the high bits,
          --   in order to keep the rail to rail
-- for loop
-- end loop
          assert false report "TODO" severity failure;
        end if;
        high_bits_Z := angle_z(angle_z'high downto angle_z'high -2);
        -- The mathematics says to negate a number, using the 2' complement,
        -- the bits should be inverted and 1 (0..01) should be added.
        -- The addition is not performed for resources and latency reasons.
        -- That introduces an a very small error.
        -- To minimize it, the size of the registers can be raised
        -- X and Y
        QUADRANT_PI : case high_bits_Z is
          when "000" | "111" =>         -- nothing to be done
            shift_reg_x <= temp_reg_x;
            shift_reg_y <= temp_reg_y;
          when "001" | "010" =>         -- spin by PI/2
            shift_reg_x <= not temp_reg_y;
            shift_reg_y <= temp_reg_x;
          when "011" | "100" =>         -- spin by PI
            shift_reg_x <= not temp_reg_x;
            shift_reg_y <= not temp_reg_y;
          when "101" | "110" =>         -- spin by 3.PI/2
            shift_reg_x <= temp_reg_y;
            shift_reg_y <= not temp_reg_x;
          when others => null;
        end case QUADRANT_PI;

        -- Case 1: bit high - 2 is 0.
        -- Regardless the PI/2 quadrant, X and Z was transformed,
        -- then bits high and high - 1 has to be cleared.
        -- Case 2: bit high - 2 is 1
        -- Getting the symmetry from PI/2 means
        -- 010 00..00 - 001 ab..cd -> 000 NaNb..NcNd + 1
        -- Getting the opposite means
        -- 111 ab..cd - 1 + 1
        -- Then the bit high - 2 has be be copied into high and high - 1,
        -- nothing else.
        shift_reg_z(shift_reg_z'high - 3 downto shift_reg_z'low)  <= angle_z(angle_z'high - 3 downto angle_z'low);
        shift_reg_z(shift_reg_z'high downto shift_reg_z'high - 2) <= (others => angle_z(angle_z'high - 2));
        -- ... and transfer the MD
        meta_data_out                                             <= meta_data_in;
      else
        -- Run the shifts by the arithmetic size
        shift_reg_x(shift_reg_x'high - arithm_size downto shift_reg_x'low) <=
          shift_reg_x(shift_reg_x'high downto shift_reg_x'low + arithm_size);
        shift_reg_y(shift_reg_y'high - arithm_size downto shift_reg_y'low) <=
          shift_reg_y(shift_reg_y'high downto shift_reg_y'low + arithm_size);
        shift_reg_z(shift_reg_z'high - arithm_size downto shift_reg_z'low) <=
          shift_reg_z(shift_reg_z'high downto shift_reg_z'low + arithm_size);
      end if REGSYNC_IF;

    end if CLK_IF;
  end process main_proc;
  
end architecture rtl;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Cordic Z to 0 last stage
--!
--! Performs some sanity checks on Z.
--! No process is needed on X and Y as
--! the prefilter uses the serial data.\n
--! The checks on Z are for the tests.
--! They can, however, be synthesised
--! as some tests can be done using
--! FPGA (with specific input and
--! output file).
entity Cordic_LastStage_Z_to_0 is
  port (
    CLK              : in  std_logic;
    RST              : in  std_logic;
    reg_sync         : in  std_logic;
    scz_in           : in  reg_sin_cos_z;
    Z_error_exponent : out std_logic_vector(5 downto 0)
    );
end entity Cordic_LastStage_Z_to_0;

architecture rtl of Cordic_LastStage_Z_to_0 is
  signal is_negative        : std_logic;
  signal still_ok           : std_logic;
  signal Z_error_exponent_s : std_logic_vector(5 downto 0);
  -- we are looking for the first one (positive number )
  -- or the first zero ( negative number )
  -- then we are shifting from LSB to MSB
  -- then we need a separate register;
  signal angle_working      : std_logic_vector(scz_in.angle_z'range);
begin
  assert arithm_size = 1 report "It has not yet been tested for arithm_size (" &
    integer'image(arithm_size) & ") other than 1" severity warning;
  assert 2**Z_error_exponent'length > reg_size
    report "Internal error 2 power Z_error_exponent (" &
    integer'image(Z_error_exponent'length) &
    ") should be greater than the register size ("&
    integer'image(reg_size)
    severity failure;


  main_proc : process(CLK)
    variable Z_error_exponent_v : unsigned(Z_error_exponent'range);
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        -- load others=>1 into the counter
        is_negative        <= scz_in.angle_z(scz_in.angle_z'high);
        still_ok           <= '1';
        Z_error_exponent_s <= (others => '1');
        angle_working      <= scz_in.angle_z;
        Z_error_exponent   <= Z_error_exponent_s;
      else
        -- decrease the counter as long as the bits are
        -- the same as the sign bit
        if still_ok = '1' then
          Z_error_exponent_v := unsigned(Z_error_exponent_s);
          for ind in 1 to arithm_size loop
            if angle_working(angle_working'high - ind + 1) /= is_negative then
              --  If not, the counter freezes until the next reg_sync
              still_ok <= '0';
              exit;
            else
              Z_error_exponent_v := Z_error_exponent_v - 1;
            end if;
          end loop;
          Z_error_exponent_s <= std_logic_vector(Z_error_exponent_v);
        end if;


        angle_working(angle_working'high downto angle_working'low + arithm_size) <=
          angle_working(angle_working'high - arithm_size downto angle_working'low);
        -- If the FPGA is a couple a gates too short, comment this line out
        -- The error that would have been reported is even lower
        -- than the rounding errors
        angle_working(angle_working'low + arithm_size - 1 downto angle_working'low) <=
          (others => is_negative);
      end if REGSYNC_IF;
    end if CLK_IF;
  end process main_proc;
end architecture rtl;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Cordic Y to 0 first stage
--!
--! Perform a pre-processing and format the data
--! in order to run the stages.
--! X, Y and Z are provided
--! This preprocessing has two advantages:\n
--! * reduce the Cordic stages
--! * keep some dynamic. The algorithm introduces a cosine constant.
--! 1/cos(arc-tan( 2 )) = 1/cos( 63.43 ) = 2.236
--! 1/cos(arc-tan( 1 )) = 1/cos( 45 ) = 1.414
--! 1/cos(arc-tan( 0.5 )) = 1/cos( 26.56 ) = 1.118.
--! With the preprosessing, the numbers does not grow a lot.
--! The Y to 0 is supposed to be used after the Z to 0.
--! The total cumulative cos product is about 1.17.
--! Applying a second time cordic stages, that makes 1.3689 ( < 2 )\n
--! The signs of X and Y are predictable (at the syn pulse).
--! The greater-than needs a stage run.
--! The toggle needs a stage run after the compare completed.\n
--! Three Cordic stages are replaced by two preprocess stages.\n
--! Normaly, the negation of a 2'nd complement is to invert and to add 1
--! omitting the 1 addition makes a small rounding error
--! Since the resources for that purpose are large,
--! it is better to increase the register size. 
entity Cordic_FirstStage_Y_to_0 is
  generic (
    debug_mode : boolean := false
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    -- For the first and the intermediary stages,
    -- the flag tells the data has stopped in order
    -- to update some signals such as CCW_or_CW
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    --! Input of X, Y and Z.
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Cordic_FirstStage_Y_to_0;

architecture rtl of Cordic_FirstStage_Y_to_0 is
  --! Since, it is the first stage, z does not come from the scz structure
  signal shift_reg_z          : reg_type;
  --! As other stages, data about the action, that are stable between syncs
  signal xy1_are_neg          : std_logic_vector(1 downto 0);
  --! Incremental X Y compare result
  signal y_gt_x_loop          : std_logic;
  --! Result of the X Y compare for the stage 2
  signal y_gt_x_2             : std_logic;
  --! Intermediary latch to forward the meta data after 2 syncs, rather than 1
  signal meta_data_out_1_in_2 : meta_data_t;
  --! Stage 1 is between scz_in and scz_12. Stage 2 is between scz_12 and scz_out
  signal scz_12               : reg_sin_cos_z;
  --! For the old VHDL versions and or compilers
  signal scz_out_s            : reg_sin_cos_z;
  --! Z "offset" in stage 2
  signal z_add_stage_2        : reg_type;
  signal carry_Z              : std_logic;
  signal debug_catch_X_sync, debug_catch_Y_sync : reg_type;
  signal debug_catch_Z_sync      : reg_type;
begin

  assert (reg_size - 3) mod arithm_size = 0 report " the register size (" & integer'image(arithm_size) &
    ") minus 3 should be a multiple of the arithmetic size (" & integer'image(arithm_size) & ")" severity failure;

  scz_out <= scz_out_s;

  main_proc : process(CLK)
    variable temp_reg_X, temp_reg_Y : reg_type;
    variable high_bits_Z            : std_logic_vector(2 downto 0);
    variable result_X, result_Y     : std_logic_vector(arithm_size - 1 downto 0);
    variable result_Z_1               : std_logic_vector(arithm_size - 1 downto 0);
    variable result_Z_2               : std_logic_vector(arithm_size downto 0);
    variable y_gt_x_var             : std_logic;
    variable op_N_Z, op_C_Z         : std_logic_vector(arithm_size downto 0);
    variable carry_in_vector_Z      : std_logic_vector(arithm_size downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        -- Stages 1 and 2 are handled here together
        -- 1 Y, 0 X
        xy1_are_neg(0)                    <= scz_in.the_cos(scz_in.the_cos'high);
        xy1_are_neg(1)                    <= scz_in.the_sin(scz_in.the_sin'high);
        shift_reg_z(shift_reg_z'high)     <= scz_in.the_sin(scz_in.the_sin'high);
        shift_reg_z(shift_reg_z'high - 1) <= scz_in.the_cos(scz_in.the_cos'high) xor
                                             scz_in.the_sin(scz_in.the_sin'high);
        y_gt_x_2                                                        <= y_gt_x_loop;
        -- probabely never used, set to 0 in case the numbers are absolutely equal
        y_gt_x_loop                                                     <= '0';
        -- An intermediary latch of the meta data
        meta_data_out_1_in_2                                            <= meta_data_in;
        meta_data_out                                                   <= meta_data_out_1_in_2;
        -- Set the angle stage 2 to add
        z_add_stage_2(z_add_stage_2'high downto z_add_stage_2'high - 1) <= (others => y_gt_x_loop);
        z_add_stage_2(z_add_stage_2'high - 2 downto z_add_stage_2'low)  <= (others => '0');

        carry_Z <= '0';
        
        debug_catch_X_sync <= scz_12.the_cos;
        debug_catch_Y_sync <= scz_12.the_sin;
        debug_catch_Z_sync <= scz_12.angle_z;
      else
        -- Run the first stage
        -- It copies the data according with the signs
        xy1_quadrant : case xy1_are_neg is
          when "00" =>
            result_X := scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
            result_Y := scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
          when "01" =>
            result_X := scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
            result_Y := not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
          when "10" =>
            result_X := not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
            result_Y := not scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
          when "11" =>
            result_X := not scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
            result_Y := scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
          when others => null;
        end case xy1_quadrant;
        -- populate the greate than
        -- The comparaisons are done from low to high
        -- if X or Y is greater, force the signal, otherwise left is as it
        y_gt_x_var := y_gt_x_loop;
        for ind in 1 to arithm_size loop
          if result_X(ind - 1) = '1' and result_Y(ind - 1) = '0' then
            y_gt_x_var := '0';
          elsif result_X(ind - 1) = '0' and result_Y(ind - 1) = '1' then
            y_gt_x_var := '1';
          end if;
        end loop;
        y_gt_x_loop <= y_gt_x_var;
        -- Z does not rely on shifts of a previous stage, do it now
        shift_reg_z(shift_reg_z'high - arithm_size downto shift_reg_z'low) <=
          shift_reg_z(shift_reg_z'high downto shift_reg_z'low + arithm_size);
        shift_reg_z(shift_reg_z'low + arithm_size - 1 downto shift_reg_z'low)            <= (others => '0');
        -- place the result into the signals
        scz_12.the_cos(scz_12.the_cos'high downto scz_12.the_cos'high - arithm_size + 1) <= result_X;
        scz_12.the_sin(scz_12.the_sin'high downto scz_12.the_sin'high - arithm_size + 1) <= result_Y;
        -- Z is easy, nothing to do.
        scz_12.angle_z(scz_12.angle_z'high downto scz_12.angle_z'high - arithm_size + 1) <= 
          shift_reg_z(shift_reg_z'low + arithm_size - 1 downto shift_reg_z'low);
        --shift the rest of the registers
        scz_12.the_cos(scz_12.the_cos'high - arithm_size downto scz_12.the_cos'low) <=
          scz_12.the_cos(scz_12.the_cos'high downto scz_12.the_cos'low + arithm_size);
        scz_12.the_sin(scz_12.the_sin'high - arithm_size downto scz_12.the_sin'low) <=
          scz_12.the_sin(scz_12.the_sin'high downto scz_12.the_sin'low + arithm_size);
        scz_12.angle_z(scz_12.angle_z'high - arithm_size downto scz_12.angle_z'low) <=
          scz_12.angle_z(scz_12.angle_z'high downto scz_12.angle_z'low + arithm_size);
        --
        -- Run the second stage
        --
        -- prepare the Z calculatiuon
        carry_in_vector_Z(carry_in_vector_Z'high downto carry_in_vector_Z'low + 1) := (others => '0');
        carry_in_vector_Z(carry_in_vector_Z'low)                                   := carry_Z;
        op_N_Z(op_N_Z'high)                                                        := '0';
        op_C_Z(op_C_Z'high)                                                        := '0';
        op_N_Z(op_N_Z'high - 1 downto op_N_Z'low) :=
          scz_12.angle_z(scz_12.angle_z'low + arithm_size - 1 downto scz_12.angle_z'low);
        op_C_Z(op_C_Z'high - 1 downto op_C_Z'low) :=
          z_add_stage_2(z_add_stage_2'low + arithm_size - 1 downto z_add_stage_2'low);
        --- ... nd make its shift
        z_add_stage_2(z_add_stage_2'high - arithm_size downto z_add_stage_2'low) <=
          z_add_stage_2(z_add_stage_2'high downto z_add_stage_2'low + arithm_size );
        -- Do it
        result_Z_2 := std_logic_vector(unsigned(op_N_Z) + unsigned(op_C_Z) + unsigned(carry_in_vector_Z));
        carry_Z <= result_Z_2(result_Z_2'high);


        if y_gt_x_2 = '0' then
          result_X := scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
          result_Y := scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
        else
          result_X := scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);
          result_Y := not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
        end if;
        -- place the result into the signals
        scz_out_s.the_cos(scz_out_s.the_cos'high downto scz_out_s.the_cos'high - arithm_size + 1) <= result_X;
        scz_out_s.the_sin(scz_out_s.the_sin'high downto scz_out_s.the_sin'high - arithm_size + 1) <= result_Y;
        scz_out_s.angle_z(scz_out_s.angle_z'high downto scz_out_s.angle_z'high - arithm_size + 1) <=
          result_Z_2( result_Z_2'high - 1 downto result_Z_2'low );
        --shift the rest of the registers
        scz_out_s.the_cos(scz_out_s.the_cos'high - arithm_size downto scz_out_s.the_cos'low) <=
          scz_out_s.the_cos(scz_out_s.the_cos'high downto scz_out_s.the_cos'low + arithm_size);
        scz_out_s.the_sin(scz_out_s.the_sin'high - arithm_size downto scz_out_s.the_sin'low) <=
          scz_out_s.the_sin(scz_out_s.the_sin'high downto scz_out_s.the_sin'low + arithm_size);
        scz_out_s.angle_z(scz_out_s.angle_z'high - arithm_size downto scz_out_s.angle_z'low) <=
          scz_out_s.angle_z(scz_out_s.angle_z'high downto scz_out_s.angle_z'low + arithm_size);
      end if REGSYNC_IF;
    end if CLK_IF;
  end process main_proc;
end architecture rtl;
