library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;


--! @brief Cordic Z to 0 first stage
--!
--! Perform a pre-processing and format the data
--!   in order to run the following stages.\n
--! X is provided as a signed data, using a vector
--!   of a length between 4 bits and the register size minus 1
--!   minus 1 in our case.\n
--! This preprocessing has two advantages:\n
--! * reduce the Cordic stages
--! * keep some dynamic, as the algorithm introduces a cosine constant.
--! 1/cos(arc-tan( 2 )) = 1/cos( 63.43 ) = 2.236
--! 1/cos(arc-tan( 1 )) = 1/cos( 45 ) = 1.414
--! 1/cos(arc-tan( 0.5 )) = 1/cos( 26.56 ) = 1.118.
--! With the preprosessing, the numbers does not grow a lot.\n
--! Additional information is in the Python utility Cordic_values_grow.py\n
--! It is not more or less resources consuming to start
--!   the following stages at 1 or 0.5. It helps the second set (Y to 0)
--! Then the choice is 0.5.
--! For this set (Z to 0) a 2 division of the input is enough
--! as the output (of the set) increases by less than 17%.
entity Cordic_FirstStage_Z_to_0 is
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
    --! The size should be not more than the reg_size size minus 1,
    --!   in order to divide by 2 the input
    --!   as Cordic slightly increases the gain (see the Python simulation)
    input_x, input_y :     std_logic_vector;
    --! Output of X, Y and Z.
    scz_out              : out reg_sin_cos_z
    );
end entity Cordic_FirstStage_Z_to_0;

--! @brief preprocess with the choices
--!
--! Rules applies in this order
--! to spin the vector while subtracting the angle from Z
--! PI to 2.PI: the high bit of Z is cleared, X=-X and Y=-Y\n 
--! PI/2 to PI: the high - 1 bit of Z is cleared, X=-Y and Y=X
--! For the PI/4 to PI/2 zone,
--!  the vector has spun CW by PI/2, the (remaining) angle
--!  is between -PI/4 and PI/4.\n
--! That makes 8 zones of PI/4 each, that generates 3 PI/2 possible rotations.
--! There is no risk the angle does not converge to 0
--!   as if the following stages computes all CW or all CCW,
--!   the rotation is bigger than PI/4.
--! The Z is transmitted while filling up the 3 high bits
--! using the high - 2 bit of the input Z.
architecture rtl of Cordic_FirstStage_Z_to_0 is
  signal scz_local : reg_sin_cos_z;
  signal z_3_high_bits : std_logic_vector(2 downto 0);
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


  reg_sync_out <= reg_sync_in;
  
  main_proc : process(CLK)
    variable temp_reg_X, temp_reg_Y : reg_type;
    variable high_bits_Z            : std_logic_vector(2 downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync_in = '1' then
        meta_data_out <= meta_data_in;
        -- Sync, parallel load the input registers
        --
        -- Load the registers X and Y, while formatting
        -- Duplicate the high bit, in order to divide by 2
        scz_local.the_cos(scz_local.the_cos'high) <= input_x(input_x'high);
        scz_local.the_cos(scz_local.the_cos'high - 1 downto
                          scz_local.the_cos'high - 1 - input_x'length + 1) <= input_x;
        if scz_local.the_cos'length > (input_x'length + 1) then
          -- If input_x is short, duplicate the high bits,
          --   in order to keep the rail to rail
-- for loop
-- end loop
          assert false report "TODO" severity failure;
        end if;
        -- Duplicate the high bit, in order to divide by 2
        scz_local.the_sin(scz_local.the_sin'high) <= input_y(input_y'high);
        scz_local.the_sin(scz_local.the_sin'high - 1 downto
                          scz_local.the_sin'high - 1 - input_y'length + 1) <= input_y;
        if scz_local.the_sin'length > (input_y'length + 1) then
          -- If input_x is short, duplicate the high bits,
          --   in order to keep the rail to rail
-- for loop
-- end loop
          assert false report "TODO" severity failure;
        end if;

        -- Value Z is set during the reg_sync
        -- It is not exposed outside.
        -- The relevant bit is stored separately
        scz_local.angle_z(scz_local.angle_z'high - 3 downto scz_local.angle_z'low) <=
          angle_z(angle_z'high - 3 downto angle_z'low);
        scz_local.angle_z(scz_local.angle_z'high downto scz_local.angle_z'high - 2 ) <=
          (others => angle_z(angle_z'high - 2 ));
        
        z_3_high_bits <= angle_z(angle_z'high downto angle_z'high -2);

        -- Perhaps this is updated soon if a full negation is implemented
        -- The mathematics says to negate a number, using the 2' complement,
        -- the bits should be inverted and 1 (0..01) should be added.
        -- The addition is not performed for resources and latency reasons.
        -- That introduces an a very small error.
        -- To minimize it, the size of the registers can be raised
        -- X and Y

      else
        -- Run the shifts on the inputs by the arithmetic size
        scz_local.the_cos(scz_local.the_cos'high - arithm_size downto scz_local.the_cos'low) <=
          scz_local.the_cos(scz_local.the_cos'high downto scz_local.the_cos'low + arithm_size);
        scz_local.the_sin(scz_local.the_sin'high - arithm_size downto scz_local.the_sin'low) <=
          scz_local.the_sin(scz_local.the_sin'high downto scz_local.the_sin'low + arithm_size);
        scz_local.angle_z(scz_local.angle_z'high - arithm_size downto scz_local.angle_z'low) <=
          scz_local.angle_z(scz_local.angle_z'high downto scz_local.angle_z'low + arithm_size);

        -- Move the inputs to the out scz according the placements of the vectors
        --
        -- Case 1: bit high - 2 is 0.
        -- Z is in [ 0 PI/4 ] or [ PI/2 3PI/4 ] or[ PI 5PI/4 ] or[ 3PI/2 7PI/4 ],
        --   that means after one of the 4 axles.
        -- X and Y are spun by N time PI/2 accordingly.
        -- The two high bits are cleared, in order to set Z in [ 0 PI/4 ].
        --
        -- Case 2: bit high - 2 is 1
        -- Z is in [ PI/4 PI/2 ] or [ 3PI/4 PI ] or[ 5PI/4 3PI/2 ] or[ 7PI/4 2PI ],
        --   that means before one of the 4 axles.
        -- X and Y are spun by N time PI/2 accordingly.
        -- The two high bits are set, in order to set Z in [ 7PI/4 2PI ].
        --
        -- Since data is spun only by PI/4 multiples,
        --   the Z low bits are copied as them.
        QUADRANT_PI : case z_3_high_bits is
          when "111" | "000" =>         -- nothing to be done
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
              scz_local.the_cos(scz_local.the_cos'low + arithm_size - 1 downto scz_local.the_cos'low );
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
              scz_local.the_sin(scz_local.the_sin'low + arithm_size - 1 downto scz_local.the_sin'low );
          when "001" | "010" =>         -- spin by PI/2
         --   shift_reg_x <= not temp_reg_y;
         --   shift_reg_y <= temp_reg_x;
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <= not
              scz_local.the_sin(scz_local.the_sin'low + arithm_size - 1 downto scz_local.the_sin'low );
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
              scz_local.the_cos(scz_local.the_cos'low + arithm_size - 1 downto scz_local.the_cos'low );
          when "011" | "100" =>         -- spin by PI
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <= not
              scz_local.the_cos(scz_local.the_cos'low + arithm_size - 1 downto scz_local.the_cos'low );
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <= not
              scz_local.the_sin(scz_local.the_sin'low + arithm_size - 1 downto scz_local.the_sin'low );
          when "101" | "110" =>         -- spin by 3.PI/2
          --  shift_reg_x <= temp_reg_y;
          --  shift_reg_y <= not temp_reg_x;
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
              scz_local.the_sin(scz_local.the_sin'low + arithm_size - 1 downto scz_local.the_sin'low );
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <= not
              scz_local.the_cos(scz_local.the_cos'low + arithm_size - 1 downto scz_local.the_cos'low );
          when others => null;
        end case QUADRANT_PI;
        scz_out.angle_z(scz_out.angle_z'high downto scz_out.angle_z'high - arithm_size + 1) <= not
          scz_local.angle_z(scz_local.angle_z'low + arithm_size - 1 downto scz_local.angle_z'low );

        -- Run the shifts by the arithmetic size
        scz_out.the_cos(scz_out.the_cos'high - arithm_size downto scz_out.the_cos'low) <=
          scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'low + arithm_size);
        scz_out.the_sin(scz_out.the_sin'high - arithm_size downto scz_out.the_sin'low) <=
          scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'low + arithm_size);
        scz_out.angle_z(scz_out.angle_z'high - arithm_size downto scz_out.angle_z'low) <=
          scz_out.angle_z(scz_out.angle_z'high downto scz_out.angle_z'low + arithm_size);
      end if REGSYNC_IF;

    end if CLK_IF;
  end process main_proc;
  
end architecture rtl;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Cordic last stage for test
--!
--! 
entity Cordic_LastStage_4_test is
  generic (
    Z_not_Y_2_0    : boolean
    );
  port (
    CLK            : in  std_logic;
    RST            : in  std_logic;
    reg_sync       : in  std_logic;
    scz_in         : in  reg_sin_cos_z;
    X_out          : out reg_type;
    Y_out          : out reg_type;
    Z_out          : out reg_type;
    error_exponent : out std_logic_vector(5 downto 0)
    );
end entity Cordic_LastStage_4_test;

architecture rtl of Cordic_LastStage_4_test is
  signal is_negative        : std_logic;
  signal still_ok           : std_logic;
  signal error_exponent_s : std_logic_vector(5 downto 0);
  -- we are looking for the first one (positive number )
  -- or the first zero ( negative number )
  -- then we are shifting from LSB to MSB
  -- then we need a separate register;
  signal value_working      : std_logic_vector(scz_in.angle_z'range);
begin
  assert arithm_size = 1 report "It has not yet been tested for arithm_size (" &
    integer'image(arithm_size) & ") other than 1" severity warning;
  assert 2**error_exponent'length > reg_size
    report "Internal error 2 power Z_error_exponent (" &
    integer'image(error_exponent'length) &
    ") should be greater than the register size ("&
    integer'image(reg_size)
    severity failure;


  main_proc : process(CLK)
    variable error_exponent_v : unsigned(error_exponent'range);
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        if Z_not_Y_2_0 then
          is_negative        <= scz_in.angle_z(scz_in.angle_z'high);
          value_working      <= scz_in.angle_z;
        else
          is_negative        <= scz_in.the_sin(scz_in.the_sin'high);
          value_working      <= scz_in.the_sin;
        end if;
        still_ok           <= '1';
        -- load others=>1 into the counter
        error_exponent_s <= (others => '1');
        error_exponent   <= error_exponent_s;
        -- Take a photo of the registers
        X_out <= scz_in.the_cos;
        Y_out <= scz_in.the_sin;
        Z_out <= scz_in.angle_z;
      else
        -- decrease the counter as long as the bits are
        -- the same as the sign bit
        if still_ok = '1' then
          error_exponent_v := unsigned(error_exponent_s);
          for ind in 1 to arithm_size loop
            if value_working(value_working'high - ind + 1) /= is_negative then
              --  If not, the counter freezes until the next reg_sync
              still_ok <= '0';
              exit;
            else
              error_exponent_v := error_exponent_v - 1;
            end if;
          end loop;
          error_exponent_s <= std_logic_vector(error_exponent_v);
        end if;


        value_working(value_working'high downto value_working'low + arithm_size) <=
          value_working(value_working'high - arithm_size downto value_working'low);
        -- If the FPGA is a couple a gates too short, comment this line out
        -- The error that would have been reported is even lower
        -- than the rounding errors
        value_working(value_working'low + arithm_size - 1 downto value_working'low) <=
          (others => is_negative);
      end if REGSYNC_IF;
    end if CLK_IF;
  end process main_proc;
end architecture rtl;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Cordic Y to 0 first stage
--!
--! Perform a pre-processing and format the data
--!   in order to run the stages.\n
--! X and Y are both provided as a signed data, using a vector
--!   of a length reg_size.\n
--! The advantages of a preprocessing is explained
--!   in the Cordic_FirstStage_Z_2_0 entity documentation.\n
--! Additional information is in the Python utility Cordic_values_grow.py\n
--! Due to the choices of the first set of Cordic stages,
--!   the start at tan=1 would have been logic.
--! The grow from an input is about 92%. For an input at 0.5,
--!   the output does not overflow.
--! However:
--! * The verification tool is more tricky.
--!   The maximum input of many groups of stages should fit with the previous group.
--!   Then, there is a step verifying the verification "tool".
--! * There is a potential overflow.
--!   supplying the first stage with 1.1644 with an input at 0.5 is Ok.
--!   The required level is not more than 1.3333.
--!   The output is 1.6468.
--!   The input of the next stage should not be more 1.5999..99
--! * That can provide an amplitude margin
--!   in case the filters modules require it (that needs to re-validate).
entity Cordic_FirstStage_Y_to_0 is
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

--! @brief preprocess with the choices
--!
--! The first stage check sequentially which one is greater than.
--! It copies X and Y to the scz_12 as them.
--! The sign of X and Y are only used to run the compare.\n
--! The second stage set X and Y to the scz_out from X or -X and Y or -Y.\n
--! The compare is done on the high-1 bits, using unsigned.
--! This data is passed to the second stage.
--! During the reg_sync,
--! the second stage checks the sign of X, the sign of Y and the greater than.\n
--! TODO specific VERIFICATION
--! There are some approximations, due to the computation of negation of the numbers.
--! The bits are toggled but not +1 addition is performed.
--! The X<Y may be slightly incorrect. However, in the utility Cordic_values_grow.py
--!   there is a simulation of all the Cordic stages spinning CW or CCW.
--! A vector at an angle of slightly more than PI/4 can converge to 0,
--!   without the stage PI/4 tan=1.\n
--! This should be verified for low number of bits and/or high arithm_size and/or
--!   low amplitude signals.
--! It does not generate any overflow but only some low level noise.
architecture rtl of Cordic_FirstStage_Y_to_0 is
  --! Data latched for the way to compare X and Y and for the second stage
  signal xy_in_is_neg         : std_logic_vector(1 downto 0);
  --! Action computed for the second stage
  signal quadrant_to_rotate   : std_logic_vector(1 downto 0);
  --! Incremental X Y compare result, if, at one level, X = Y, this carry is used
  signal y_gt_x               : std_logic;
  --! Intermediary latch to forward the meta data after 2 syncs, rather than 1
  signal meta_data_12 : meta_data_t;
  --! Stage 1 is between scz_in and scz_12. Stage 2 is between scz_12 and scz_out
  signal scz_12               : reg_sin_cos_z;
  --! Some Debug variables, subject to be removed
  signal debug_catch_X_sync, debug_catch_Y_sync : reg_type;
  signal debug_catch_Z_sync      : reg_type;
  signal debug_catch_rot_sync : std_logic_vector(1 downto 0);
begin

  assert (reg_size - 3) mod arithm_size = 0 report " the register size (" & integer'image(arithm_size) &
    ") minus 3 should be a multiple of the arithmetic size (" & integer'image(arithm_size) & ")" severity failure;


  proc_first_stage : process(CLK)
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        -- Stages 1 and 2 are handled here together
        -- 1 Y, 0 X
        xy_in_is_neg(1)                    <= scz_in.the_cos(scz_in.the_cos'high);
        xy_in_is_neg(0)                    <= scz_in.the_sin(scz_in.the_sin'high);
        
        y_gt_x <= 'X';
        -- If the values are equal, the angle is PI/4 modulo PI/2
        meta_data_12 <= meta_data_in;
      else
        -- Check which one has the greater absolute value
        greater_by_quadrant : case xy_in_is_neg is
          when "00" =>
            if unsigned( scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) >
              unsigned( scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) then
              y_gt_x <= '0';
            elsif unsigned( scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) >
              unsigned( scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) then
              y_gt_x <= '1';
              -- else keep y_gt_x
            end if;
          when "01" =>
            if unsigned( scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) >
              unsigned( not scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) then
              y_gt_x <= '0';
            elsif unsigned( not scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) >
              unsigned( scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) then
              y_gt_x <= '1';
              -- else keep y_gt_x
            end if;
          when "10" =>
            if unsigned(not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) >
              unsigned(scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) then
              y_gt_x <= '0';
            elsif unsigned( scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) >
              unsigned( not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) then
              y_gt_x <= '1';
              -- else keep y_gt_x
            end if;
          when "11" =>
            if unsigned( not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) >
              unsigned( not scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) then
              y_gt_x <= '0';
            elsif unsigned( not scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low)) >
              unsigned( not scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) then
              y_gt_x <= '1';
              -- else keep y_gt_x
            end if;
            when others => NULL;
        end case greater_by_quadrant;
        --shift the scz_in into scz_12 (no Z)
        scz_12.the_cos(scz_12.the_cos'high downto scz_12.the_cos'high - arithm_size + 1) <=
          scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
        scz_12.the_sin(scz_12.the_sin'high downto scz_12.the_sin'high - arithm_size + 1) <=
          scz_in.the_sin(scz_in.the_sin'low + arithm_size - 1 downto scz_in.the_sin'low);        
        --shift the rest of the registers (no Z)
        scz_12.the_cos(scz_12.the_cos'high - arithm_size downto scz_12.the_cos'low) <=
          scz_12.the_cos(scz_12.the_cos'high downto scz_12.the_cos'low + arithm_size);
        scz_12.the_sin(scz_12.the_sin'high - arithm_size downto scz_12.the_sin'low) <=
          scz_12.the_sin(scz_12.the_sin'high downto scz_12.the_sin'low + arithm_size);
      end if REGSYNC_IF;
    end if CLK_IF;
  end process proc_first_stage;

  
  proc_second_stage : process(CLK)
    variable xy_in_is_neg_y_gt_x_v : std_logic_vector(2 downto 0);
    variable quadrant_to_rotate_v : std_logic_vector( 1 downto 0 );
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        meta_data_out                                                      <= meta_data_12;

        xy_in_is_neg_y_gt_x_v(0) := y_gt_x;
        xy_in_is_neg_y_gt_x_v(2 downto 1) := xy_in_is_neg;
        case xy_in_is_neg_y_gt_x_v is
          when "000" =>
            quadrant_to_rotate_v := "00";
          when "001" =>
            quadrant_to_rotate_v := "01";
          when "101" =>
            quadrant_to_rotate_v := "01";
          when "100" =>
            quadrant_to_rotate_v := "10";
          when "110" =>
            quadrant_to_rotate_v := "10";
          when "111" =>
            quadrant_to_rotate_v := "11";
          when "011" =>
            quadrant_to_rotate_v := "11";
          when "010" =>
            quadrant_to_rotate_v := "00";
            when others =>
        end case;
        
        quadrant_to_rotate <= quadrant_to_rotate_v;

        -- Value Z is set during the reg_sync
        -- The next stage depend on Y only, not Z
        scz_out.angle_z( scz_out.angle_z'high downto scz_out.angle_z'high - 1 ) <= quadrant_to_rotate_v;
        scz_out.angle_z( scz_out.angle_z'high - 2 downto scz_out.angle_z'low ) <= ( others => '0' );

        debug_catch_X_sync <= scz_out.the_cos;
        debug_catch_Y_sync <= scz_out.the_sin;
        debug_catch_Z_sync <= scz_out.angle_z;
        debug_catch_rot_sync <= quadrant_to_rotate_v;
      else
        xy1_quadrant : case quadrant_to_rotate is
          when "00" =>
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
              scz_12.the_cos(scz_12.the_cos'low + arithm_size - 1 downto scz_12.the_cos'low);
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
              scz_12.the_sin(scz_12.the_sin'low + arithm_size - 1 downto scz_12.the_sin'low);
          when "01" =>
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
              scz_12.the_sin(scz_12.the_sin'low + arithm_size - 1 downto scz_12.the_sin'low);
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
              not scz_12.the_cos(scz_12.the_cos'low + arithm_size - 1 downto scz_12.the_cos'low);
          when "10" =>
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
              not scz_12.the_cos(scz_12.the_cos'low + arithm_size - 1 downto scz_12.the_cos'low);
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
              not scz_12.the_sin(scz_12.the_sin'low + arithm_size - 1 downto scz_12.the_sin'low);
          when "11" =>
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
              not scz_12.the_sin(scz_12.the_sin'low + arithm_size - 1 downto scz_12.the_sin'low);
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
              scz_12.the_cos(scz_12.the_cos'low + arithm_size - 1 downto scz_12.the_cos'low);
          when others => null;
        end case xy1_quadrant;
        --shift the rest of the registers
        scz_out.the_cos(scz_out.the_cos'high - arithm_size downto scz_out.the_cos'low) <=
          scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'low + arithm_size);
        scz_out.the_sin(scz_out.the_sin'high - arithm_size downto scz_out.the_sin'low) <=
          scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'low + arithm_size);
        scz_out.angle_z(scz_out.angle_z'high - arithm_size downto scz_out.angle_z'low) <=
          scz_out.angle_z(scz_out.angle_z'high downto scz_out.angle_z'low + arithm_size);
      end if REGSYNC_IF;
    end if CLK_IF;
  end process proc_second_stage;

end architecture rtl;
