library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.math_real.all,
  work.Meta_data_package.all;

--! @brief Monitor a given meta-data
--!
--! Used with Cordic_Interm_monitor,
--! it register the result for a given meta-data.
entity Cordic_Interm_verif_register is
  generic (
    metadata_catch : meta_data_t);
  port (
    CLK             : in  std_logic;
    RST             : in  std_logic;
    reg_sync        : in  std_logic;
    report_in       : in  std_logic;
    report_out      : out std_logic;
    meta_data_after : in  meta_data_t;
    determinant     : in  real;
    z_diff          : in  real);
end entity Cordic_Interm_verif_register;

architecture arch of Cordic_Interm_verif_register is
  signal determinant_max, determinant_min : real;
  signal z_diff_max, z_diff_min           : real;

begin
  main_proc : process (CLK) is

  begin
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        if metadata_catch = meta_data_after then
          if determinant > determinant_max then
            determinant_max <= determinant;
          end if;
          if determinant < determinant_min then
            determinant_min <= determinant;
          end if;
          if z_diff > z_diff_max then
            z_diff_max <= z_diff;
          end if;
          if z_diff < z_diff_min then
            z_diff_min <= z_diff;
          end if;
        end if;
      else
        determinant_min <= real'high;
        determinant_max <= real'low;
        z_diff_min      <= real'high;
        z_diff_max      <= real'low;
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

  report_proc : process
  begin
    wait until report_in = '1';
    report meta_data_image(metadata_catch) & ":  " & real'image(determinant_min) & "<|det|<" & real'image(determinant_max) & "    " &
      real'image(z_diff_min) & "<|Z|<" & real'image(z_diff_max)
      severity note;
    report_out <= '1';
    wait for 1 ns;
  end process report_proc;
end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.Meta_data_package.all,
  work.MultiFreqDetect_package.all;

--! @brief Monitor a given cordic stage
--!
--! For a given stage, this entity is called
--! to verify the rotation (CW or CCW) of the vector
--! and to verify the addition or subtraction of Z
--! According with the metadata.\n
--! This entity catches the data before and after.\n
--! The determinant is computed with X and Y, its absolute value is kept.
--! The difference of Z is computed, its absolute value is kept.\n
--! The entity passes this data to the register component
--! in order to catch the one related to its metadata
entity Cordic_Interm_monitor is
  generic (
    Z_not_Y_to_0        : boolean;
    stage_num           : positive;
    metadata_catch_list : meta_data_list_t
    );
  port (
    CLK             : in  std_logic;
    RST             : in  std_logic;
    reg_sync        : in  std_logic;
    full_sync       : in  std_logic;
    report_in       : in  std_logic;
    report_out      : out std_logic;
    meta_data_after : in  meta_data_t;
    scz_before      : in  reg_sin_cos_z;
    scz_after       : in  reg_sin_cos_z);
end entity Cordic_Interm_monitor;

architecture arch of Cordic_Interm_monitor is
  signal determinant        : real;
  signal z_diff             : real;
  signal report_array       : std_logic_vector(metadata_catch_list'length downto 0);
  signal reg_sync_out       : std_logic;
  signal meta_data_s        : meta_data_t;
  signal x_before, y_before : real;
  signal z_before           : std_logic_vector(scz_before.angle_z'range);
  signal bad_YZ             : boolean   := false;
  -- It is a little bit arbitrars
  signal delay_start_regist : natural   := 3;
  signal RST_delayed        : std_logic := '1';
  signal full_cycles_count  : natural   := 0;
  -- Since this component is only called once,
  -- it is declared here rather than in a component
  component Cordic_Interm_verif_register is
    generic (
      metadata_catch : meta_data_t);
    port (
      CLK             : in  std_logic;
      RST             : in  std_logic;
      reg_sync        : in  std_logic;
      report_in       : in  std_logic;
      report_out      : out std_logic;
      meta_data_after : in  meta_data_t;
      determinant     : in  real;
      z_diff          : in  real);
  end component Cordic_Interm_verif_register;
  
begin
  report_out <= report_array(report_array'high);

  metadata_components : for ind in 1 to metadata_catch_list'length generate
    verif_register : Cordic_Interm_verif_register
      generic map(
        metadata_catch => metadata_catch_list(metadata_catch_list'low + ind - 1))
      port map(
        CLK             => CLK,
        RST             => RST_delayed,
        reg_sync        => reg_sync_out,
        report_in       => report_array(report_array'low +ind - 1),
        report_out      => report_array(report_array'low +ind),
        meta_data_after => meta_data_s,
        determinant     => determinant,
        z_diff          => z_diff);
  end generate metadata_components;

  main_proc : process(CLK)
    variable x_after, y_after : real;
    variable z_after          : real;
    variable determinant_v    : real;
    -- set one bit more for the diff
    -- One high bit is added to the 2 signed nulbers, they are casted as signed,
    --   the subtraction is performed and the high bit is voided
    variable zb_for_diff      : signed(scz_before.angle_z'length downto 0);
    variable za_for_diff      : signed(scz_after.angle_z'length downto 0);
    variable zs_for_diff      : signed(scz_after.angle_z'length downto 0);
  begin
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        if full_sync = '1' then
          -- Handles the delay RST to stabilized the data
          if delay_start_regist /= 0 then
            delay_start_regist <= delay_start_regist - 1;
          else
            full_cycles_count <= full_cycles_count + 1;
            RST_delayed       <= '0';
          end if;
        end if;
        REG_SYNC_IF : if reg_sync = '1' then
          -- X Y
          -- We normalize to 1 the scale of the vector
          x_before                                                 <= real(to_integer(signed(scz_before.the_cos))) / real(2 ** (scz_before.the_cos'length - 1));
          x_after                                                  := real(to_integer(signed(scz_after.the_cos))) / real(2 ** (scz_after.the_cos'length - 1));
          y_before                                                 <= real(to_integer(signed(scz_before.the_sin))) / real(2 ** (scz_before.the_sin'length - 1));
          y_after                                                  := real(to_integer(signed(scz_after.the_sin))) / real(2 ** (scz_after.the_sin'length - 1));
          -- Compute the determinant
          determinant_v                                            := x_before * y_after - x_after * y_before;
          determinant_v                                            := abs(determinant_v);
          -- TODO divide by the cos(h) to compensate the little gain
          -- "Stores" the result
          determinant                                              <= determinant_v;
          -- Z
          z_before                                                 <= scz_before.angle_z;
          za_for_diff(za_for_diff'high)                            := '0';
          zb_for_diff(zb_for_diff'high)                            := '0';
          za_for_diff(za_for_diff'high - 1 downto za_for_diff'low) := signed(scz_after.angle_z);
          zb_for_diff(zb_for_diff'high - 1 downto zb_for_diff'low) := signed(z_before);
          zs_for_diff                                              := za_for_diff - zb_for_diff;
          z_after                                                  := real(to_integer(signed(scz_after.angle_z))) / real(2 ** (scz_after.angle_z'length - 1));
          z_diff <= abs(real(to_integer(zs_for_diff(zs_for_diff'high - 1 downto zs_for_diff'low))) /
                             real(2 ** (zs_for_diff'length - 2)));      
          if Z_not_Y_to_0 then
            if z_after > 0.25 or z_after < -0.25 then
              bad_YZ <= true;
            end if;
          else
            if y_after > 0.708 or y_after < -0.708 then
              bad_YZ <= true;
            end if;
          end if;
          -- delay of one CLK cycle the reg_sync for the registration component
          reg_sync_out <= '1';
          meta_data_s  <= meta_data_after;
        else
          reg_sync_out <= '0';
        end if REG_SYNC_IF;
      else
        reg_sync_out <= '0';
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

  report_proc : process
    variable the_angle : real;
  begin
    wait until report_in = '1';
    if bad_YZ then
      report "**** Stage " & positive'image(stage_num) & ", WARNING |Z| > PI/4 ****" severity warning;
    else
      the_angle := arctg_2_angle_real( stage_num );
      -- Convert back as the angle was normalized to 1.0
      report "Verif. stage " & positive'image(stage_num) &
        ", angle is (degres): " & real'image( the_angle * 360.0 ) & ", " &
        "sin(a): " & real'image( sin( 2.0 * MATH_PI * the_angle )) & " . " &
        natural'image(full_cycles_count) & " full cycles"
        severity note;
    end if;
    report_array(report_array'low) <= '1';
    wait for 1 ns;
  end process report_proc;

end architecture arch;
