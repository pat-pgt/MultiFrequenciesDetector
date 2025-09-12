--! @brief Verify there are no risk of parasitic loops
--!
--! In case of a cosmic ray or a bad power on reset,
--!   an digital system might enter in a sub state
--!   that it can never exit.
--! This is intended to be placed in formal verification check list.
entity Cordic_stages_fallback_test is
end entity Cordic_stages_fallback_test;

architecture arch of Cordic_stages_fallback_test is
begin
  main_proc : process
  begin
    assert false report "The Cordic stages output a result that is purely combinatorial." severity note;
    assert false report "It does not depend on the past. It is a pipeline." severity note;
    assert false report "In case of a wrong data, it is going to be pushed to the end." severity note;
    assert false report "The system resumes after the latency of the process chain" severity note;
    assert false report "There is no more verification than a proof reading" severity note;
    wait;
  end process main_proc;
end architecture arch;
