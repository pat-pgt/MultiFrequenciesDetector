NETLIST_PROG?=gnetlist
EXPORT_PROG?=gaf
GHDL_PROG=ghdl
GCC_PROG=g++
CXXFLAGS?=-std=c++20 -ffast-math -m64 -O2
VFLAGS?=--std=08
SRCDIR?=
BUILDDIR?=build/
DESTDIR?=
WAVDESTDIR?=/mnt/ramfs/
SYNTHDESTDIR?=Synth/
CXXVERIFDIR?=CXX_VERIF/
YOSYS_PROG?=yosys
NEXTPNR-ICE40_PROG?=nextpnr-ice40
NEXTPNR-ECP5_PROG?=nextpnr-ecp5
ICEPACK?=icepack


$(BUILDDIR)packages_compiled : $(SCRDIR)Utils_package.vhdl $(SCRDIR)InterModule_package.vhdl $(SCRDIR)Metadata_package.vhdl $(SCRDIR)Frequencies_package.vhdl $(SCRDIR)Input_package.vhdl $(SCRDIR)MFD_package.vhdl $(SCRDIR)DataMonitor_package.vhdl $(SCRDIR)Cordic_package.vhdl $(SCRDIR)Prefilter_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Utils_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)InterModule_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Metadata_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Frequencies_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Input_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)MFD_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)DataMonitor_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_package.vhdl
	touch $(BUILDDIR)packages_compiled


simul_AnglesGene : $(BUILDDIR)packages_compiled $(SCRDIR)AnglesGene.vhdl $(SCRDIR)DataMonitor.vhdl $(SCRDIR)AnglesGene_test.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)AnglesGene_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) AngleGene_test
	$(GHDL_PROG) -r $(VFLAGS) AngleGene_test --vcd=$(WAVDESTDIR)AngleGene.wav 2>&1 | tee $(DESTDIR)AnglesGene.out.txt
	echo "The wav file is NOT an audio file, it can be opened, for instance, using gtk-wave" > $(WAVDESTDIR)README.important


simul_Cordic_E2E_DC_spin_angles : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl $(SCRDIR)Cordic_E2E_DC_test.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_IO_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Interm_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Bundle_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Monitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_bundle.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) Cordic_bundle_test_Z_to_0_Y_to_0
	@echo "$$ version 1.1" > $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/angle_gene_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_first_stage_Z_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_bundle_Z_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel	
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_first_stage_Y_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_bundle_Y_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	$(GHDL_PROG) -r $(VFLAGS) Cordic_bundle_test_Z_to_0_Y_to_0 -gnbre_Y_2_0_stages=14 -gnbre_Z_2_0_stages=16 -gwith_4_angles_test=false -gwith_angles_vs_MD=true --vcd=$(WAVDESTDIR)Cordic_E2E_dc_spin_angles.wav --read-wave-opt=$(BUILDDIR)GHDL_wave_opt_toplevel 2>&1 | tee $(DESTDIR)Cordic.out.txt
	echo "The wav file is ***NOT*** an audio file, it can be opened, for instance, using gtk-wave" > $(WAVDESTDIR)README.important

simul_Cordic_E2E_DC_four_inits : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl $(SCRDIR)Cordic_E2E_DC_test.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_IO_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Interm_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Bundle_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Monitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_bundle.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) Cordic_bundle_test_Z_to_0_Y_to_0
	@echo "$$ version 1.1" > $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/three_other_angles/cordic_e2e_dc_bundle_instanc_090/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/three_other_angles/cordic_e2e_dc_bundle_instanc_180/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/three_other_angles/cordic_e2e_dc_bundle_instanc_240/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/angle_gene_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_first_stage_Z_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_bundle_Z_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel	
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_first_stage_Y_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	@echo "/Cordic_bundle_test_Z_to_0_Y_to_0/cordic_e2e_dc_bundle_instanc_000/cordic_bundle_Y_2_0_instanc/*" >> $(BUILDDIR)GHDL_wave_opt_toplevel
	$(GHDL_PROG) -r $(VFLAGS) Cordic_bundle_test_Z_to_0_Y_to_0 -gnbre_Y_2_0_stages=14 -gnbre_Z_2_0_stages=16 -gwith_4_angles_test=true -gwith_angles_vs_MD=false --vcd=$(WAVDESTDIR)Cordic_E2E_DC_four_inits.wav --read-wave-opt=$(BUILDDIR)GHDL_wave_opt_toplevel 2>&1 | tee $(DESTDIR)Cordic.out.txt
	echo "The wav file is ***NOT*** an audio file, it can be opened, for instance, using gtk-wave" > $(WAVDESTDIR)README.important

$(BUILDDIR)Cordic_E2E_DC_CXX_compiled : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl $(SCRDIR)Downsampling_package.vhdl $(SCRDIR)Downsampling.vhdl $(SCRDIR)Cordic_E2E_DC_CXX_test.vhdl $(SCRDIR)Cordic_E2E_DC_bundle.vhdl 
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_IO_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Interm_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Bundle_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Monitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Downsampling_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Downsampling.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_bundle.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_CXX_test.vhdl
	touch $(BUILDDIR)Cordic_E2E_DC_CXX_compiled

$(SYNTHDESTDIR)Cordic_E2E_DC_CXX_noDS.cpp : $(BUILDDIR)Cordic_E2E_DC_CXX_compiled
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) -gwith_downsampling=0 Cordic_E2E_DC_CXX_test; write_cxxrtl  -namespace Cordic_E2E_DC_0 $(SYNTHDESTDIR)Cordic_E2E_DC_CXX_noDS.cpp' 2>&1 |tee $(SYNTHDESTDIR)Cordic_E2E_DC_0.synth.out.txt

$(SYNTHDESTDIR)Cordic_E2E_DC_CXX_DS1.cpp : $(BUILDDIR)Cordic_E2E_DC_CXX_compiled
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) -gwith_downsampling=1 -gextra_shifts=2 Cordic_E2E_DC_CXX_test; write_cxxrtl  -namespace Cordic_E2E_DC_1 $(SYNTHDESTDIR)Cordic_E2E_DC_CXX_DS1.cpp' 2>&1 |tee $(SYNTHDESTDIR)Cordic_E2E_DC_1.synth.out.txt

$(SYNTHDESTDIR)Cordic_E2E_DC_CXX_DS2.cpp : $(BUILDDIR)Cordic_E2E_DC_CXX_compiled
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) -gwith_downsampling=2 Cordic_E2E_DC_CXX_test; write_cxxrtl  -namespace Cordic_E2E_DC_2 $(SYNTHDESTDIR)Cordic_E2E_DC_CXX_DS2.cpp' 2>&1 |tee $(SYNTHDESTDIR)Cordic_E2E_DC_2.synth.out.txt

$(CXXVERIFDIR)Cordic_E2E_DC_verif_noDS : $(SYNTHDESTDIR)Cordic_E2E_DC_CXX_noDS.cpp $(CXXVERIFDIR)Cordic_E2E_DC_verif.cpp $(CXXVERIFDIR)Cordic_E2E_DC_verif.hxx
	$(GCC_PROG) $(CXXFLAGS) $(CXXVERIFDIR)Cordic_E2E_DC_verif.cpp -DCordic_E2E_DC_CXX_noDS -o $(CXXVERIFDIR)Cordic_E2E_DC_verif_noDS -I$(SYNTHDESTDIR) -I/usr/local/share/yosys/include/backends/cxxrtl/runtime

$(CXXVERIFDIR)Cordic_E2E_DC_verif_DS1 : $(SYNTHDESTDIR)Cordic_E2E_DC_CXX_DS1.cpp $(CXXVERIFDIR)Cordic_E2E_DC_verif.cpp $(CXXVERIFDIR)Cordic_E2E_DC_verif.hxx
	$(GCC_PROG) $(CXXFLAGS) $(CXXVERIFDIR)Cordic_E2E_DC_verif.cpp -DCordic_E2E_DC_CXX_DS1 -o $(CXXVERIFDIR)Cordic_E2E_DC_verif_DS1 -I$(SYNTHDESTDIR) -I/usr/local/share/yosys/include/backends/cxxrtl/runtime

$(CXXVERIFDIR)Cordic_E2E_DC_verif_DS2 : $(SYNTHDESTDIR)Cordic_E2E_DC_CXX_DS2.cpp $(CXXVERIFDIR)Cordic_E2E_DC_verif.cpp $(CXXVERIFDIR)Cordic_E2E_DC_verif.hxx
	$(GCC_PROG) $(CXXFLAGS) $(CXXVERIFDIR)Cordic_E2E_DC_verif.cpp -DCordic_E2E_DC_CXX_DS2 -o $(CXXVERIFDIR)Cordic_E2E_DC_verif_DS2 -I$(SYNTHDESTDIR) -I/usr/local/share/yosys/include/backends/cxxrtl/runtime

$(CXXVERIFDIR)Cordic_E2E_DC_verif_all : $(CXXVERIFDIR)Cordic_E2E_DC_verif_noDS $(CXXVERIFDIR)Cordic_E2E_DC_verif_DS1 $(CXXVERIFDIR)Cordic_E2E_DC_verif_DS2



simul_Prefilter_Storage : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_storage_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) Prefilter_Storage_Test
	$(GHDL_PROG) -r $(VFLAGS) Prefilter_Storage_Test --vcd=$(WAVDESTDIR)PrefilterStorage.wav 2>&1 | tee $(DESTDIR)PrefilterStorage.out.txt

simul_Prefilter_Stages_debug2 : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) Prefilter_Stages_test_debug2
	$(GHDL_PROG) -r $(VFLAGS) Prefilter_Stages_test_debug2 --vcd=$(WAVDESTDIR)PrefilterStagesTestDebug2.wav 2>&1 | tee $(DESTDIR)PrefilterStagesTestDebug2.out.txt

simul_Prefilter_Stages_debug1 : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) Prefilter_Stages_test_debug1
	$(GHDL_PROG) -r $(VFLAGS) Prefilter_Stages_test_debug1 --vcd=$(WAVDESTDIR)PrefilterStagesTestDebug1.wav 2>&1 | tee $(DESTDIR)PrefilterStagesTestDebug1.out.txt

simul_Prefilter_Stages_debug3 : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_test.vhdl
	$(GHDL_PROG) -e $(VFLAGS) Prefilter_Stages_test_debug3
	$(GHDL_PROG) -r $(VFLAGS) Prefilter_Stages_test_debug3 --vcd=$(WAVDESTDIR)PrefilterStagesTestDebug3.wav 2>&1 | tee $(DESTDIR)PrefilterStagesTestDebug3.out.txt

synth_AnglesGene	: $(BUILDDIR)packages_compiled
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) --synth $(VFLAGS) AngleGene
	mkdir -p $(SYNTHDESTDIR)
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) AngleGene; synth_ice40 -json $(SYNTHDESTDIR)AngleGene.ice40.json' 2>&1 |tee $(SYNTHDESTDIR)AngleGene.synth.out.txt
	$(NEXTPNR-ICE40_PROG) --hx4k --package tq144 --freq 50.00 --top AngleGene --asc $(SYNTHDESTDIR)AngleGene.asc --json $(SYNTHDESTDIR)AngleGene.ice40.json --placed-svg $(SYNTHDESTDIR)AngleGene.placed.svg --routed-svg $(SYNTHDESTDIR)AngleGene.routed.svg --report $(SYNTHDESTDIR)AngleGene.report.json 2>&1 |tee $(SYNTHDESTDIR)AngleGene.P_and_R.out.txt
	$(ICEPACK) $(SYNTHDESTDIR)AngleGene.asc $(SYNTHDESTDIR)AngleGene.bin 2>&1 |tee $(SYNTHDESTDIR)AngleGene.pack.out.txt


# cm225 tq144

synth_Cordic_E2E_DC_FPGA	: $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_E2E_DC_bundle.vhdl $(SRCDIR)Cordic_E2E_DC_FPGA_test.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_IO_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Interm_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_Bundle_Stages.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Downsampling_package.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Downsampling.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Cordic_E2E_DC_bundle.vhdl
	$(GHDL_PROG) -a $(VFLAGS) $(SRCDIR)Cordic_E2E_DC_FPGA_test.vhdl
	mkdir -p $(SYNTHDESTDIR)
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) -gextra_shifts=3 -gnbre_Z_2_0_stages=18 -gnbre_Y_2_0_stages=20 Cordic_E2E_DC_FPGA_test; synth_ice40 -json $(SYNTHDESTDIR)Cordic.ice40.json' 2>&1 |tee $(SYNTHDESTDIR)Cordic.synth.out.txt
	$(NEXTPNR-ICE40_PROG) --hx4k --package tq144 --freq 30.00 --top Cordic_E2E_DC_FPGA_test --asc $(SYNTHDESTDIR)Cordic.asc --json $(SYNTHDESTDIR)Cordic.ice40.json --placed-svg $(SYNTHDESTDIR)Cordic.placed.svg --routed-svg $(SYNTHDESTDIR)Cordic.routed.svg --report $(SYNTHDESTDIR)Cordic.report.json 2>&1 |tee $(SYNTHDESTDIR)Cordic.P_and_R.out.txt
	$(ICEPACK) $(SYNTHDESTDIR)Cordic.asc $(SYNTHDESTDIR)Cordic.bin 2>&1 |tee $(SYNTHDESTDIR)Cordic.pack.out.txt

#	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) -gnbre_Z_2_0_stages=18 -gnbre_Y_2_0_stages=15 Cordic_E2E_DC_FPGA_test; synth_ecp5 -json $(SYNTHDESTDIR)Cordic.ecp5.json' 2>&1 |tee $(SYNTHDESTDIR)Cordic.synth.out.txt
#	$(NEXTPNR-ECP5_PROG) --12k --freq 30.00 --top Cordic_E2E_DC_FPGA_test --package TQFP144 --textcfg $(SYNTHDESTDIR)Cordic.txtcfg --json $(SYNTHDESTDIR)Cordic.ecp5.json --placed-svg $(SYNTHDESTDIR)Cordic.placed.svg --routed-svg $(SYNTHDESTDIR)Cordic.routed.svg --report $(SYNTHDESTDIR)Cordic.report.json 2>&1 |tee $(SYNTHDESTDIR)Cordic.P_and_R.out.txt
#	ecppack --svf $(SYNTHDESTDIR)Cordic.svf $(SYNTHDESTDIR)Cordic.txtcfg

synth_Prefilter	: $(BUILDDIR)packages_compiled
	$(GHDL_PROG) -a $(VFLAGS) $(SCRDIR)Prefilter_stages.vhdl
	mkdir -p $(SYNTHDESTDIR)
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) $(VFLAGS) Prefilter_Storage; synth_ice40 -json $(SYNTHDESTDIR)Prefilter.ice40.json' 2>&1 |tee $(SYNTHDESTDIR)Prefilter.synth.out.txt
	$(NEXTPNR-ICE40_PROG) --hx4k --package cm225 --freq 30.00 --top Prefilter_Storage --asc $(SYNTHDESTDIR)Prefilter.asc --json $(SYNTHDESTDIR)Prefilter.ice40.json --placed-svg $(SYNTHDESTDIR)Prefilter.placed.svg --routed-svg $(SYNTHDESTDIR)Prefilter.routed.svg --report $(SYNTHDESTDIR)Prefilter.report.json 2>&1 |tee $(SYNTHDESTDIR)Prefilter.P_and_R.out.txt
	$(ICEPACK) $(SYNTHDESTDIR)Prefilter.asc $(SYNTHDESTDIR)Prefilter.bin 2>&1 |tee $(SYNTHDESTDIR)Prefilter.pack.out.txt




clean	:
	rm -f work-obj93.cf work-obj08.cf $(BUILDDIR)packages_compiled
#	rm -f $(DESTDIR)AngleGene.net $(BUILDDIR)AngleGene.cir $(BUILDDIR)AngleGene_spice.cir
