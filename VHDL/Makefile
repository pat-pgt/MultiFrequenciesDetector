NETLIST_PROG?=gnetlist
EXPORT_PROG?=gaf
GHDL_PROG=ghdl
SRCDIR?=
BUILDDIR?=build/
DESTDIR?=
WAVDESTDIR?=/mnt/ramfs/
SYNTHDESTDIR?=Synth/
YOSYS_PROG?=yosys
NEXTPNR-ICE40_PROG?=nextpnr-ice40
ICEPACK?=icepack


$(BUILDDIR)packages_compiled : $(SCRDIR)MFD_package.vhdl $(SCRDIR)DataMonitor_package.vhdl
	touch $(BUILDDIR)packages_compiled
	$(GHDL_PROG) -a $(SCRDIR)MFD_package.vhdl
	$(GHDL_PROG) -a $(SCRDIR)DataMonitor_package.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_package.vhdl


simul_AnglesGene : $(BUILDDIR)packages_compiled $(SCRDIR)AnglesGene.vhdl $(SCRDIR)DataMonitor.vhdl $(SCRDIR)AnglesGene_test.vhdl
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene_test.vhdl
	$(GHDL_PROG) -e AngleGene_test
	$(GHDL_PROG) -r AngleGene_test --vcd=$(WAVDESTDIR)AngleGene.wav 2>&1 | tee $(DESTDIR)AnglesGene.out.txt
	echo "The wav file is NOT an audio file, it can be opened, for instance, using gtk-wave" > $(WAVDESTDIR)README.important


simul_Cordic : $(BUILDDIR)packages_compiled $(SCRDIR)Cordic_IO_Stages.vhdl $(SCRDIR)Cordic_Interm_Stages.vhdl $(SCRDIR)Cordic_Bundle_Stages.vhdl $(SCRDIR)DataMonitor.vhdl $(SCRDIR)Cordic_test.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_IO_Stages.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_Interm_Stages.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_Bundle_Stages.vhdl
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_Monitor.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_test.vhdl
	$(GHDL_PROG) -e Cordic_bundle_test_Z_to_0_Y_to_0
	$(GHDL_PROG) -r Cordic_bundle_test_Z_to_0_Y_to_0 --vcd=$(WAVDESTDIR)Cordic.wav 2>&1 | tee $(DESTDIR)Cordic.out.txt
	echo "The wav file is NOT an audio file, it can be opened, for instance, using gtk-wave" > $(WAVDESTDIR)README.important

synth_AnglesGene	: $(BUILDDIR)packages_compiled
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene.vhdl
	mkdir -p $(SYNTHDESTDIR)
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) AngleGene; synth_ice40 -json $(SYNTHDESTDIR)AngleGene.ice40.json' 2>&1 |tee $(SYNTHDESTDIR)AngleGene.synth.out.txt
	$(NEXTPNR-ICE40_PROG) --hx4k --package tq144 --freq 50.00 --top AngleGene --asc $(SYNTHDESTDIR)AngleGene.asc --json $(SYNTHDESTDIR)AngleGene.ice40.json --placed-svg $(SYNTHDESTDIR)AngleGene.placed.svg --routed-svg $(SYNTHDESTDIR)AngleGene.routed.svg --report $(SYNTHDESTDIR)AngleGene.report.json 2>&1 |tee $(SYNTHDESTDIR)AngleGene.P_and_R.out.txt
	$(ICEPACK) $(SYNTHDESTDIR)AngleGene.asc $(SYNTHDESTDIR)AngleGene.bin 2>&1 |tee $(SYNTHDESTDIR)AngleGene.pack.out.txt


synth_Cordic	: $(BUILDDIR)packages_compiled
	$(GHDL_PROG) -a $(SCRDIR)Cordic_IO_Stages.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_Interm_Stages.vhdl
	$(GHDL_PROG) -a $(SCRDIR)Cordic_Bundle_Stages.vhdl
	mkdir -p $(SYNTHDESTDIR)
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) Cordic_Bundle_Z_to_0; synth_ice40 -json $(SYNTHDESTDIR)Cordic.ice40.json' 2>&1 |tee $(SYNTHDESTDIR)Cordic.synth.out.txt
	$(NEXTPNR-ICE40_PROG) --hx4k --package cm225 --freq 30.00 --top Cordic_Bundle_Z_to_0_Y_to_0 --asc $(SYNTHDESTDIR)Cordic.asc --json $(SYNTHDESTDIR)Cordic.ice40.json --placed-svg $(SYNTHDESTDIR)Cordic.placed.svg --routed-svg $(SYNTHDESTDIR)Cordic.routed.svg --report $(SYNTHDESTDIR)Cordic.report.json 2>&1 |tee $(SYNTHDESTDIR)Cordic.P_and_R.out.txt
	$(ICEPACK) $(SYNTHDESTDIR)Cordic.asc $(SYNTHDESTDIR)Cordic.bin 2>&1 |tee $(SYNTHDESTDIR)Cordic.pack.out.txt




clean	:
	rm -f $(DESTDIR)AngleGene.net $(BUILDDIR)AngleGene.cir $(BUILDDIR)AngleGene_spice.cir
