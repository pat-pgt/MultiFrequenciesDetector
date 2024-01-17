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


simul_AnglesGene : $(BUILDDIR)packages_compiled $(SCRDIR)AnglesGene.vhdl $(SCRDIR)AnglesGene_test.vhdl
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene.vhdl
	$(GHDL_PROG) -a $(SCRDIR)DataMonitor.vhdl
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene_test.vhdl
	$(GHDL_PROG) -e AngleGene_test
	$(GHDL_PROG) -r AngleGene_test --vcd=$(WAVDESTDIR)AngleGene.wav 2>&1 | tee $(DESTDIR)AnglesGene.out.txt
	echo "The wav file is NOT an audio file, it can be opened, for instance, using gtk-wave" > $(WAVDESTDIR)README.important

synth_ice40	: $(BUILDDIR)packages_compiled 
	$(GHDL_PROG) -a $(SCRDIR)MFD_package.vhdl
	$(GHDL_PROG) -a $(SCRDIR)AnglesGene.vhdl
	mkdir -p $(SYNTHDESTDIR)
	$(YOSYS_PROG) -m ghdl -p '$(GHDL_PROG) AngleGene; synth_ice40 -json $(SYNTHDESTDIR)AngleGene.ice40.json' 2>&1 |tee $(SYNTHDESTDIR)AngleGene.synth.out.txt
	$(NEXTPNR-ICE40_PROG) --hx4k --package tq144 --freq 70.00 --top AngleGene --asc $(SYNTHDESTDIR)AngleGene.asc --json $(SYNTHDESTDIR)AngleGene.ice40.json --placed-svg $(SYNTHDESTDIR)AngleGene.placed.svg --routed-svg $(SYNTHDESTDIR)AngleGene.routed.svg --report $(SYNTHDESTDIR)AngleGene.report.json 2>&1 |tee $(SYNTHDESTDIR)AngleGene.P_and_R.out.txt
	$(ICEPACK) $(SYNTHDESTDIR)AngleGene.asc $(SYNTHDESTDIR)AngleGene.bin 2>&1 |tee $(SYNTHDESTDIR)AngleGene.pack.out.txt




clean	:
	rm -f $(DESTDIR)AngleGene.net $(BUILDDIR)AngleGene.cir $(BUILDDIR)AngleGene_spice.cir
