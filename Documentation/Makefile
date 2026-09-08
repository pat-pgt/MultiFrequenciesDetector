DOT?=dot
DOTLANG?=pdf
DOTFLAGS?=
GHOSTSCRIPTEXE?=gs
SRCDIR?=
BUILDDIR?=

doc			:
	$(DOT) $(DOTFLAGS) -T$(DOTLANG) $(SRCDIR)Overview.gv -o $(BUILDDIR)Overview.$(DOTLANG) 
	$(DOT) $(DOTFLAGS) -T$(DOTLANG) $(SRCDIR)CXX_test_E2E_DC.gv -o $(BUILDDIR)CXX_test_E2E_DC.$(DOTLANG) 
ifeq ($(DOTLANG),pdf)
	$(GHOSTSCRIPTEXE) -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -sOutputFile=$(BUILDDIR)Cordic_IntermStages_selector.shift_4.pdf Cordic_IntermStages_selector.shift_4.ps
endif
ifeq ($(DOTLANG),ps)
	cp Cordic_IntermStages_selector.shift_4.ps $(BUILDDIR)
endif
ifeq ($(DOTLANG),png)
	$(GHOSTSCRIPTEXE) -sDEVICE=png16m -dNOPAUSE -dBATCH -sOutputFile=$(BUILDDIR)Cordic_IntermStages_selector.shift_4.png Cordic_IntermStages_selector.shift_4.ps
endif
ifeq ($(DOTLANG),jpeg)
	$(GHOSTSCRIPTEXE) -sDEVICE=jpeg -dNOPAUSE -dBATCH -sOutputFile=$(BUILDDIR)Cordic_IntermStages_selector.shift_4.jpeg Cordic_IntermStages_selector.shift_4.ps
endif
clean		:
	rm -f $(BUILDDIR)Overview.$(DOTLANG)
	rm -f $(BUILDDIR)CXX_test_E2E_DC.$(DOTLANG)
