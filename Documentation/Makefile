DOT?=dot
DOTLANG?=pdf
DOTFLAGS?=
SRCDIR?=
BUILDDIR?=

doc			:
	$(DOT) $(DOTFLAGS) -T$(DOTLANG) $(SRCDIR)Overview.gv -o $(BUILDDIR)Overview.$(DOTLANG) 
	$(DOT) $(DOTFLAGS) -T$(DOTLANG) $(SRCDIR)CXX_test_E2E_DC.gv -o $(BUILDDIR)CXX_test_E2E_DC.$(DOTLANG) 

clean		:
	rm -f $(BUILDDIR)Overview.$(DOTLANG)
	rm -f $(BUILDDIR)CXX_test_E2E_DC.$(DOTLANG)
