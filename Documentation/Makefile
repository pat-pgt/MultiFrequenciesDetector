DOT?=dot
DOTLANG?=pdf
DOTFLAGS?=
SRCDIR?=
BUILDDIR?=

doc			:
	$(DOT) $(DOTFLAGS) -T$(DOTLANG) $(SRCDIR)Overview.gv -o $(BUILDDIR)Overview.$(DOTLANG) 

clean		:
	rm -f $(BUILDDIR)Overview.$(DOTLANG)
