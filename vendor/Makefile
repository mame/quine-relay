LCI=lci-0.10.5
NPIET=npiet-1.3c
CFUNGE=cfunge-0.9.0

all: $(LCI)/lci $(NPIET)/npiet $(CFUNGE)/cfunge

$(LCI)/lci:
	unzip $(LCI).zip
	(cd $(LCI) && cmake . && make)

$(NPIET)/npiet:
	tar xzf $(NPIET).tar.gz
	(cd $(NPIET) && ./configure && make npiet)

$(CFUNGE)/cfunge:
	tar xjf $(CFUNGE).tar.bz2
	(cd $(CFUNGE) && cmake . && make)

clean:
	rm -rf $(LCI)/ $(NPIET)/ $(CFUNGE)/
