LCI=lci-0.10.5
NPIET=npiet-1.3c

all: $(LCI)/lci $(NPIET)/npiet

$(LCI)/lci:
	unzip $(LCI).zip
	(cd $(LCI) && cmake . && make)

$(NPIET)/npiet:
	tar xzf $(NPIET).tar.gz
	(cd $(NPIET) && ./configure && make npiet)

clean:
	rm -rf $(LCI)/ $(NPIET)/
