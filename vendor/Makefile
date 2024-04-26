LCI       := lci-0.10.5
NPIET     := npiet-1.3e
CFUNGE    := cfunge-0.9.0
ACME_CHEF := Acme-Chef-1.03
SPL	  := spl-1.2.1
LOCAL     := $(CURDIR)/local

.NOTPARALLEL:
all: $(LOCAL)/bin/lci $(LOCAL)/bin/npiet $(LOCAL)/bin/cfunge $(LOCAL)/bin/compilechef $(LOCAL)/bin/lazyk $(LOCAL)/bin/spl2c $(LOCAL)/bin/Vlt.exe

$(LOCAL)/bin/lci:
	unzip -q $(LCI).zip
	(cd $(LCI) && cmake -DCMAKE_INSTALL_PREFIX=$(LOCAL) . && make && make install)

$(LOCAL)/bin/npiet:
	tar xzf $(NPIET).tar.gz
	(cd $(NPIET) && ./configure --prefix=$(LOCAL) && make && make install)

$(LOCAL)/bin/cfunge:
	tar xjf $(CFUNGE).tar.bz2
	(cd $(CFUNGE) && cmake -DCMAKE_INSTALL_PREFIX=$(LOCAL) . && make && make install)

$(LOCAL)/bin/compilechef:
	tar xzf $(ACME_CHEF).tar.gz
	(cd $(ACME_CHEF) && perl Makefile.PL INSTALL_BASE=$(LOCAL) && make && make install)

$(LOCAL)/bin/lazyk:
	$(CC) lazyk.c -o $@

$(LOCAL)/bin/spl2c:
	tar xzf $(SPL).tar.gz
	(cd $(SPL) && ln -s ../local spl && make spl2c CCFLAGS="-O0 -g -Wall" && make install)

$(LOCAL)/bin/Vlt.exe:
	unzip -d $(LOCAL)/bin -q Velato_0_1.zip

clean:
	rm -rf $(LOCAL)/ $(LCI)/ $(NPIET)/ $(CFUNGE)/ $(ACME_CHEF)/ $(SPL)/
