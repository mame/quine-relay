LCI=lci-0.10.5

all: $(LCI)/lci

$(LCI)/lci:
	unzip $(LCI).zip
	(cd $(LCI) && cmake . && make)

clean:
	rm -rf $(LCI)/
