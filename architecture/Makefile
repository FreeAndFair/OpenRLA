INFORMAL_BON_FILES = RLA.bon
FORMAL_BON_FILES =
REQUIREMENTS = Creation.bon \
               Events.bon \
               Scenarios.bon

BON_FILES = $(INFORMAL_BON_FILES) $(FORMAL_BON_FILES) $(REQUIREMENTS)

.PHONY: all java

all: rla-iig.dot index.txt index.html html/index.html consistency

typecheck-informal:
	bonc -ci $(INFORMAL_BON_FILES)

typecheck-formal:
	bonc -cf $(FORMAL_BON_FILES)

typecheck: typecheck-formal typecheck-informal
	bonc -tc $(BON_FILES)

consistency: typecheck
	bonc -cc $(BON_FILES)

dictionary: consistency
	bonc -gcd $(BON_FILES)

rla-icg.dot: $(BON_FILES)
	bonc -g ICG $(BON_FILES) > rla-icg.dot

rla-iig.dot: $(BON_FILES)
	bonc -p IIG -po rla-iig.dot $(BON_FILES)

index.txt: $(BON_FILES)
	bonc -po index.txt -p TXT $(BON_FILES)

index.html: $(BON_FILES)
	bonc -po index.html -p HTML $(BON_FILES)

html/index.html: $(BON_FILES)
	bonc -p NEWHTML -po html $(BON_FILES)
