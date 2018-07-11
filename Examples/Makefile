FILES=Examples/AddMult.hs \
	Examples/Array.hs \
	Examples/BadExtVars.hs \
	Examples/Cast.hs \
	Examples/ClockExamples.hs \
	Examples/EngineExample.hs \
	Examples/Examples2.hs \
	Examples/Examples.hs \
	Examples/ExtFuns.hs \
	Examples/Languages.hs \
	Examples/Local.hs \
	Examples/LTLExamples.hs \
	Examples/PTLTLExamples.hs \
	Examples/Random.hs \
	Examples/RegExpExamples.hs \
	Examples/Sat.hs \
	Examples/StackExamples.hs \
	Examples/StatExamples.hs \
	Examples/VotingExamples.hs

examples: $(FILES)

.PHONY: phony
phony:

Examples/%.hs: phony
	-./runtest $@
