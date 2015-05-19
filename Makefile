sources = Main.hs Serializable.hs FixedPointData.hs TimeData.hs TimeConfiguration.hs Shlex.hs Ask.hs Holidays.hs ParseTime.hs

time: $(sources)
	ghc -Wall -o time -O $(sources)

.PHONY: deps
deps:
	cat dependencies | xargs cabal install

