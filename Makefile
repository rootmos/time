sources = Main.hs Serializable.hs FixedPointData.hs TimeData.hs TimeConfiguration.hs Shlex.hs

time: $(sources)
	ghc -o time -O $(sources)
