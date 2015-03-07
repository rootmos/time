sources = Main.hs Serializable.hs FixedPointData.hs

time: $(sources)
	ghc -o time -O $(sources)
