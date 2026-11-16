package econetcons

/**
 * Local CSV driver for the tile-size × nPop × protection-fraction exploration, comparing
 * SpeciesRichness vs InteractionRichness via Observables.runPair. Emits one CSV row per
 * (nPop, tileSize, fraction, seed). OpenMOLE would call Observables.runPair directly and
 * handle replication/aggregation; this driver is for local inspection.
 */
object TileStrategySweep:

  val numberOfSpecies = 200
  val connectance     = 0.4
  val medianHomeRange = 0.05
  val preyPerPredator = 2.0

  val tileSizes = Seq(16, 32, 64, 128, 256)
  val popCounts = Seq(1000, 5000, 10000)
  val fractions = (1 to 10).map(_ * 0.05)   // 0.05 .. 0.50
  val seeds     = Seq(1L, 2L, 3L)

  def main(args: Array[String]): Unit =
    val realOut = System.out
    def silence[T](b: => T): T =
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    // Warm one run to fix the column order.
    val sample = silence { Observables.run(numberOfSpecies, connectance, 1000, medianHomeRange, 64, preyPerPredator, 0.3, 1L) }
    val cols = Observables.columns(sample)

    val header = (Seq("nPop", "tileSize", "fraction", "seed") ++ cols).mkString(",")
    realOut.println(header)

    for
      nPop     <- popCounts
      tileSize <- tileSizes
      frac     <- fractions
      seed     <- seeds
    do
      val m = silence { Observables.run(numberOfSpecies, connectance, nPop, medianHomeRange, tileSize, preyPerPredator, frac, seed) }
      val row = (Seq(nPop.toString, tileSize.toString, f"$frac%.2f", seed.toString) ++ cols.map(c => f"${m(c)}%.4f")).mkString(",")
      realOut.println(row)
