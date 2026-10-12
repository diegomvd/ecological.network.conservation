package econetcons

/**
 * TEMPORARY timing harness: measures wall-clock cost of a single Observables.run
 * (one base world, all default strategies, full extinction pipeline) across the
 * parameters that drive runtime — nPopulations, numberOfSpecies, epsilon.
 * Used to size the Saltelli exploration budget. Delete once ranges are chosen.
 */
object TimingHarness:
  def main(args: Array[String]): Unit =
    val realOut = System.out
    def silence[T](b: => T): T =
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    def timeOne(
      nSpecies: Int, conn: Double, nPop: Int, hr: Double,
      tile: Int, frac: Double, eps: Double, seed: Long
    ): (Double, Int) =
      val t0 = System.nanoTime()
      val m = silence { Observables.run(nSpecies, conn, nPop, hr, tile, 0.0, frac, seed, epsilon = eps) }
      val ms = (System.nanoTime() - t0) / 1e6
      (ms, m.size)

    // Fixed context for the timing grid
    val hr = 0.05
    val tile = 64
    val frac = 0.30
    val conn = 0.15          // valid niche-model connectance (β = 0.5/C − 1 requires C < ~0.5)
    val warmSeed = 999L

    // JIT warm-up (first run pays classloading + JIT compilation; not representative)
    System.err.println("[warm-up] ...")
    silence { Observables.run(200, conn, 5000, hr, tile, 0.0, frac, warmSeed, epsilon = 1.0) }

    val nSpeciesGrid = Seq(200, 400)
    val nPopGrid     = Seq(5000, 7500, 10000)
    val epsGrid      = Seq(0.5, 2.0, 5.0)
    val timingSeeds  = Seq(1L, 2L, 3L)   // 3 reps per cell to get a mean

    System.err.println(f"${"nSpecies"}%9s ${"nPop"}%7s ${"epsilon"}%8s ${"mean_ms"}%10s ${"min_ms"}%9s ${"max_ms"}%9s ${"nOut"}%6s")
    for
      s   <- nSpeciesGrid
      np  <- nPopGrid
      eps <- epsGrid
    do
      val results = timingSeeds.map(seed => timeOne(s, conn, np, hr, tile, frac, eps, seed))
      val times = results.map(_._1)
      val nOut = results.head._2
      val mean = times.sum / times.size
      System.err.println(f"$s%9d $np%7d $eps%8.1f $mean%10.1f ${times.min}%9.1f ${times.max}%9.1f $nOut%6d")
