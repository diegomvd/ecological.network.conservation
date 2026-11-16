package econetcons

/**
 * TEMPORARY: exercises the extreme corners of the locked 10-D Saltelli input box to catch
 * domain failures (e.g. the Invalid bounds throw at high connectance) BEFORE spending a full
 * Saltelli budget. Runs each corner on a couple of seeds, reports OK / FAILED + a couple of
 * sanity outputs (baseline species, IR-vs-SR interaction-loss gap) and timing. Delete after
 * the .oms validates.
 */
object CornerSmokeTest:
  def main(args: Array[String]): Unit =
    val realOut = System.out
    def silence[T](b: => T): T =
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    // Locked box bounds (see saltelli_sensitivity_plan.md)
    val connLo = 0.05;  val connHi = 0.25
    val epsLo  = 0.5;   val epsHi  = 5.0
    val nPopLo = 5000;  val nPopHi = 10000
    val nSpLo  = 200;   val nSpHi  = 200   // FIXED at 200
    val hrLo   = 0.01;  val hrHi   = 0.05
    val extLo  = 0.05;  val extHi  = 1.0
    val fracLo = 0.05;  val fracHi = 0.45  // trimmed ceiling 0.95 -> 0.45
    val tileLo = 10;    val tileHi = 100   // capped 200 -> 100
    val sigLo  = 1.0;   val sigHi  = 1.0   // FIXED at 1.0
    val patchLo = 1.0;  val patchHi = 10.0

    case class Corner(
      name: String, nSp: Int, conn: Double, nPop: Int, hr: Double, tile: Int,
      frac: Double, eps: Double, ext: Double, sig: Double, patch: Double
    )

    // A curated set of corners: all-low, all-high, plus the ecologically/numerically risky ones
    // (low-C × high-S sparse-web; dense homeRange × fine tiles; sigma extremes; low-eps deep web).
    val corners = Seq(
      Corner("all-low",           nSpLo, connLo, nPopLo, hrLo, tileLo, fracLo, epsLo, extLo, sigLo, patchLo),
      Corner("all-high",          nSpHi, connHi, nPopHi, hrHi, tileHi, fracHi, epsHi, extHi, sigHi, patchHi),
      Corner("hiC-hiS-sparse",    nSpHi, connHi, nPopHi, hrLo, tileLo, fracLo, epsLo, extHi, sigLo, patchLo),
      Corner("loC-hiS-broadbase", nSpHi, connLo, nPopHi, hrHi, tileLo, fracHi, epsLo, extLo, sigHi, patchHi),
      Corner("dense-fine-tiles",  nSpHi, connLo, nPopHi, hrHi, tileLo, fracLo, epsLo, extHi, sigHi, patchHi),
      Corner("sparse-coarse",     nSpLo, connHi, nPopLo, hrLo, tileHi, fracHi, epsHi, extLo, sigLo, patchLo),
      Corner("sigma-hi-loC",      nSpHi, connLo, nPopHi, hrHi, tileLo, fracHi, epsHi, extHi, sigHi, patchLo),
      Corner("sigma-lo-hiC",      nSpLo, connHi, nPopLo, hrLo, tileLo, fracLo, epsLo, extLo, sigLo, patchHi),
      Corner("loeps-deepweb",     nSpHi, connLo, nPopHi, hrHi, tileHi, fracLo, epsLo, extHi, sigHi, patchHi),
      Corner("hieps-loprotect",   nSpLo, connLo, nPopLo, hrLo, tileLo, fracLo, epsHi, extLo, sigLo, patchLo),
    )

    val seeds = Seq(1L, 7L)

    def runOne(c: Corner, seed: Long): Either[String, Map[String, Double]] =
      try Right(silence {
        Observables.run(
          numberOfSpecies = c.nSp, connectanceMetaWeb = c.conn, nPopulations = c.nPop,
          medianHomeRange = c.hr, avgPopulationsPerTile = c.tile, preyPerPredator = 0.0,
          fractionProtected = c.frac, seed = seed, epsilon = c.eps,
          extinctionProbabilityUnprotected = c.ext, bodySizeSigma = c.sig,
          meanPatchesPerSpecies = c.patch,
        )
      })
      catch case t: Throwable => Left(s"${t.getClass.getSimpleName}: ${t.getMessage}")

    def log(s: String): Unit = { System.err.println(s); System.err.flush() }

    log(f"${"corner"}%-18s ${"seed"}%5s ${"status"}%-8s ${"baseSR"}%8s ${"dSpInt_IR_SR"}%13s ${"ms"}%8s")
    var failures = 0
    var total = 0.0
    for c <- corners; seed <- seeds do
      log(f"  -> starting ${c.name}%-18s seed=$seed (tile=${c.tile} frac=${c.frac}%.2f) ...")
      val t0 = System.nanoTime()
      val res = runOne(c, seed)
      val ms = (System.nanoTime() - t0) / 1e6
      total += ms
      res match
        case Right(m) =>
          val base = m.getOrElse("SR_baseline_sp", Double.NaN)
          val gap  = m.getOrElse("IR_spint_ext_total", Double.NaN) - m.getOrElse("SR_spint_ext_total", Double.NaN)
          log(f"${c.name}%-18s $seed%5d ${"OK"}%-8s $base%8.1f $gap%13.2f $ms%8.1f")
        case Left(err) =>
          failures += 1
          log(f"${c.name}%-18s $seed%5d ${"FAILED"}%-8s ${err}")

    val n = corners.size * seeds.size
    log(f"\n[CornerSmokeTest corners] $n runs, $failures failed. mean=${total / n}%.0f ms")

    // Random interior points — a better estimate of the true box mean than corners alone.
    val rnd = new scala.util.Random(42L)
    def u(lo: Double, hi: Double): Double = lo + rnd.nextDouble() * (hi - lo)
    val nRand = 30
    var randTotal = 0.0
    log(f"\n[random interior sample: $nRand points]")
    for i <- 1 to nRand do
      val c = Corner(s"rand$i",
        200, u(connLo, connHi), math.round(u(nPopLo, nPopHi)).toInt, u(hrLo, hrHi),
        math.round(u(tileLo, tileHi)).toInt, u(fracLo, fracHi), u(epsLo, epsHi),
        u(extLo, extHi), 1.0, u(patchLo, patchHi))
      val t0 = System.nanoTime()
      val res = runOne(c, 1L)
      val ms = (System.nanoTime() - t0) / 1e6
      randTotal += ms
      val ok = if res.isRight then "OK" else "FAILED"
      log(f"  rand$i%-6s tile=${c.tile}%3d nPop=${c.nPop}%5d frac=${c.frac}%.2f  $ok%-6s $ms%8.1f ms")
    log(f"\n[random interior] $nRand runs, mean=${randTotal / nRand}%.0f ms  =>  projected box mean per run")
