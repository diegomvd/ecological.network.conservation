package econetcons

import java.io.{FileWriter, PrintWriter}

/**
 * TEMPORARY: estimates the mean per-run wall time over the FINAL locked Saltelli box by
 * timing uniformly-random interior points. Writes each line to a CSV file (flushed per line)
 * so nothing is lost to stdout buffering under background/pipe execution. Delete after budget
 * is confirmed.
 */
object BoxMeanProbe:
  def main(args: Array[String]): Unit =
    val nPoints = if args.nonEmpty then args(0).toInt else 20
    val out = new PrintWriter(new FileWriter("/tmp/boxmean.csv"), true) // autoFlush = true

    def silence[T](b: => T): T =
      val realOut = System.out
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    // FINAL locked box (see saltelli_sensitivity_plan.md): k=8, tile 10–100, species=200, sigma=1.0
    val connLo = 0.05; val connHi = 0.25
    val epsLo  = 0.5;  val epsHi  = 5.0
    val nPopLo = 5000; val nPopHi = 10000
    val hrLo   = 0.01; val hrHi   = 0.05
    val extLo  = 0.05; val extHi  = 1.0
    val fracLo = 0.05; val fracHi = 0.45
    val tileLo = 10;   val tileHi = 100
    val patchLo = 1.0; val patchHi = 10.0

    val rnd = new scala.util.Random(42L)
    def u(lo: Double, hi: Double): Double = lo + rnd.nextDouble() * (hi - lo)

    // warm-up (excluded)
    silence { Observables.run(200, 0.15, 5000, 0.03, 50, 0.0, 0.25, 999L, epsilon = 2.0,
      extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0) }

    out.println("i,tile,nPop,conn,eps,frac,ms")
    var total = 0.0
    for i <- 1 to nPoints do
      val conn = u(connLo, connHi); val eps = u(epsLo, epsHi)
      val nPop = math.round(u(nPopLo, nPopHi)).toInt
      val hr = u(hrLo, hrHi); val ext = u(extLo, extHi)
      val frac = u(fracLo, fracHi); val tile = math.round(u(tileLo, tileHi)).toInt
      val patch = u(patchLo, patchHi)
      val t0 = System.nanoTime()
      silence { Observables.run(200, conn, nPop, hr, tile, 0.0, frac, 1L, epsilon = eps,
        extinctionProbabilityUnprotected = ext, bodySizeSigma = 1.0, meanPatchesPerSpecies = patch) }
      val ms = (System.nanoTime() - t0) / 1e6
      total += ms
      out.println(f"$i,$tile,$nPop,$conn%.3f,$eps%.2f,$frac%.2f,$ms%.0f")
    out.println(f"# MEAN over $nPoints points: ${total / nPoints}%.0f ms")
    out.close()
