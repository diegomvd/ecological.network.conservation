package econetcons

/** Dumps Observables.run outputs for fixed configs/seeds, to check refactors keep defaults bit-identical. */
object RegressionSnapshot:
  def main(args: Array[String]): Unit =
    val out = new java.io.PrintStream(new java.io.FileOutputStream(args.headOption.getOrElse("snapshot.txt")))
    val real = System.out
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    System.setProperty("econetcons.skipRobustness", "true")
    for (eps, seed) <- Seq((2.0, 1L), (2.0, 2L), (0.0, 3L)) do
      val m = Observables.run(300, 0.2, 5000, 0.02, 32, 0.0, 0.2, seed, epsilon = eps,
        extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0)
      m.toSeq.sortBy(_._1).foreach { case (k, v) => out.println(s"eps=$eps seed=$seed $k=$v") }
    out.close()
    real.println("snapshot written")
