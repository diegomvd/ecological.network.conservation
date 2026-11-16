package econetcons

/**
 * TEMPORARY diagnostic: extended epsilon sweep (incl. epsilon >> 2) reporting BOTH the
 * ε-response and whether the SR vs IR conservation strategies actually diverge. For each ε
 * it builds one shared base world per seed and evaluates every strategy on it (via
 * Observables.run), then reports:
 *   baseline species (post-Phase-0), SR/IR total species loss %, SR/IR robustness (30% shock),
 *   and the SR-vs-IR divergence: protected-set Jaccard, score Spearman, unique species protected.
 * Small Jaccard / low Spearman / high uniqueSpecies => strategies genuinely differ at that ε.
 */
object PPPCheck:
  def main(args: Array[String]): Unit =
    val realOut = System.out
    def silence[T](b: => T): T =
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    val seeds = Seq(1L, 2L, 3L)
    val nSpecies = 200; val conn = 0.10; val nPop = 5000; val hr = 0.05; val tile = 64
    val frac = 0.30

    // Fix column order via defaultStrategies (SR, IR, EIR). Pairwise keys use these labels.
    def k(name: String): Map[String, Double] => Double = m => m(name)
    val keys: Seq[(String, Map[String, Double] => Double)] = Seq(
      "baseline_sp"      -> k("SR_baseline_sp"),
      "SR_spLoss%"       -> k("SR_sp_ext_total"),
      "IR_spLoss%"       -> k("IR_sp_ext_total"),
      "SR_robust30"      -> k("SR_robust_sp_ext_30"),
      "IR_robust30"      -> k("IR_robust_sp_ext_30"),
      "SRvsIR_jaccard"   -> k("pair_SR_IR_jaccard_protected"),
      "SRvsIR_spearman"  -> k("pair_SR_IR_rankCorr_spearman"),
      "SRvsIR_uniqSp_SR" -> k("pair_SR_IR_uniqueSpecies_SR"),
    )

    System.err.println(s"[epsilon sweep + strategy divergence]  C=$conn nPop=$nPop S=$nSpecies frac=$frac")
    System.err.println("  eps | " + keys.map(_._1).mkString(" | "))

    for eps <- Seq(0.5, 1.0, 2.0, 4.0, 8.0, 16.0) do
      val maps = seeds.map { s =>
        silence { Observables.run(nSpecies, conn, nPop, hr, tile, 0.0, frac, s, epsilon = eps) }
      }
      def avg(f: Map[String, Double] => Double) = maps.map(f).sum / maps.size
      val vals = keys.map { case (_, f) => avg(f) }
      System.err.println(f"  ${eps}%5.1f | " + vals.map(v => f"$v%8.2f").mkString(" | "))
