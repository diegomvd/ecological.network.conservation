package econetcons

import java.io.{File, PrintWriter, FileOutputStream, OutputStreamWriter, BufferedWriter}
import java.nio.charset.StandardCharsets

/**
 * Headless CLI runner for the Saltelli / Sobol sensitivity analysis on a plain SLURM
 * job array (Drago CSIC). This is the native-execution replacement for the abandoned
 * OpenMOLE workflow `saltelli_sensitivity.oms` — it reproduces the EXACT per-design-point
 * logic of that `.oms` ScalaTask (5-seed averaging, Int rounding of nPop/tile, the same 17
 * emitted output columns) but reads its design points from a CSV instead of an OM sampler.
 *
 * ── Contract ────────────────────────────────────────────────────────────────────────
 * Reads a Saltelli design matrix CSV (produced offline by `scripts/gen_design.py` using
 * SALib) with a header and these 8 input columns (order-independent, matched by NAME):
 *
 *   connectanceMetaWeb, epsilon, nPopulations, medianHomeRange,
 *   extinctionProbabilityUnprotected, fractionProtected, avgPopulationsPerTile,
 *   meanPatchesPerSpecies
 *
 * The runner processes a ROW RANGE [startRow, endRow) of that design (0-based, endRow
 * exclusive) so a SLURM array can chunk the design across tasks. Each task writes its OWN
 * output file (no shared-file contention). Every output row carries the original design
 * `row_index` so the shards can be concatenated and matched back to the design in any order.
 *
 * ── Usage ───────────────────────────────────────────────────────────────────────────
 *   java -cp <fatjar> econetcons.SaltelliRunner \
 *        --design  design.csv \
 *        --out     results_<taskid>.csv \
 *        --start   <startRow> \
 *        --end     <endRow> \
 *        [--seeds  5]
 *
 * If --start/--end are omitted the whole design is processed (useful for a local pilot).
 *
 * ── Output columns ──────────────────────────────────────────────────────────────────
 *   row_index,
 *   <the 8 input columns, echoed back for self-contained analysis>,
 *   dSp_IR_SR, dSpInt_IR_SR, dRobust_IR_SR,
 *   dSp_EIR_SR, dSpInt_EIR_SR, dRobust_EIR_SR,
 *   SR_sp_ext_total, IR_sp_ext_total, EIR_sp_ext_total,
 *   SR_spint_ext_total, IR_spint_ext_total, EIR_spint_ext_total,
 *   SR_robust_sp_ext_30, IR_robust_sp_ext_30, EIR_robust_sp_ext_30,
 *   baseline_sp, jaccard_SR_IR
 */
object SaltelliRunner:

  // Fixed factors (see saltelli_sensitivity_plan.md): numberOfSpecies=200, bodySizeSigma=1.0,
  // preyPerPredator=0.0 (legacy cascade off), observationNoise/falsePositiveRate=0.0 (run defaults).
  private val NumberOfSpecies = 200
  private val BodySizeSigma   = 1.0
  private val PreyPerPredator = 0.0

  // Input columns expected in the design CSV, in the canonical echo order.
  private val InputCols = Seq(
    "connectanceMetaWeb",
    "epsilon",
    "nPopulations",
    "medianHomeRange",
    "extinctionProbabilityUnprotected",
    "fractionProtected",
    "avgPopulationsPerTile",
    "meanPatchesPerSpecies",
  )

  // The 17 emitted metric columns (order matches saltelli_sensitivity.oms outputs).
  private val OutputCols = Seq(
    "dSp_IR_SR", "dSpInt_IR_SR", "dRobust_IR_SR",
    "dSp_EIR_SR", "dSpInt_EIR_SR", "dRobust_EIR_SR",
    "SR_sp_ext_total", "IR_sp_ext_total", "EIR_sp_ext_total",
    "SR_spint_ext_total", "IR_spint_ext_total", "EIR_spint_ext_total",
    "SR_robust_sp_ext_30", "IR_robust_sp_ext_30", "EIR_robust_sp_ext_30",
    "baseline_sp", "jaccard_SR_IR",
  )

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect {
      case Array(k, v) if k.startsWith("--") => k.drop(2) -> v
    }.toMap

  /** One design point → the 17 seed-averaged outputs, mirroring the .oms ScalaTask exactly. */
  private def evalPoint(in: Map[String, Double], nSeeds: Int): Map[String, Double] =
    val nPop = math.round(in("nPopulations")).toInt
    val tile = math.round(in("avgPopulationsPerTile")).toInt

    val maps: Seq[Map[String, Double]] = (1 to nSeeds).map { s =>
      Observables.run(
        numberOfSpecies                  = NumberOfSpecies,
        connectanceMetaWeb               = in("connectanceMetaWeb"),
        nPopulations                     = nPop,
        medianHomeRange                  = in("medianHomeRange"),
        avgPopulationsPerTile            = tile,
        preyPerPredator                  = PreyPerPredator,
        fractionProtected                = in("fractionProtected"),
        seed                             = s.toLong,
        epsilon                          = in("epsilon"),
        extinctionProbabilityUnprotected = in("extinctionProbabilityUnprotected"),
        bodySizeSigma                    = BodySizeSigma,
        meanPatchesPerSpecies            = in("meanPatchesPerSpecies"),
      )
    }

    def mean(key: String): Double = maps.map(_.getOrElse(key, 0.0)).sum / maps.size

    val SR_sp_ext_total      = mean("SR_sp_ext_total")
    val IR_sp_ext_total      = mean("IR_sp_ext_total")
    val EIR_sp_ext_total     = mean("EIR_sp_ext_total")
    val SR_spint_ext_total   = mean("SR_spint_ext_total")
    val IR_spint_ext_total   = mean("IR_spint_ext_total")
    val EIR_spint_ext_total  = mean("EIR_spint_ext_total")
    val SR_robust_sp_ext_30  = mean("SR_robust_sp_ext_30")
    val IR_robust_sp_ext_30  = mean("IR_robust_sp_ext_30")
    val EIR_robust_sp_ext_30 = mean("EIR_robust_sp_ext_30")
    val baseline_sp          = mean("SR_baseline_sp")
    val jaccard_SR_IR        = mean("pair_SR_IR_jaccard_protected")

    Map(
      "dSp_IR_SR"           -> (IR_sp_ext_total      - SR_sp_ext_total),
      "dSpInt_IR_SR"        -> (IR_spint_ext_total   - SR_spint_ext_total),
      "dRobust_IR_SR"       -> (IR_robust_sp_ext_30  - SR_robust_sp_ext_30),
      "dSp_EIR_SR"          -> (EIR_sp_ext_total     - SR_sp_ext_total),
      "dSpInt_EIR_SR"       -> (EIR_spint_ext_total  - SR_spint_ext_total),
      "dRobust_EIR_SR"      -> (EIR_robust_sp_ext_30 - SR_robust_sp_ext_30),
      "SR_sp_ext_total"     -> SR_sp_ext_total,
      "IR_sp_ext_total"     -> IR_sp_ext_total,
      "EIR_sp_ext_total"    -> EIR_sp_ext_total,
      "SR_spint_ext_total"  -> SR_spint_ext_total,
      "IR_spint_ext_total"  -> IR_spint_ext_total,
      "EIR_spint_ext_total" -> EIR_spint_ext_total,
      "SR_robust_sp_ext_30" -> SR_robust_sp_ext_30,
      "IR_robust_sp_ext_30" -> IR_robust_sp_ext_30,
      "EIR_robust_sp_ext_30"-> EIR_robust_sp_ext_30,
      "baseline_sp"         -> baseline_sp,
      "jaccard_SR_IR"       -> jaccard_SR_IR,
    )

  def main(args: Array[String]): Unit =
    val opts   = parseArgs(args)
    val design = opts.getOrElse("design", sys.error("--design <design.csv> is required"))
    val out    = opts.getOrElse("out",    sys.error("--out <results.csv> is required"))
    val nSeeds = opts.get("seeds").map(_.toInt).getOrElse(5)

    // Read design CSV (comma-separated, header row with named columns).
    val lines = scala.io.Source.fromFile(design, "UTF-8").getLines().toVector
    if lines.isEmpty then sys.error(s"design file $design is empty")
    val header = lines.head.split(",", -1).map(_.trim).toVector
    val colIdx = header.zipWithIndex.toMap
    for c <- InputCols do
      if !colIdx.contains(c) then
        sys.error(s"design CSV is missing required input column '$c'. Header: ${header.mkString(",")}")

    val body = lines.tail  // one design point per row
    val nRows = body.length

    val startRow = opts.get("start").map(_.toInt).getOrElse(0)
    val endRow   = opts.get("end").map(_.toInt).getOrElse(nRows)
    val start    = math.max(0, startRow)
    val end      = math.min(nRows, endRow)

    // Silence the model's internal println noise so stdout stays clean for logs.
    val realErr = System.err
    def silence[T](b: => T): T =
      val devnull = new java.io.PrintStream((_: Int) => ())
      val prevOut = System.out
      System.setOut(devnull)
      try b finally System.setOut(prevOut)

    val outFile = new File(out)
    Option(outFile.getParentFile).foreach(_.mkdirs())
    val writer = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(outFile), StandardCharsets.UTF_8))

    try
      val headerOut = (Seq("row_index") ++ InputCols ++ OutputCols).mkString(",")
      writer.write(headerOut); writer.newLine(); writer.flush()

      realErr.println(s"[SaltelliRunner] design=$design rows=$nRows processing [$start,$end) seeds=$nSeeds -> $out")
      val t0 = System.currentTimeMillis()

      var i = start
      while i < end do
        val cells = body(i).split(",", -1).map(_.trim).toVector
        val inputs: Map[String, Double] = InputCols.map(c => c -> cells(colIdx(c)).toDouble).toMap

        val res =
          try silence(evalPoint(inputs, nSeeds))
          catch
            case e: Throwable =>
              realErr.println(s"[SaltelliRunner] ROW $i FAILED: ${e.getClass.getSimpleName}: ${e.getMessage}")
              throw e

        val inEcho  = InputCols.map(c => f"${inputs(c)}%.10g")
        val outEcho = OutputCols.map(c => f"${res(c)}%.10g")
        writer.write((Seq(i.toString) ++ inEcho ++ outEcho).mkString(","))
        writer.newLine()
        writer.flush()  // flush each row so a killed task leaves a valid partial file

        val done = i - start + 1
        if done % 10 == 0 || i == end - 1 then
          val elapsed = (System.currentTimeMillis() - t0) / 1000.0
          val rate = if done > 0 then elapsed / done else 0.0
          val elapsedStr = f"$elapsed%.0f"
          val rateStr    = f"$rate%.1f"
          realErr.println(s"[SaltelliRunner] $done/${end - start} rows  ${elapsedStr}s (${rateStr} s/row)")
        i += 1

      realErr.println(s"[SaltelliRunner] DONE rows [$start,$end) -> $out")
    finally
      writer.close()
