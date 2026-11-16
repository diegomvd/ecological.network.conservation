package econetcons

/**
 * TEMPORARY: peak-heap probe for a single worst-case Observables.run, to size the
 * per-worker memory footprint for 20-way local parallelism. Delete after sizing.
 */
object MemProbe:
  def main(args: Array[String]): Unit =
    val realOut = System.out
    def silence[T](b: => T): T =
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    val rt = Runtime.getRuntime
    def usedMB(): Double =
      System.gc(); Thread.sleep(50); System.gc()
      (rt.totalMemory() - rt.freeMemory()) / 1e6

    // Worst-case within the LOCKED box: max populations, max species, densest
    // homeRange, low connectance (broad basal base = most survivors/edges), low eps
    // (deep web retained), fine tiles.
    val nSpecies = 300; val conn = 0.05; val nPop = 10000
    val hr = 0.05; val tile = 10; val frac = 0.3; val eps = 0.5

    silence { Observables.run(200, 0.15, 5000, 0.05, 64, 0.0, 0.3, 999L, epsilon = 1.0) } // warm
    val before = usedMB()

    // Track peak across a few worst-case runs on a background sampler.
    @volatile var peak = before
    val sampler = new Thread(() =>
      while !Thread.currentThread().isInterrupted do
        val u = (rt.totalMemory() - rt.freeMemory()) / 1e6
        if u > peak then peak = u
        Thread.sleep(20)
    )
    sampler.setDaemon(true); sampler.start()

    val t0 = System.nanoTime()
    for s <- 1L to 3L do
      silence { Observables.run(nSpecies, conn, nPop, hr, tile, 0.0, frac, s, epsilon = eps) }
    val ms = (System.nanoTime() - t0) / 3e6

    sampler.interrupt()
    val after = usedMB()

    System.err.println(f"[MemProbe worst-case]  S=$nSpecies C=$conn nPop=$nPop hr=$hr tile=$tile eps=$eps")
    System.err.println(f"  baseline heap:      $before%8.1f MB")
    System.err.println(f"  peak during runs:   $peak%8.1f MB")
    System.err.println(f"  per-run delta (~):  ${peak - before}%8.1f MB")
    System.err.println(f"  settled after GC:   $after%8.1f MB")
    System.err.println(f"  mean run time:      $ms%8.1f ms")
    System.err.println(f"  Max heap (-Xmx):    ${rt.maxMemory() / 1e6}%8.1f MB")
