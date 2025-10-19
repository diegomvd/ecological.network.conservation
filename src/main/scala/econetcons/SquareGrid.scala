package econetcons

import org.locationtech.jts.geom.Coordinate

/**
 * Square lattice with periodic boundaries - much simpler than polygons!
 * @param sideLength number of cells along each side
 */
case class SquareGrid(sideLength: Int) {
  
  def totalCells: Int = sideLength * sideLength
  
  // Pre-compute Moore neighbors (8-connected) for each cell
  private val neighborCache: Map[Int, Seq[Int]] = {
    (0 until totalCells).map { cellId =>
      cellId -> computeMooreNeighbors(cellId)
    }.toMap
  }
  
  /**
   * Get Moore neighborhood (8 surrounding cells) - O(1) lookup
   */
  def neighbors(cellId: Int): Seq[Int] = {
    neighborCache(cellId)
  }
  
  /**
   * Compute the 8 Moore neighbors with periodic boundaries
   */
  private def computeMooreNeighbors(cellId: Int): Seq[Int] = {
    val (centerCol, centerRow) = cellIdToCoord(cellId)
    
    val result = for {
      dRow <- -1 to 1
      dCol <- -1 to 1
      if !(dRow == 0 && dCol == 0) // Exclude center cell
    } yield {
      val wrappedCol = ((centerCol + dCol) % sideLength + sideLength) % sideLength
      val wrappedRow = ((centerRow + dRow) % sideLength + sideLength) % sideLength
      wrappedRow * sideLength + wrappedCol
    }
    
    result.toSeq
  }
  
  /**
   * Convert coordinate to cell ID using simple arithmetic - no polygon needed!
   */
  def coordToCell(coord: Coordinate): Int = {
    val col = math.floor(coord.x * sideLength).toInt.min(sideLength - 1)
    val row = math.floor(coord.y * sideLength).toInt.min(sideLength - 1)
    row * sideLength + col
  }
  
  /**
   * Get center coordinate of a cell
   */
  def getCellCenter(cellId: Int): Coordinate = {
    val (col, row) = cellIdToCoord(cellId)
    val cellSize = 1.0 / sideLength.toDouble
    new Coordinate(
      (col + 0.5) * cellSize,
      (row + 0.5) * cellSize,
      0.0
    )
  }
  
  
  /**
   * All cell IDs as a simple sequence - no polygons needed
   */
  def cellIndices: Seq[Int] = 0 until totalCells
  
  // ========== PRIVATE METHODS ==========
  
  private def cellIdToCoord(cellId: Int): (Int, Int) = {
    val row = cellId / sideLength
    val col = cellId % sideLength
    (col, row)
  }
}

object SquareGrid {
  
  /**
   * Create grid from total area (equivalent to HexagonalGrid.radius)
   */
  def fromArea(totalCells: Int): SquareGrid = {
    val sideLength = math.ceil(math.sqrt(totalCells)).toInt
    SquareGrid(sideLength)
  }
  
  /**
   * Convert coordinate to wrapped version in [0,1)²
   * This is the magic of periodic boundaries!
   */
  def wrapCoordinate(coord: Coordinate): Coordinate = {
    // Extract fractional part - this handles negative coordinates too
    val wrappedX = coord.x - math.floor(coord.x)
    val wrappedY = coord.y - math.floor(coord.y)
    new Coordinate(wrappedX, wrappedY, 0.0)
  }
  
  /**
   * Calculate shortest distance considering periodic boundaries
   */
  def periodicDistance(coord1: Coordinate, coord2: Coordinate): Double = {
    val dx = math.abs(coord1.x - coord2.x)
    val dy = math.abs(coord1.y - coord2.y)
    
    // Consider wrapping - choose shorter path
    val wrappedDx = math.min(dx, 1.0 - dx)
    val wrappedDy = math.min(dy, 1.0 - dy)
    
    math.sqrt(wrappedDx * wrappedDx + wrappedDy * wrappedDy)
  }
  
  /**
   * Generate random point in [0,1)² - 100% efficiency!
   */
  def randomPoint(rnd: scala.util.Random): Coordinate = {
    new Coordinate(rnd.nextDouble(), rnd.nextDouble(), 0.0)
  }
}