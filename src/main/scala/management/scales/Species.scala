package management.scales

case class Species(
                    id: Long,
                    trophicLevel: Int,
                    degree: Int,
                    abundance: Double,
                    homeRange: Double
                  )

object Species:

  def apply(
            id: Long,
            trophicLevel: Int,
            degree: Int
           ):
  Species =
    val (abundance, homeRange)  = speciesTraits(trophicLevel, degree)
    Species(id, trophicLevel, degree,abundance = abundance, homeRange = homeRange)

  private def speciesTraits(trophicLevel: Int, degree: Int): (Double,Double) =
    (trophicLevel, degree)

end Species
