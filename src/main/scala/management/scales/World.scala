package management.scales

case class World(
                radius: Double,
                numberOfSpecies: Long,
                connectanceMetaWeb: Double,
                populationsDensity: Double,
                managementScale: Double,
                managementClustering: Double,
                populations: List[Population]
                )
