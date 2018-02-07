package scheme

trait Environment {
  val variables: Map[String, Expression]
}
