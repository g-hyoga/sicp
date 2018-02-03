package scheme.SelfEvaluating.Str

import scheme.SelfEvaluating.SelfEvaluating

class Str(str: String) extends SelfEvaluating {
  override def eval(): String = str
}
