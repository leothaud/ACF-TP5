package simplifier.Leothaud_Rodet_TP5

import library._

class MySimplifier extends Simplifier{
	def simplify(p: List[Symbol]): List[Symbol] = {
		p match {
			case Nil => Nil
			case Star :: Star :: next => simplify(Star :: next)
			case Star :: Qmark :: next => simplify(Plus :: next)
			case Qmark :: Star :: next => simplify(Plus :: next)
			case Star :: Plus :: next => simplify(Plus :: next)
			case Plus :: Star :: next => simplify(Plus :: next)
			case head :: next => head :: simplify(next)
		}
	}

}