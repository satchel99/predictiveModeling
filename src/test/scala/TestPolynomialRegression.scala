package tests
import org.scalatest._
import genetics.GeneticAlgorithm
import genetics.geometry._
import genetics.genes._


class TestPolynomialRegression extends FunSuite {
	test("testing linear regreszsion") {
		val testPoints: List[Point] = List(new Point(-1, -1), new Point(0, 3), new Point(1, 2.5), new Point(2, 5), new Point(3, 4), new Point(5, 2), new Point(7, 5), new Point(9, 4));
        
        var result: Polynomial = GeneticAlgorithm.polynomialRegression(testPoints, 2);
        
        assert((result.coefficients(0) > 1) && (result.coefficients(0) < 2), result);
		
	}
}



/*


start by generating 10 lines.

score the best fit.

pick the best.

mutate some.

add more. 

keep picking the best. 

OKAY OKAY OKAY


*/
