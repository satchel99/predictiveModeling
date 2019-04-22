package tests
import org.scalatest._
import genetics.GeneticAlgorithm
import genetics.geometry._
import genetics.genes._


class TestLinearRegression extends FunSuite {
	test("testing linear regreszsion") {
		val testPoints: List[Point] = List(new Point(9, 260), new Point(13, 320), new Point(21, 420), new Point(30, 530), new Point(31, 560), new Point(31, 550), new Point(34, 590), new Point(25, 500), new Point(28, 560), new Point(20, 440), new Point(5, 300));
        
        var result: Line = GeneticAlgorithm.linearRegression(testPoints);
        
        assert((result.slope > 11 && result.slope < 14), result);
		
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
