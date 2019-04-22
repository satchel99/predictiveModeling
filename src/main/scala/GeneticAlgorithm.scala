package genetics
import genetics.geometry._
import genetics.genes._
import scala.collection.mutable.ListBuffer


object GeneticAlgorithm {
    

    def geneticAlgorithm[T](f : T => Double, makeObj: List[Gene] => T, sampleGene : List[Gene]) : T = {
      
        var generation : List[Animal] = generateTwenty(sampleGene);
        var mostFit : Animal = generation(0)
        var mostFitVal : Double = f(makeObj(generation(0).genes))
        var secondMostFit : Animal = generation(0)
        var secondMostFitVal : Double = f( makeObj(generation(0).genes))
        for(animal <- generation) {
            animal.fitness = f(makeObj(animal.genes))
        }
        for(a <- 1 to 10000) {
            generation = onegen[T](generation, sampleGene, f, makeObj)
        }
        generation = sortAnimalsPrimary(generation)
        println(generation(generation.length - 1))
        //println(generation)
        return makeObj(generation(generation.length - 1).genes); 
    }
    
    def onegen[T](gen : List[Animal], sampleGene : List[Gene], f : T => Double, makeObj: List[Gene] => T) : List[Animal] = {
        var generation : List[Animal]= sortAnimalsPrimary(gen)
        var newGeneration : List[Animal] = List[Animal]()
        var firstBest : Animal = generation(generation.length - 1)
        var secondBest : Animal = generation(generation.length - 2)
        newGeneration =  firstBest :: newGeneration
        newGeneration =  mutateAnimal(firstBest) :: newGeneration
        newGeneration =  mutateAnimal(firstBest) :: newGeneration
        newGeneration =  mutateAnimal(secondBest) :: newGeneration
        var children : List[Animal] = produceChildren(firstBest, secondBest, generation(generation.length - 3), generation(generation.length - 4))
        for(child <- children) {
            newGeneration =  child :: newGeneration
        }
        var randomTen : List[Animal] = generateTwenty(sampleGene);
        //randomTen = sortAnimals(randomTen)
        for(a <- 0 to 9) {
            newGeneration = randomTen(a) :: newGeneration
        }
        for(animal <- newGeneration) {
            animal.fitness = f(makeObj(animal.genes))
        }
        //println(newGeneration)
        return newGeneration
    }
    
    def sortAnimalsPrimary(animals : List[Animal]) : List[Animal]  = {
        var animals_temp : ListBuffer[Animal] = animals.to[ListBuffer]
        var newList : List[Animal] = List[Animal]();
        for(i <- 0 to animals_temp.length-1) {
            var index : Int = findMin(animals_temp)
            newList =  newList :+ animals_temp(index)
            animals_temp.remove(index)
        }
        return newList
    }
    
    
    def fitnessFunctionTheRight(points : List[Point], line : Line) : Double = {
        var total_sum : Double = 0.0;
        for (point <- points) {
            //println(point.y + " " + (line.evaluate(point.x)))
            total_sum = total_sum + Math.abs(line.evaluate(point.x) - point.y);
        }
        val new_one : Double = (Math.atan(total_sum))/(Math.PI/2)
        return (1 - new_one)
    }
    
    def fitnessFunction(points : List[Point], line : Polynomial) : Double = {
        var total_sum : Double = 0.0;
        for (point <- points) {
            //println(point.y + " " + (line.evaluate(point.x)))
            total_sum = total_sum + Math.abs(line.evaluate(point.x) - point.y);
        }
        val new_one : Double = (Math.atan(total_sum))/(Math.PI/2)
        return (1 - new_one)
    }
    
    
    def generateTwenty(sampleGene : List[Gene]) : List[Animal] = {
        var animals : List[Animal] = List[Animal]();
        for( a <- 1 to 20){
            var genes : List[Gene] = List[Gene]();
            for(i <- sampleGene) {
                var rand : Double = scala.util.Random.nextDouble()
                genes = (new Gene(rand)) :: genes
            }
            animals = (new Animal(genes)) :: animals
        }
        return animals
    }
    
    def mutateGene(gene : Gene) : Gene = {
        var scale : Double = gene.geneValue/100;
        var rand : Double = scala.util.Random.nextDouble()
        rand = rand * scale
        return new Gene(gene.geneValue + rand) 
    }
    
    def mutateAnimal(animal : Animal) : Animal = {
        var newgenes : List[Gene] = List[Gene]();
        for(gene <- animal.genes) {
            newgenes =  newgenes :+ mutateGene(gene)
        }
        return new Animal(newgenes)
    }
    
    def makeChild(parent1: Animal, parent2: Animal) : Animal = {
        var newgenes : List[Gene] = List[Gene]();
        for(a <- 0 to parent1.genes.length-1) {
            var val1 : Double = parent1.genes(a).geneValue
            var val2 : Double = parent2.genes(a).geneValue
            var ave : Double = (val1 + val2)/2
            newgenes =  newgenes :+ (new Gene(ave))
        }
        return new Animal(newgenes)
    }
    
    def produceChildren(animal1 : Animal, animal2 : Animal, animal3 : Animal, animal4 : Animal) : List[Animal] = {
        
        var animals : List[Animal] = List[Animal]();
        animals = makeChild(animal1, animal2) :: animals
        animals = makeChild(animal1, animal3) :: animals
        animals = makeChild(animal1, animal4) :: animals
        animals = makeChild(animal2, animal3) :: animals
        animals = makeChild(animal2, animal4) :: animals
        animals = makeChild(animal3, animal4) :: animals
        return animals  
    }
    
    def linearRegression(points : List[Point]) : Line = {
        def fitFunc(line : Line) : Double = {
            return fitnessFunctionTheRight(points, line);
        }
        return geneticAlgorithm[Line](fitFunc, makeLine, List[Gene](new Gene(0.1), new Gene(0.2)))
    }
    
    def findMin(animals : ListBuffer[Animal]) : Int = {
        var minFit : Double = animals(0).fitness;
        var minAnimal : Int = 0
        for(a <- 0 to animals.length-1) {
            var animal : Animal = animals(a)
            if(animal.fitness < minFit) {
                minFit = animal.fitness;
                minAnimal = a
            }
        }
        return minAnimal
    }
    
    
    def makeLine(genes : List[Gene]) : Line = {
        var newSlope : Double = Math.tan((genes(0).geneValue - 0.5) * Math.PI)
        var newInt : Double = Math.tan((genes(1).geneValue - 0.5) * Math.PI)
        return new Line(newSlope, newInt); 
    }
    def makePoly(genes : List[Gene]) : Polynomial = {
        var coef : List[Double] = List[Double]();
        for(gene <- genes) {
            coef = coef :+ Math.tan((gene.geneValue - 0.5) * Math.PI)
        }
        return new Polynomial(coef);
    }
    
    def polynomialRegression(points : List[Point], degree : Int) : Polynomial = {
        var sampleGene : List[Gene] = List[Gene]();
        for(a <- 0 to degree) {
            sampleGene = new Gene(0.1) :: sampleGene
        }
        def fitFunc(line : Polynomial) : Double = {
            return fitnessFunction(points, line);
        }
        return geneticAlgorithm[Polynomial](fitFunc, makePoly, sampleGene)
        
    }
    
    
    //so create a function that takes points, and
    
    def main(args : Array[String]) : Unit = {
       val testPoints3: List[Point] = List(new Point(9, 260), new Point(13, 320), new Point(21, 420), new Point(30, 530), new Point(31, 560), new Point(31, 550), new Point(34, 590), new Point(25, 500), new Point(28, 560), new Point(20, 440), new Point(5, 300));
       val testPoints: List[Point] = List(new Point(1, 2), new Point(2, 5), new Point(3, 10), new Point(4, 17), new Point(5, 26), new Point(6, 37), new Point(7, 50), new Point(8, 65));
       val testPoints2: List[Point] = List(new Point(-1, 30), new Point(0, 20), new Point(1, 10), new Point(2, 0), new Point(3, -10), new Point(4, -20), new Point(5, -30), new Point(6, -40), new Point(7, -50), new Point(8, -60), new Point(9, -70));
       var generation : List[Animal] = generateTwenty(List[Gene](new Gene(0.1), new Gene(0.2)));
       println(generation.length)
       for(animal <- generation) {
           animal.fitness = scala.util.Random.nextDouble()
       }
       generation = sortAnimalsPrimary(generation)
       //println(generation)
       //println(linearRegression(testPoints))
       //val testPoly: List[Point] = List(new Point(-1, -1), new Point(0, 3), new Point(1, 2.5), new Point(2, 5), new Point(3, 4), new Point(5, 2), new Point(7, 5), new Point(9, 4));
        //println(polynomialRegression(testPoly, 2))
        println(linearRegression(testPoints2))
        println(polynomialRegression(testPoints, 2))
        println(linearRegression(testPoints3))
        
    }    
    
}
