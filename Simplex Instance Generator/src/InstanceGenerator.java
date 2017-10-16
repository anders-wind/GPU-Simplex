import java.util.Random;

public class InstanceGenerator {
    private Random random = new Random();
    private SimplexSolver simplexSolver = new SimplexSolver();

    public SimplexInstance generate(int variableNumber, int constraintNumber){
        SimplexInstance instance = initializeFormulation(variableNumber, constraintNumber);

        double expectedObjective = simplexSolver.solveSimplex(instance);
        instance.setExpectedObjective(expectedObjective);

        return instance;
    }

    private SimplexInstance initializeFormulation(int variableNumber, int constraintNumber) {
        double[][] constraints = new double[constraintNumber][variableNumber];
        double[] initialCoefficients = new double[variableNumber];
        double[] initialConstants = new double[constraintNumber];

        // initialize the initial objective value.
        double initialObjective = random.nextDouble()*100;

        // Initialize coefficients of the objective function
        for (int i = 0; i < variableNumber; i++) {
            initialCoefficients[i] = random.nextDouble() * 100;
        }

        // initialize constants of the constraints
        for (int i = 0; i < constraintNumber; i++) {
            initialConstants[i] = random.nextDouble() * 100;
        }

        // initialize the coefficients of the constraints
        for (int i = 0; i < constraintNumber; i++) {
            for (int j = 0; j < variableNumber; j++) {
                constraints[i][j] = random.nextDouble() * 100;
            }
        }

        return new SimplexInstance(constraints, initialObjective, initialCoefficients, initialConstants);
    }
}