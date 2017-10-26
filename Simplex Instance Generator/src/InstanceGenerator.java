import java.util.Random;

public class InstanceGenerator {
    private Random random = new Random();
    private SimplexSolver simplexSolver = new SimplexSolver();

    public SimplexInstance generate(int variableNumber, int constraintNumber){
        SimplexInstance instance = initializeFormulation(variableNumber, constraintNumber);

        float expectedObjective = simplexSolver.solveSimplex(instance);
        instance.setExpectedObjective(expectedObjective);

        return instance;
    }

    private SimplexInstance initializeFormulation(int variableNumber, int constraintNumber) {
        int[][] constraints = new int[constraintNumber][variableNumber];
        int[] initialCoefficients = new int[variableNumber];
        int[] initialConstants = new int[constraintNumber];

        // Initialize coefficients of the objective function
        for (int i = 0; i < variableNumber; i++) {
            initialCoefficients[i] = random.nextInt(101);
        }

        // initialize constants of the constraints
        for (int i = 0; i < constraintNumber; i++) {
            initialConstants[i] = random.nextInt(101);
        }

        // initialize the coefficients of the constraints
        for (int i = 0; i < constraintNumber; i++) {
            for (int j = 0; j < variableNumber; j++) {
                constraints[i][j] = random.nextInt(101);
            }
        }
        return new SimplexInstance(constraints, initialConstants, initialCoefficients);
    }
}
