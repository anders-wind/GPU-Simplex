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
        float[][] constraints = new float[constraintNumber][variableNumber];
        float[] initialCoefficients = new float[variableNumber];
        float[] initialConstants = new float[constraintNumber];

        // Initialize coefficients of the objective function
        for (int i = 0; i < variableNumber; i++) {
            initialCoefficients[i] = random.nextFloat() * 100;
        }

        // initialize constants of the constraints
        for (int i = 0; i < constraintNumber; i++) {
            initialConstants[i] = random.nextFloat() * 100;
        }

        // initialize the coefficients of the constraints
        for (int i = 0; i < constraintNumber; i++) {
            for (int j = 0; j < variableNumber; j++) {
                constraints[i][j] = random.nextFloat() * 100;
            }
        }

        return new SimplexInstance(constraints, initialCoefficients, initialConstants);
    }
}
