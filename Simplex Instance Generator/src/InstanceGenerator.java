import java.util.Random;

public class InstanceGenerator {
    private Random random = new Random();

    public SimplexInstance generate(int variableLowNumber,
            int variableHighNumber, int constraintLowNumber, int constraintHighNumber) {

        int variableNumber = random.nextInt(variableHighNumber-variableLowNumber+1)
            + variableLowNumber;
        int constraintNumber = random.nextInt(constraintHighNumber-constraintLowNumber+1)
            + constraintLowNumber;

        SimplexInstance instance = initializeFormulation(variableNumber, constraintNumber);
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
