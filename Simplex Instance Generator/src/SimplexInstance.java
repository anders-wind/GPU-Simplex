import java.util.Arrays;

public class SimplexInstance {
    final double[][] constraints;
    final double[] coefficients;
    final double[] constants;
    final double initialObjective;
    private double expectedObjective;

    public SimplexInstance(double[][] constraints, double initialObjective, double[] coefficients, double[] constants, double expectedObjective) {
        this(constraints, initialObjective, coefficients, constants);
        this.expectedObjective = expectedObjective;
    }

    SimplexInstance(double[][] constraints, double initialObjective, double[] coefficients, double[] constants) {
        this.constraints = constraints;
        this.initialObjective = initialObjective;
        this.coefficients = coefficients;
        this.constants = constants;
    }

    public int getVariableNumber(){
        return coefficients.length;
    }

    public int getConstrainNumber(){
        return constants.length;
    }

    public double getExpectedObjective() {
        return expectedObjective;
    }

    void setExpectedObjective(double expectedObjective) {
        this.expectedObjective = expectedObjective;
    }


    @Override
    public String toString() {
        String constraintString = "[";
        for (double[] constraint : constraints) {
            constraintString += Arrays.toString(constraint) + ", ";
        }
        constraintString += "]";
        return  constraintString + " " +
                Arrays.toString(coefficients) + " " +
                Arrays.toString(constants) + " " +
                initialObjective + " " +
                expectedObjective;
    }
}
