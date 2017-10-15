import java.util.Arrays;

public class Instance {
    public final int[] nonBasic;
    public final int[] basic;
    public final double[][] constraints;
    public final double initialObjective;
    public final double[] initialCoefficients;
    public final double[] initialConstants;
    public final double expectedObjective;

    public Instance(int[] nonBasic, int[] basic, double[][] constraints, double initialObjective, double[] initialCoefficients, double[] initialConstants, double expectedObjective) {
        this.nonBasic = nonBasic;
        this.basic = basic;
        this.constraints = constraints;
        this.initialObjective = initialObjective;
        this.initialCoefficients = initialCoefficients;
        this.initialConstants = initialConstants;
        this.expectedObjective = expectedObjective;
    }

    @Override
    public String toString() {
        String constraintString = "[";
        for (double[] constraint : constraints) {
            constraintString += Arrays.toString(constraint) + ", ";
        }
        constraintString += "]";
        return  Arrays.toString(nonBasic) + " " +
                Arrays.toString(basic) + " " +
                constraintString + " " +
                initialObjective + " " +
                Arrays.toString(initialCoefficients) + " " +
                Arrays.toString(initialConstants) + " " +
                expectedObjective;
    }
}
