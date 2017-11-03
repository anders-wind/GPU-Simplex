import java.util.Arrays;

public class SimplexInstance {
    final int[][] constraints;
    final int[] coefficients;
    final int[] constants;
    final int variableUpperBound;
    final int variableLowerBound = 0; // for these instances, vars are always non-negative
    private float expectedObjective;

    SimplexInstance(int[][] constraints, int[] constants, int[] coefficients, int variableUpperBound) {
        this.constraints = constraints;
        this.coefficients = coefficients;
        this.constants = constants;
        this.variableUpperBound = variableUpperBound;
    }

    public int getVariableNumber(){
        return coefficients.length;
    }

    public int getConstraintNumber() {
        return constants.length;
    }

    public int getVariableUpperBound() {
        return variableUpperBound;
    }

    public int getVariableLowerBound() {
        return variableLowerBound;
    }

    public float getExpectedObjective() {
        return expectedObjective;
    }

    void setExpectedObjective(float expectedObjective) {
        this.expectedObjective = expectedObjective;
    }

    public String coefficientsString() {
        StringBuilder coefficientsString = new StringBuilder("[");
        for (int i = 0; i < coefficients.length; i++) {
            coefficientsString.append(coefficients[i]);
            if(i < coefficients.length -1 )
                coefficientsString.append(",");
        }
        coefficientsString.append("]");
        return coefficientsString.toString();
    }

    public String constantsString() {
        StringBuilder constantsString = new StringBuilder("[");
        for (int i = 0; i < constants.length; i++) {
            constantsString.append(constants[i]);
            if(i < constants.length -1)
                constantsString.append(",");
        }
        constantsString.append("]");
        return constantsString.toString();
    }

    public String constraintsString(boolean flat) {
        StringBuilder constraintString = new StringBuilder("[");
        for (int i = 0; i < constraints.length; i++) {

            if (!flat)
                constraintString.append("[");
            for (int j = 0; j < constraints[i].length; j++) {
                constraintString.append(constraints[i][j]);
                if (j < constraints[i].length-1)
                    constraintString.append(",");
            }
            if (!flat)
                constraintString.append("]");
            if(i < constraints.length-1)
                constraintString.append(",");

        }
        constraintString.append("]");
        return constraintString.toString();
    }

    public String expectedObjectiveString() {
        return Float.toString(expectedObjective);
    }

    @Override
    public String toString() {
        return String.format("(%d,%d)\n%s\n%s\n%s\n%s",
                getVariableNumber(),
                getConstraintNumber(),
                constraintsString(false),
                constantsString(),
                coefficientsString(),
                expectedObjectiveString()
                );
    }
}
