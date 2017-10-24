import java.util.Arrays;

public class SimplexInstance {
    final float[][] constraints;
    final float[] coefficients;
    final float[] constants;
    private float expectedObjective;

    SimplexInstance(float[][] constraints, float[] constants, float[] coefficients) {
        this.constraints = constraints;
        this.coefficients = coefficients;
        this.constants = constants;
    }

    public int getVariableNumber(){
        return coefficients.length;
    }

    public int getConstrainNumber(){
        return constants.length;
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
            coefficientsString.append(coefficients[i]).append("f32");
            if(i < coefficients.length -1 )
                coefficientsString.append(",");
        }
        coefficientsString.append("]");
        return coefficientsString.toString();
    }

    public String constantsString() {
        StringBuilder constantsString = new StringBuilder("[");
        for (int i = 0; i < constants.length; i++) {
            constantsString.append(constants[i]).append("f32");
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
                constraintString.append(constraints[i][j]).append("f32");
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
        return expectedObjective + "f32";
    }

    @Override
    public String toString() {
        return String.format("%s\n%s\n%s\n%s",
                constraintsString(false),
                constantsString(),
                coefficientsString(),
                expectedObjectiveString()
                );
    }
}
