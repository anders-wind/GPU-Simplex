import java.util.Arrays;

public class SimplexInstance {
    final float[][] constraints;
    final float[] coefficients;
    final float[] constants;
    final float initialObjective;
    private float expectedObjective;

    public SimplexInstance(float[][] constraints, float initialObjective, float[] coefficients, float[] constants, float expectedObjective) {
        this(constraints, initialObjective, coefficients, constants);
        this.expectedObjective = expectedObjective;
    }

    SimplexInstance(float[][] constraints, float initialObjective, float[] coefficients, float[] constants) {
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

    public float getExpectedObjective() {
        return expectedObjective;
    }

    void setExpectedObjective(float expectedObjective) {
        this.expectedObjective = expectedObjective;
    }


    @Override
    public String toString() {
        StringBuilder constraintString = new StringBuilder("[");
        for (int i = 0; i < constraints.length; i++) {
            constraintString.append(Arrays.toString(constraints[i]));
            if(i < constraints.length-1)
                constraintString.append(", ");
        }
        constraintString.append("]");
        return  Arrays.toString(coefficients) + " " +
                Arrays.toString(constants) + " " +
                constraintString + " " +
                initialObjective + " " +
                expectedObjective;
    }
}
