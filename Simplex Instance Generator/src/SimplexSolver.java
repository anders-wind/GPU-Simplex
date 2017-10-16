import ilog.concert.IloException;
import ilog.concert.IloLinearNumExpr;
import ilog.concert.IloNumVar;
import ilog.cplex.IloCplex;

public class SimplexSolver {

    public double solveSimplex(SimplexInstance instance) {
        double expectedObjective = instance.initialObjective;
        int variableNumber = instance.getVariableNumber(), constraintNumber = instance.getConstrainNumber();
        try {
            IloCplex cplex = new IloCplex();
            IloNumVar[] x =  cplex.boolVarArray(variableNumber);

            // Add the objective function
            IloLinearNumExpr obj = cplex.linearNumExpr();
            for (int i = 0; i < variableNumber; i++) {
                obj.addTerm(instance.coefficients[i], x[i]);
            }
            cplex.addMaximize();

            // Add the constraints
            IloLinearNumExpr expr;
            for (int i = 0; i < constraintNumber; i++) {
                expr = cplex.linearNumExpr();
                for (int j = 0; j < variableNumber; j++) {
                    expr.addTerm(instance.constraints[i][j], x[j]);
                }
                cplex.le(expr, instance.constants[i]);
            }

            // Solve the formulation
            boolean solved = cplex.solve();
            if(solved){
                expectedObjective += cplex.getBestObjValue();
            }

        } catch (IloException e) {
            e.printStackTrace();
        }
        return expectedObjective;
    }
}