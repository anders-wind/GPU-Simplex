import ilog.concert.IloException;
import ilog.concert.IloLinearNumExpr;
import ilog.concert.IloNumVar;
import ilog.cplex.IloCplex;

public class SimplexSolver {

    public float solveSimplex(SimplexInstance instance, Benchmark.Timer timer) {
        float expectedObjective = 0;
        int variableNumber = instance.getVariableNumber();
        int constraintNumber = instance.getConstraintNumber();
        int variableUpperBound = instance.getVariableUpperBound();
        int variableLowerBound = instance.getVariableLowerBound();
        try {
            IloCplex cplex = new IloCplex();
            cplex.setOut(null);
            IloNumVar[] x =  cplex.numVarArray(
                    variableNumber,variableLowerBound,variableUpperBound);

            // Add the objective function
            IloLinearNumExpr obj = cplex.linearNumExpr();
            for (int i = 0; i < variableNumber; i++) {
                obj.addTerm(instance.coefficients[i], x[i]);
            }
            cplex.addMaximize(obj);

            // Add the constraints
            IloLinearNumExpr expr;
            for (int i = 0; i < constraintNumber; i++) {
                expr = cplex.linearNumExpr();
                for (int j = 0; j < variableNumber; j++) {
                    expr.addTerm(instance.constraints[i][j], x[j]);
                }
                cplex.addLe(expr, instance.constants[i]);
            }

            // Solve the formulation
            if (timer != null) timer.play();
            boolean solved = cplex.solve();
            if (timer != null) timer.pause();
            if (solved) {
                if(cplex.getCplexStatus().equals(IloCplex.CplexStatus.Infeasible))
                    System.err.println("Was infeasible");
                else if(cplex.getCplexStatus().equals(IloCplex.CplexStatus.Unbounded))
                    System.err.println("Was unbounded");
                expectedObjective += cplex.getObjValue();
            }

        } catch (IloException e) {
            e.printStackTrace();
        }
        return expectedObjective;
    }

    public float solveSimplex(SimplexInstance instance) {
        return solveSimplex(instance, null);
    }
}
