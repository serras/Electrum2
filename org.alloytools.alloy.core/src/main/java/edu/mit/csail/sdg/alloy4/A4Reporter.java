/* Alloy Analyzer 4 -- Copyright (c) 2006-2009, Felix Chang
 * Electrum -- Copyright (c) 2015-present, Nuno Macedo
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

/**
 * This class receives diagnostic, progress, and warning messages from Alloy4.
 * (This default implementation ignores all calls; you should subclass it to do
 * the appropriate screen output)
 *
 * @modified [electrum] the solving process now reports in which step of the
 *           temporal analysis is (reported info should regard that step only)
 *           and the decompose strategy; translation also reports temporal
 *           parameters (max/min trace length)
 */

public class A4Reporter {

    /**
     * If nonnull, then we will forward requests to this reporter.
     */
    private final A4Reporter       parent;

    /**
     * This is a pre-constructed instance that simply ignores all calls.
     */
    public static final A4Reporter NOP = new A4Reporter();

    /**
     * Constructs a default A4Reporter object that does nothing.
     */
    public A4Reporter() {
        parent = null;
    }

    /**
     * Constructs an A4Reporter that forwards each method to the given A4Reporter.
     */
    public A4Reporter(A4Reporter reporter) {
        parent = reporter;
    }

    /**
     * This method is called at various points to report the current progress; it is
     * intended as a debugging aid for the developers; the messages are generally
     * not useful for end users.
     */
    public void debug(String msg) {
        if (parent != null)
            parent.debug(msg);
    }

    /**
     * This method is called by the parser to report parser events.
     */
    public void parse(String msg) {
        if (parent != null)
            parent.parse(msg);
    }

    /**
     * This method is called by the typechecker to report the type for each
     * field/function/predicate/assertion, etc.
     */
    public void typecheck(String msg) {
        if (parent != null)
            parent.typecheck(msg);
    }

    /**
     * This method is called by the typechecker to report a nonfatal type error.
     */
    public void warning(ErrorWarning msg) {
        if (parent != null)
            parent.warning(msg);
    }

    /**
     * This method is called by the ScopeComputer to report the scope chosen for
     * each sig.
     */
    public void scope(String msg) {
        if (parent != null)
            parent.scope(msg);
    }

    /**
     * This method is called by the BoundsComputer to report the bounds chosen for
     * each sig and each field.
     */
    public void bound(String msg) {
        if (parent != null)
            parent.bound(msg);
    }

    /**
     * This method is called by the translator just before it begins generating CNF.
     *
     * @param strat - the decomposed solving strategy (disabled, parallel, hybrid)
     * @param solver - the solver chosen by the user (eg. SAT4J, MiniSat...)
     * @param bitwidth - the integer bitwidth chosen by the user
     * @param maxseq - the scope on seq/Int chosen by the user
     * @param mintrace - the minimum trace length
     * @param maxtrace - the maximum trace length
     * @param skolemDepth - the skolem function depth chosen by the user (0, 1,
     *            2...)
     * @param symmetry - the amount of symmetry breaking chosen by the user (0...)
     * @param strat - selected decompose solving strategy
     */
    public void translate(String solver, int bitwidth, int maxseq, int mintrace, int maxtrace, int skolemDepth, int symmetry, String strat) {
        if (parent != null)
            parent.translate(solver, bitwidth, maxseq, mintrace, maxtrace, skolemDepth, symmetry, strat);
    }

    /**
     * This method is called by the translator just after it generated the CNF.
     *
     * @param plength - the prefix length considered for this analysis
     * @param primaryVars - the total number of primary variables
     * @param totalVars - the total number of variables including the number of
     *            primary variables
     * @param clauses - the total number of clauses
     */
    public void solve(int plength, int primaryVars, int totalVars, int clauses) {
        if (parent != null)
            parent.solve(plength, primaryVars, totalVars, clauses);
    }

    /**
     * If solver==KK or solver==CNF, this method is called by the translator after
     * it constructed the Kodkod or CNF file.
     *
     * @param filename - the Kodkod or CNF file generated by the translator
     */
    public void resultCNF(String filename) {
        if (parent != null)
            parent.resultCNF(filename);
    }

    /**
     * If solver!=KK and solver!=CNF, this method is called by the translator if the
     * formula is satisfiable.
     *
     * @param command - this is the original Command used to generate this solution
     * @param solvingTime - this is the number of milliseconds the solver took to
     *            obtain this result
     * @param solution - the satisfying A4Solution object
     */
    public void resultSAT(Object command, long solvingTime, Object solution) {
        if (parent != null)
            parent.resultSAT(command, solvingTime, solution);
    }

    /**
     * If solver!=KK and solver!=CNF, this method is called by the translator before
     * starting the unsat core minimization.
     *
     * @param command - this is the original Command used to generate this solution
     * @param before - the size of the unsat core before calling minimization
     */
    public void minimizing(Object command, int before) {
        if (parent != null)
            parent.minimizing(command, before);
    }

    /**
     * If solver!=KK and solver!=CNF, this method is called by the translator after
     * performing the unsat core minimization.
     *
     * @param command - this is the original Command used to generate this solution
     * @param before - the size of the unsat core before calling minimization
     * @param after - the size of the unsat core after calling minimization
     */
    public void minimized(Object command, int before, int after) {
        if (parent != null)
            parent.minimized(command, before, after);
    }

    /**
     * If solver!=KK and solver!=CNF, this method is called by the translator if the
     * formula is unsatisfiable.
     *
     * @param command - this is the original Command used to generate this solution
     * @param solvingTime - this is the number of milliseconds the solver took to
     *            obtain this result
     * @param solution - the unsatisfying A4Solution object
     */
    public void resultUNSAT(Object command, long solvingTime, Object solution) {
        if (parent != null)
            parent.resultUNSAT(command, solvingTime, solution);
    }

    /**
     * This method is called by the A4SolutionWriter when it is writing a particular
     * sig, field, or skolem.
     */
    public void write(Object expr) {
        if (parent != null)
            parent.write(expr);
    }
}
