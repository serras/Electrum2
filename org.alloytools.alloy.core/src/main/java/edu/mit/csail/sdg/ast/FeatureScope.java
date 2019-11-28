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

package edu.mit.csail.sdg.ast;

import java.util.List;

import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pos;

/**
 * Immutable; represents a feature scope in a "run" or "check" command.
 * <p>
 * <b>Invariant:</b> -9 >= feats <= -1 || 1 >= feats <= 9
 * <p>
 *
 * @author Nuno Macedo // [HASLab] electrum-features
 */
public class FeatureScope {

    /**
     * The position in the original source file where this scope was declared; can
     * be Pos.UNKNOWN if unknown.
     */
    public final Pos           pos;

    /**
     * The list of positive/negative features for this scope.
     */
    public final List<Integer> feats;

    /** True iff the scope is an exact scope. */
    public final boolean       isExact;

    /**
     * Construct a new FeatureScope object.
     *
     * @param pos - the position where this scope is given
     * @param isExact - true iff the scope is intended to be exact
     * @param feats - the selected features
     * @throws ErrorSyntax if features outside available
     */
    // TODO: this needs additional type checking
    public FeatureScope(Pos pos, boolean isExact, List<Integer> feats) throws ErrorSyntax {
        if (pos == null)
            pos = Pos.UNKNOWN;
        if (feats == null)
            throw new NullPointerException();
        if (feats.stream().filter(f -> f < -9 || f > 9 || f == 0).count() > 0)
            throw new ErrorSyntax(pos, "Features must be within 1 and 9: " + feats);
        for (Integer f : feats)
            if (feats.contains(-f))
                throw new ErrorSyntax(pos, "Negative and positive of same feature: " + f);
        this.pos = pos;
        this.isExact = isExact;
        this.feats = feats;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return (isExact ? "exactly " : "") + feats;
    }
}
