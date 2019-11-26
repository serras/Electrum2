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
 * @author Nuno Macedo // [HASLab] electrum-colorful
 */

public class ErrorColor extends Err {


    /** This ensures this class can be serialized reliably. */
    private static final long serialVersionUID = 0;

    /**
     * Constructs a new color marked error.
     *
     * @param msg - the actual error message (can be null)
     */
    public ErrorColor(String msg) {
        super(null, msg, null);
    }

    /**
     * Constructs a new color marked error with "cause" as the underlying cause.
     *
     * @param msg - the actual error message (can be null)
     * @param cause - if nonnull, it is the cause of this exception
     */
    public ErrorColor(String msg, Throwable cause) {
        super(null, msg, cause);
    }

    /**
     * Constructs a new color marked error.
     *
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message (can be null)
     */
    public ErrorColor(Pos pos, String msg) {
        super(pos, msg, null);
    }

    /** Returns a textual description of the error. */
    @Override
    public String toString() {
        if (pos == Pos.UNKNOWN)
            return "Feature paint error:\n" + msg;
        if (pos.filename.length() > 0)
            return "Feature paint error in " + pos.filename + " at line " + pos.y + " column " + pos.x + ":\n" + msg;
        return "Feature paint error at line " + pos.y + " column " + pos.x + ":\n" + msg;
    }
}
