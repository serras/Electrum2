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

package edu.mit.csail.sdg.translator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.function.Function;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.Attr;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprHasName;
import edu.mit.csail.sdg.ast.ExprITE;
import edu.mit.csail.sdg.ast.ExprLet;
import edu.mit.csail.sdg.ast.ExprList;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.FeatureScope;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Module;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.ast.Sig.PrimSig;
import edu.mit.csail.sdg.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.ast.VisitReturn;
import edu.mit.csail.sdg.parser.Macro;

/**
 * author Nuno Macedo // [HASLab] electrum-features
 */

public final class TranslateColorfulToAlloy extends VisitReturn<Expr> {

    public static final String feature_label = "_Feature";
    public static final String variant_label = "_Variant";

    public static Pair<Command,Pair<Collection<Sig>,Iterable<Func>>> expandColors(Module world, Command cmd) {
        Map<Object,Set<Integer>> decls = new HashMap<Object,Set<Integer>>();

        TranslateColorfulToAlloy trans = new TranslateColorfulToAlloy();

        for (Sig s : world.getAllReachableSigs())
            trans.resolveSig(s);

        for (Sig s : world.getAllReachableSigs())
            trans.resolveFields(s);

        trans.resolveFunc(world.getAllFunc());

        trans.resolveMacro(world.getAllMacro());

        trans.resolveAssert(world.getAllAssertions());

        trans.processFeatScope(cmd.feats);

        Expr nf = cmd.formula.accept(trans);
        Command new_cmd = cmd.change(nf.and(ExprList.make(Pos.UNKNOWN, Pos.UNKNOWN, ExprList.Op.AND, trans.extraFacts)));
        Set<Sig> all_sigs = new HashSet<Sig>(trans.oldsig2new.values());
        Iterable<Func> all_funcs = new SafeList<>(trans.oldfunc2new.values());
        all_sigs.addAll(trans.used_feats.values());
        all_sigs.add(trans.feature_sig);
        all_sigs.add(trans.variant_sig);
        return new Pair<>(new_cmd, new Pair<>(all_sigs, all_funcs));
    }

    private final Map<Object,Set<Integer>>     decls;
    private final Stack<Set<Integer>>          context;
    private final Map<Sig,Sig>                 oldsig2new;
    private final Map<Field,Field>             oldfield2new;
    private final Map<Func,Func>               oldfunc2new;
    private final Map<ExprHasName,ExprHasName> oldvar2new;
    private final List<Expr>                   extraFacts;
    private final Map<Integer,Sig>             used_feats;
    private final PrimSig                      feature_sig = new PrimSig(feature_label, Attr.ABSTRACT, Attr.PRIVATE);
    private final SubsetSig                    variant_sig = new SubsetSig(variant_label, Collections.singleton(feature_sig), Attr.PRIVATE);

    private TranslateColorfulToAlloy() {
        this.decls = new HashMap<Object,Set<Integer>>();
        this.context = new Stack<Set<Integer>>();
        this.oldsig2new = new HashMap<Sig,Sig>();
        this.oldfield2new = new HashMap<Field,Field>();
        this.oldvar2new = new HashMap<ExprHasName,ExprHasName>();
        this.oldfunc2new = new HashMap<Func,Func>();
        this.extraFacts = new ArrayList<Expr>();
        this.used_feats = new HashMap<Integer,Sig>();
    }

    /**
     * Creates an expression representing the selected positive/negative marks. Will
     * create and store feature sigs as needed.
     *
     * @param feats a set of presence/absence conditions
     * @return the corresponding formula
     */
    private Expr presenceCondition(Collection<Integer> feats) {
        List<Expr> args = new ArrayList<Expr>();
        for (int i : feats) {
            Sig s = used_feats.get(Math.abs(i));
            if (s == null) {
                s = new PrimSig(feature_label + Math.abs(i), feature_sig, Attr.ONE, Attr.PRIVATE);
                used_feats.put(Math.abs(i), s);
            }
            args.add(i < 0 ? s.in(variant_sig).not() : s.in(variant_sig));
        }
        return ExprList.make(Pos.UNKNOWN, Pos.UNKNOWN, ExprList.Op.AND, args);
    }

    private void processFeatScope(FeatureScope feats) {
        Expr res;
        Set<Integer> scp = new HashSet<Integer>(feats.feats);
        if (feats.isExact)
            for (int i = 1; i < 9; i++)
                if (used_feats.containsKey(Math.abs(i)) && !scp.contains(Math.abs(i)))
                    scp.add(-i);
        extraFacts.add(presenceCondition(scp));
    }

    private Set<Integer> computeContext() {
        Set<Integer> res = new HashSet<Integer>();

        for (Set<Integer> x : context)
            res.addAll(x);

        for (Integer x : res)
            if (res.contains(-x))
                throw new ErrorSyntax("Invalid colorful context: " + x + " and " + (-x));

        return res;
    }

    private void resolveFunc(SafeList<Func> fs) {
        for (Func f : fs) {
            context.push(f.feats);
            decls.put(f, f.feats);
            Expr nr = f.isPred ? null : f.returnDecl.accept(this);
            List<Decl> nds = new ArrayList<Decl>();
            for (Decl d : f.decls)
                nds.add(new Decl(d.isPrivate, d.disjoint, d.disjoint2, d.names, d.expr.accept(this)));
            Func nf = new Func(f.pos, f.isPrivate, f.label, nds, nr, f.getBody());
            oldfunc2new.put(f, nf);
            context.pop();
        }

        for (Func f : fs) {
            context.push(f.feats);
            Expr e = f.getBody();
            if (!f.isPred && !e.deNOP().feats.isEmpty())
                throw new ErrorSyntax(f.pos, "Cannot mark function body: " + f);
            if (f.isPred)
                e = ExprList.make(f.getBody().pos, f.getBody().pos, ExprList.Op.AND, Collections.singletonList(f.getBody()));
            oldfunc2new.get(f).setBody(e.accept(this));
            context.pop();
        }
    }

    private void resolveMacro(SafeList<Macro> fs) {
        for (Macro f : fs) {
            if (!f.feats.isEmpty())
                throw new ErrorSyntax(f.pos, "Colored macros still not supported");
        }
    }

    private void resolveAssert(ConstList<Pair<String,Expr>> as) {
        for (Pair<String,Expr> a : as) {
            a.b.accept(this);
        }
    }


    private void resolveSig(Sig s) {
        context.push(s.feats);
        decls.put(s, s.feats);

        if (s.builtin) {
            oldsig2new.put(s, s);
            return;
        }

        Sig ns;
        Function<Expr,Expr> ef = null;
        if (s instanceof PrimSig) {
            if (!s.feats.containsAll(((PrimSig) s).parent.feats))
                throw new ErrorSyntax(s.isSubsig, "Invalid colorful extension: " + s);
            if (oldsig2new.get(((PrimSig) s).parent) == null)
                resolveSig(((PrimSig) s).parent);
            PrimSig parent = (PrimSig) oldsig2new.get(((PrimSig) s).parent);

            List<Attr> atts = new ArrayList<Attr>();
            atts.addAll(s.attributes);
            for (int i = 0; i < s.attributes.size() && ef == null; i++) {
                Attr a = s.attributes.get(i);
                if (a != null) {
                    switch (a.type) {
                        case LONE :
                            atts.set(i, null);
                            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), se.lone(), se.no());
                            break;
                        case ONE :
                            atts.set(i, null);
                            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), se.one(), se.no());
                            break;
                        case SOME :
                            atts.set(i, null);
                            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), se.some(), se.no());
                            break;
                        default :
                            ;
                    }
                }
            }

            ns = new PrimSig(s.label, parent, atts.toArray(new Attr[atts.size()]));
        } else {
            Set<Sig> parents = new HashSet<Sig>();
            for (Sig ss : ((SubsetSig) s).parents) {
                if (!s.feats.containsAll(ss.feats))
                    throw new ErrorSyntax(s.isSubset, "Invalid colorful extension: " + s);

                if (oldsig2new.get(ss) == null)
                    resolveSig(ss);
                parents.add(oldsig2new.get(ss));
            }

            List<Attr> atts = new ArrayList<Attr>();
            atts.addAll(s.attributes);
            for (int i = 0; i < s.attributes.size() && ef == null; i++) {
                Attr a = s.attributes.get(i);
                if (a != null) {
                    switch (a.type) {
                        case LONE :
                            atts.set(i, null);
                            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), se.lone(), se.no());
                            break;
                        case ONE :
                            atts.set(i, null);
                            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), se.one(), se.no());
                            break;
                        case SOME :
                            atts.set(i, null);
                            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), se.some(), se.no());
                            break;
                        default :
                            ;
                    }
                }

            }

            ns = new SubsetSig(s.label, parents, atts.toArray(new Attr[atts.size()]));
        }

        if (ef == null)
            ef = se -> ExprITE.make(s.pos, presenceCondition(s.feats), ExprConstant.TRUE, se.no());

        extraFacts.add(ef.apply(ns));
        oldsig2new.put(s, ns);
        oldvar2new.put(s.decl.get(), ns.decl.get());
        context.pop();
    }

    private void resolveFields(Sig s) {
        Sig ns = oldsig2new.get(s);
        context.push(s.feats);
        for (Decl f : s.getFieldDecls()) {
            context.push(f.feats);
            List<String> vs = new ArrayList<String>();
            for (ExprHasName v : f.names) {
                vs.add(v.label);
                decls.put(v, f.feats);
            }
            Expr ne = f.expr.accept(this);
            Expr nt = f.expr.type().toExpr().accept(this);
            if (f.expr.type().arity() == 1)
                nt = nt.setOf();
            String[] names = vs.toArray(new String[vs.size()]);
            Field[] nf = ns.addTrickyField(f.names.get(0).pos, f.isPrivate, f.disjoint, f.disjoint2, ((Field) f.names.get(0)).isMeta, names, nt, f.feats);
            for (int i = 0; i < f.names.size(); i++) {
                oldfield2new.put((Field) f.names.get(i), nf[i]);
                extraFacts.add(ExprITE.make(f.names.get(i).pos, presenceCondition(f.feats), ns.decl.get().join(nf[i]).in(ne).forAll(ns.decl), nf[i].no()));
            }
            context.pop();
        }

        for (Expr f : s.getFacts()) {
            decls.put(s.decl.get(), Collections.EMPTY_SET);
            ns.addFact(f.accept(this));
            decls.remove(s.decl.get());
        }

        context.pop();
    }

    @Override
    public Expr visit(ExprBinary x) throws Err {
        context.push(x.feats);
        Expr l = x.left.accept(this);
        Expr r = x.right.accept(this);
        if (!x.left.feats.isEmpty()) {
            switch (x.op) {
                case AND :
                    l = ExprITE.make(x.left.pos, presenceCondition(x.left.feats), l, ExprConstant.TRUE);
                    break;
                case OR :
                    l = ExprITE.make(x.left.pos, presenceCondition(x.left.feats), l, ExprConstant.FALSE);
                    break;
                case PLUS :
                    l = ExprITE.make(x.left.pos, presenceCondition(x.left.feats), l, Sig.NONE); // will fail: arity
                    break;
                case INTERSECT :
                    l = ExprITE.make(x.left.pos, presenceCondition(x.left.feats), l, Sig.UNIV); // will fail: arity
                    break;
                default :
                    throw new ErrorSyntax("Cannot mark binary expression: " + x.op);
            }
        }
        if (!x.right.feats.isEmpty()) {
            switch (x.op) {
                case AND :
                    r = ExprITE.make(x.right.pos, presenceCondition(x.right.feats), r, ExprConstant.TRUE);
                    break;
                case OR :
                    r = ExprITE.make(x.right.pos, presenceCondition(x.right.feats), r, ExprConstant.FALSE);
                    break;
                case PLUS :
                    r = ExprITE.make(x.right.pos, presenceCondition(x.right.feats), r, Sig.NONE); // will fail: arity
                    break;
                case INTERSECT :
                    r = ExprITE.make(x.right.pos, presenceCondition(x.right.feats), r, Sig.UNIV); // will fail: arity
                    break;
                default :
                    throw new ErrorSyntax("Cannot mark binary expression: " + x.op);
            }
        }
        context.pop();
        return x.op.make(x.pos, x.closingBracket, l, r);
    }

    @Override
    public Expr visit(ExprList x) throws Err {
        context.push(x.feats);
        List<Expr> as = new ArrayList<Expr>();
        for (Expr a : x.args) {
            Expr e = a.accept(this);
            if (!a.feats.isEmpty())
                switch (x.op) {
                    case AND :
                        as.add(ExprITE.make(a.pos, presenceCondition(a.feats), e, ExprConstant.TRUE));
                        break;
                    case OR :
                        as.add(ExprITE.make(a.pos, presenceCondition(a.feats), e, ExprConstant.FALSE));
                        break;
                    default :
                        throw new ErrorSyntax("Cannot mark list expression: " + x.op);
                }
            else
                as.add(e);
        }
        context.pop();
        return ExprList.make(x.pos, x.closingBracket, x.op, as);
    }

    @Override
    public Expr visit(ExprCall x) throws Err {
        context.push(x.feats);
        List<Expr> args = new ArrayList<Expr>();
        if (!computeContext().containsAll(decls.get(x.fun)))
            throw new ErrorSyntax(x.pos, "Invalid context for func call: " + x.fun);
        for (Expr e : x.args)
            args.add(e.accept(this));
        context.pop();
        return oldfunc2new.get(x.fun).call(args.toArray(new Expr[args.size()]));
    }

    @Override
    public Expr visit(ExprConstant x) throws Err {
        return x;
    }

    @Override
    public Expr visit(ExprITE x) throws Err {
        if (!x.cond.feats.isEmpty() || !x.left.feats.isEmpty() || !x.right.feats.isEmpty())
            throw new ErrorSyntax("Cannot mark if-then-else expressions");

        context.push(x.feats);
        Expr c = x.cond.accept(this);
        Expr l = x.left.accept(this);
        Expr r = x.right.accept(this);
        context.pop();
        return ExprITE.make(x.pos, c, l, r);
    }

    @Override
    public Expr visit(ExprLet x) throws Err {
        if (!x.var.feats.isEmpty() || !x.expr.feats.isEmpty() || !x.sub.feats.isEmpty())
            throw new ErrorSyntax("Cannot mark let expressions");

        context.push(x.feats);
        ExprVar v = (ExprVar) x.var.accept(this);
        Expr e = x.expr.accept(this);
        Expr s = x.sub.accept(this);
        context.pop();
        return ExprLet.make(x.pos, v, e, s);
    }

    @Override
    public Expr visit(ExprQt x) throws Err {
        if (!x.sub.feats.isEmpty())
            throw new ErrorSyntax("Cannot mark quantified expressions");

        context.push(x.feats);
        List<Decl> ds = new ArrayList<Decl>();
        for (Decl d : x.decls) {
            if (!d.feats.isEmpty())
                throw new ErrorSyntax("Cannot mark quantified declarations");

            Expr e = d.expr.accept(this);

            for (ExprHasName v : d.names)
                decls.put(v, Collections.EMPTY_SET);

            ds.add(new Decl(d.isPrivate, d.disjoint, d.disjoint2, d.names, e));
        }

        Expr s = x.sub.accept(this);

        for (Decl d : x.decls)
            for (ExprHasName v : d.names)
                decls.remove(v);

        context.pop();
        return x.op.make(x.pos, x.closingBracket, ds, s);
    }

    @Override
    public Expr visit(ExprUnary x) throws Err {
        if (!x.op.equals(ExprUnary.Op.NOOP) && !x.sub.feats.isEmpty())
            throw new ErrorSyntax("Cannot mark unary expressions");
        context.push(x.feats);
        Expr s = x.sub.accept(this);
        context.pop();
        return x.op.make(x.pos, s);
    }

    @Override
    public Expr visit(ExprVar x) throws Err {
        if (!computeContext().containsAll(decls.get(x)))
            throw new ErrorSyntax("Invalid context for var call: " + x);
        if (oldvar2new.containsKey(x))
            return oldvar2new.get(x);
        else
            return x;
    }

    @Override
    public Expr visit(Sig x) throws Err {
        if (!computeContext().containsAll(decls.get(x)))
            throw new ErrorSyntax("Invalid context for sig call: " + x);
        return oldsig2new.get(x);
    }

    @Override
    public Expr visit(Field x) throws Err {
        if (!computeContext().containsAll(decls.get(x)))
            throw new ErrorSyntax("Invalid context for field call: " + x);
        return oldfield2new.get(x);
    }

}
