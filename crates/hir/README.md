# swamp-hir

HIR is Swamp’s High-level Intermediate Representation: it’s close to the language (ifs, fors,
fields, indexing),
but it’s flattened into simple, ordered variable assignments. Think of it as your source
code after the sugar has been shaved off and the evaluation order nailed down.

It’s ANF-normalized (A-Normal Form): every non-trivial expression is first bound
to a let temp, so later phases don’t have to guess about order or effects.

## Quick vibe

**Close to source:** still has if as an expression, for loops, indexing, field access.

**Flattened:** complex expressions become a sequence of let bindings.

**Left-to-right evaluation:** baked in. We preserve that by introducing temps.

**Typed:** all names resolve to SymId, all nodes have TypeId.

**Spans for diagnostics:** every node carries a NodeId that maps to source_map_cache::Node.

## Example

Source:

```swamp
a[f()].x += b[g()] + c[h()]
```

HIR:

```
t_f = Call f()
addr = AddrOf( Index(Var(a), t_f).Field("x") )
t_g = Call g()
lhs = Use( Index(Var(b), t_g) )
t_h = Call h()
rhs = Use( Index(Var(c), t_h) )
sum = Add(lhs, rhs)
*addr = sum
```
