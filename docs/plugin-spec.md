# Specification of the disambiguation algorithm used in `cleff-plugin`

## Notations

- Concrete type and type constructors are written in capital letters (`E`)
- Type variables are written in lower case (`e`)
- A type that can have any form is written in curly braces (`{e}`)

## The algorithm

- Given the set of already solved constraints *Given*, and the set of constraints that GHC is unable to solve *Wanted*.
- Take the set of constraints of form `E {x1 ... xn} :> {es}` separately from *Given* and *Wanted*, call them respectively *RelevantGiven* and *RelevantWanted*.
- Take the set of constraints *not* of form `E {x1 ... xn} :> {es}` from *Wanted*, call it *ExtraWanted*.
- For each constraint in *RelevantWanted* `E {x1 ... xn} :> {es}`:
  - If `{es}` is of form `E1 {...} : E2 {...} : ... : es`, *i.e.* has concrete elements on its head:
    - Extract them as constraints in the form of `E1 ... :> {es}`, `E2 ... :> {es}` etc, then merge this set with *RelevantGiven* into a new set *Candidate*.
    - Otherwise, *Candidate* is the same as *RelevantGiven*.
  - For each constraint in the set *Candidate* `E' {b1 ... bn} :> {es'}`, if the following criteria are all met, add it to a new set *UnifiableCandidate*:
    - `E` and `E'` are the same type;
    - `{es}` and `{es'}` are the same type;
    - There is a substitution **s** that unifies `E {a1 ... an}` and `E' {b1 ... bn}`.
  - If there is only one *UnifiableCandidate*, use it as the solution.
  - Otherwise, for each constraint in the set *UnifiableCandidate* of form `E' {b1 ... bn} :> {es'}` and corresponding substitution **s**:
    - If **s**(*Given*) satisfies all constraints in **s**(*ExtraWanted*), add it to a new set *SatisfiableCandidate*.
  - If there is only one *SatisfiableCandidate*, use it as the solution.
  - Otherwise, we're unable to produce a unique solution.
