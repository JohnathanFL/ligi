use crate::words::StrID;

// Modelled after S-Expressions for maximum extensibility/modularity



// The idea is that we'll do things like
// `if x then y else z` as Cmd("if", ("x", None, "y"), (None, "z"), (None, None))
// where the last two are always `else` and `finally`


// TODO: Pull in the SmallVec crate
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Atom {
  /// Word, 1, x, \+, \if, etc
  Word(StrID),
  /// #Word
  Tag(StrID),
  Str(StrID),
  /// The command is something like "if", "+", etc
  /// Function calls and indexes are translated as the commands "()" and "[]"
  Cmd(StrID, Vec<Atom>),
  /// Something like .x: usize = 10, or let x: usize = 10
  /// The latter would be wrapped in a Cmd("Let", (Loc(...)))
  /// Stored as (All locs, All types, the init)
  /// Stuff like .(x: usize, y: bool) becomes ((x, y), (usize, bool), None)
  Loc(Box<Atom>, Box<Atom>, Box<Atom>),
  /// Separate from Compound simply to keep array(T) and tuples separate
  Tuple(Box<Atom>, Vec<Atom>),
  /// If its atoms are Locs, then it can be inferred as an anon struct
  /// Otherwise, it's inferred as an array
  Compound(Box<Atom>, Vec<Atom>),

  /// !T
  Placeholder(StrID),
  /// () - Always of void type
  Unit,
  /// _ - A special type that can absorb any value
  /// Comparisons against it are always true (for patterns)
  Sink,


  /// With the choice between using Option<Atom> everywhere and this,
  /// I choose baking None into the type. This also makes Vec<Atom> simpler.
  None,
}
