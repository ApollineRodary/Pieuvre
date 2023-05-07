include Types

let string_of_lam = Display.string_of_lam
let string_of_type = Display.string_of_type
let print_lam = Display.print_lam
let print_type = Display.print_type

let is_alpha_convertible = Is_alpha_convertible.is_alpha_convertible
let betastep = Betastep.betastep
let reduce = Reduce.reduce
let infer_type = Infer_type.infer_type
let typecheck = Typecheck.typecheck