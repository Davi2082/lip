type loc = int

type envval = IVar of loc | IProc of ide * cmd
type memval = int (* com'è possibile che cerchiamo una coppia in un intero? *)

type env = ide -> envval
type mem = loc -> memval

type state = env list * mem * loc