module DQueue

type DQueue<'a when 'a : equality>

val empty  : DQueue<'a> 
val isEmpty: DQueue<'a> -> bool
val put    : 'a -> DQueue<'a> -> DQueue<'a>
val get    : DQueue<'a> -> ('a * DQueue<'a>) option