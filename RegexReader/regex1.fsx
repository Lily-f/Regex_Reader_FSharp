type element_type = 
  | wild = 0
  | group = 1
  | character = 2
  | alternation = 3

type element = {Type: element_type; is_repetable: bool; is_open: bool; elements: element[][]; depth: int}

let depth = 0
let cursor = 0
let elements = []

let run_regex (regex, message) = 
  null

let read_character (characters: string) = 
  null

let create_element (char: char, is_repeatable: bool): Option<element> = 
  match char with
    | '.' -> Some {Type = element_type.wild; is_repetable = false; is_open = false; elements = null; depth = depth}
    | '|' -> Some {Type = element_type.alternation; is_repetable = false; is_open = false; elements = null; depth = depth}
    | '(' -> Some {Type = element_type.group; is_repetable = false; is_open = false; elements = null; depth = depth}
    | ')' -> None
    |_ -> Some {Type = element_type.character; is_repetable = false; is_open = false; elements = null; depth = depth}

let is_nested_child (elements: 'a[]) = 
  null

let verify_message (message: string) = 
  null

let evaluate_wild (element: element, characters: string): string = 
  null

let evaluate_char (element: element, characters: string): string = 
  if element.is_repetable then
    ""
  else
    ""

let evaluate_group (element: element, characters: string): string = 
  null

let evaluate_alternation (element: element, characters: string): string = 
  null



// Read stuff from files and run the methods to verify the strings. 