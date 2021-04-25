exception SYNTAX_ERROR of string

type element_type = 
  | wild = 0
  | group = 1
  | character = 2
  | alternation = 3

type element = {Type: element_type; is_repetable: bool; is_open: bool; elements: element[][]; depth: int; value: char}

let mutable depth = 0
let mutable cursor = 0
let mutable elements: element array = Array.empty

let close_group (is_repeatable: bool) = 
  null

let is_nested_child: bool = 
  elements.[elements.Length].is_open

let add_alternation (element: element) = 
  null

let run_regex (regex, message) = 
  null

let read_character (characters: string) = 
  null

let evaluate_wild (element: element, characters: char[]): bool = 
  false

let evaluate_char (element: element, characters: char[]): bool = 
  if element.is_repetable then
    false
  else
    false

let evaluate_group (element: element, characters: char[]): bool = 
  false

let evaluate_alternation (element: element, characters: char[]): bool = 
  false

let create_element (char: char, is_repeatable: bool): Option<element> = 
  match char with
    | '.' -> Some {Type = element_type.wild; is_repetable = is_repeatable; is_open = false; elements = null; depth = depth; value = char}
    | '|' -> 
      if is_nested_child then
        add_alternation(elements.[elements.Length]) |> ignore
        None
      else
        let alt_elems: element[][] = Array.empty
        let previous_elements: element array = Array.copy elements : element []
        alt_elems.[0] <- previous_elements
        alt_elems.[1] <- Array.empty
        elements <- Array.empty
        Some {Type = element_type.alternation; is_repetable = is_repeatable; is_open = false; elements = alt_elems; depth = depth; value = char}
    | '(' -> 
      if is_repeatable then raise (SYNTAX_ERROR("SYNTAX ERROR"))
      depth <- depth + 1 
      Some {Type = element_type.group; is_repetable = is_repeatable; is_open = true; elements = null; depth = depth; value = char}
    | ')' -> 
      if depth.Equals 0 then raise (SYNTAX_ERROR("SYNTAX ERROR"))
      close_group(is_repeatable) |> ignore
      depth <- depth - 1
      None
    |_ -> Some {Type = element_type.character; is_repetable = is_repeatable; is_open = false; elements = null; depth = depth; value = char}

// Verify a string against the regex elements
let verify_message (message: string) = 
  let characters = message.ToCharArray()
  let all_valid = Array.forall(fun (element: element) -> 
    match element.Type with
      | element_type.wild -> evaluate_wild(element, characters)
      | element_type.character -> evaluate_char(element, characters)
      | element_type.alternation -> evaluate_alternation(element, characters)
      | element_type.group -> evaluate_group(element, characters)
      |_ -> false
  )
  if all_valid elements && Array.isEmpty characters then
    printf "YES"
  else
    printf "NO"

let add_element (element: element) = 
  null


// Read stuff from files and run the methods to verify the strings. 