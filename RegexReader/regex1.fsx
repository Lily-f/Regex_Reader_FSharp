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

let add_element (element: element) = 
  // TODO: add element to deepest group or alternation. see ruby element class examples
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
        Some {Type = element_type.alternation; is_repetable = is_repeatable; is_open = true; elements = alt_elems; depth = depth; value = char}
    | '(' -> 
      if is_repeatable then raise (SYNTAX_ERROR("SYNTAX ERROR"))
      depth <- depth + 1 
      let group_elems: element[][] = Array.empty
      group_elems.[0] <- Array.empty
      Some {Type = element_type.group; is_repetable = is_repeatable; is_open = true; elements = group_elems; depth = depth; value = char}
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
    printfn "YES"
  else
    printfn "NO"

let read_character (characters: char[]) = 
  // Read character at cursor and next character
  let char = characters.[cursor]
  let read_ahead = if cursor < characters.Length - 1 then characters.[cursor + 1] else ' '
  cursor <- cursor + 1

  // Check for repetable syntax errors and if element is repeatable
  if char.Equals '*' then raise (SYNTAX_ERROR("SYNTAX ERROR"))
  let mutable is_repeatable = false
  if read_ahead.Equals '*' then
    if char.Equals '|' then raise (SYNTAX_ERROR("SYNTAX ERROR"))
    is_repeatable <- true
    cursor <- cursor + 1

  // Handle element creation
  let element = create_element(char, is_repeatable)
  if not element.IsNone then 
    if is_nested_child then
      add_element elements.[elements.Length - 1] |> ignore
    else
      elements.[elements.Length] <- element.Value


// Read stuff from files and run the methods to verify the strings. 