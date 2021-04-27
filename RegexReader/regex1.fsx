exception SYNTAX_ERROR of string

type element_type = 
  | wild = 0
  | group = 1
  | character = 2
  | alternation = 3

type element = {Type: element_type; mutable is_repetable: bool; mutable is_open: bool; elements: element[][]; depth: int; value: char}

let mutable depth = 0
let mutable cursor = 0
let mutable base_elements: element array = Array.empty

let rec close_group (element: element, depth: int, is_repeatable: bool) = 
  if element.Type.Equals element_type.group then
    if element.depth.Equals depth then
      element.is_open <- false
      element.is_repetable <- is_repeatable
    else
      let element_last_item = element.elements.[0].[element.elements.[0].Length - 1]
      close_group(element_last_item, depth, is_repeatable)
  else
    let inner_array = element.elements.[element.elements.Length - 1]
    close_group(inner_array.[inner_array.Length - 1], depth, is_repeatable)

let is_nested_child: bool = 
  base_elements.[base_elements.Length - 1].is_open

let add_alternation (element: element) = 
  if element.Type.Equals element_type.group then
    printf ""
  else
    printf ""

// Reads a given string, creates a regex from it, and verifies it against a given message
let run_regex (regex, message) = 
  null

// Adds an element to a container element (group or alternation)
let rec add_element (container_element: element, element: element) = 
  if element.Type.Equals element_type.group then
    if container_element.elements.[0].[container_element.elements.[0].Length - 1].is_open then
      add_element(container_element.elements.[0].[container_element.elements.[0].Length - 1], element)
    else
      container_element.elements.[0].[container_element.elements.[0].Length] <- element
  else
    let last_array = container_element.elements.[container_element.elements.Length - 1]
    if last_array.[last_array.Length - 1].is_open then
      add_element(last_array.[last_array.Length - 1], element)
    else
      last_array.[last_array.Length] <- element

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
        add_alternation(base_elements.[base_elements.Length])
        None
      else
        let alt_elems: element[][] = Array.empty
        let previous_elements: element array = Array.copy base_elements : element []
        alt_elems.[0] <- previous_elements
        alt_elems.[1] <- Array.empty
        base_elements <- Array.empty
        Some {Type = element_type.alternation; is_repetable = is_repeatable; is_open = true; elements = alt_elems; depth = depth; value = char}
    | '(' -> 
      if is_repeatable then raise (SYNTAX_ERROR("SYNTAX ERROR"))
      depth <- depth + 1 
      let group_elems: element[][] = Array.empty
      group_elems.[0] <- Array.empty
      Some {Type = element_type.group; is_repetable = false; is_open = true; elements = group_elems; depth = depth; value = char}
    | ')' -> 
      if depth.Equals 0 then raise (SYNTAX_ERROR("SYNTAX ERROR"))
      close_group(base_elements.[base_elements.Length-1], depth, is_repeatable)
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
  if all_valid base_elements && Array.isEmpty characters then
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
      add_element (base_elements.[base_elements.Length - 1], element.Value)
    else
      base_elements.[base_elements.Length] <- element.Value


// Read stuff from files and run the methods to verify the strings. 