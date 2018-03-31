// a Matsuo Basho hiaku in a list reference
let matsuoBasho = ref [ "An"; "old"; "pond!";
    "A"; "frog"; "jumps"; "in-";
    "The"; "sound"; "of"; "water" ]
    
while (List.length !matsuoBasho > 0) do
    printf "%s " (List.head !matsuoBasho)
    matsuoBasho := List.tail !matsuoBasho