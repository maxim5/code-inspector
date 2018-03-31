// a class that represents a user
// it's constructor takes three parameters, the user's 
// first name, last name and a hash of their password
type User(firstName, lastName, passwordHash) =
    // calculate the user's full name and store of later use
    let fullName = Printf.sprintf "%s %s" firstName lastName
    // print users fullname as object is being constructed    
    do printfn "User: %s" fullName
    
    // hashs the users password and checks it against
    // the known hash
    member x.Authenticate(password) =
        let hashResult = hash (password, "sha1")
        passwordHash = hashResult

    // retrieves the users full name
    member x.GetFullname() = fullName
