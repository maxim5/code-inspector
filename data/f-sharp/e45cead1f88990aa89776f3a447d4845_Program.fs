// a class that represents a user
// it's constructor takes two parameters, the user's 
// name and a hash of their password
type User(name, passwordHash) =
    // hashs the users password and checks it against
    // the known hash
    member x.Authenticate(password) =
        let hashResult = hash (password, "sha1")
        passwordHash = hashResult

    // gets the users logon message
    member x.LogonMessage() =
        Printf.sprintf "Hello, %s" name

    // creates a copy of the user with the password changed
    member x.ChangePassword(password) =
        new User(name, hash password)
