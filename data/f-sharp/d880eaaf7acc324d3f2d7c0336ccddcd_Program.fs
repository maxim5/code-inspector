// a class that represents a user
// it's constructor takes two parameters, the user's 
// name and a hash of their password
type User(name, passwordHash) =
    // store the password hash in a mutable let 
    // binding, so it can be changed later
    let mutable passwordHash = passwordHash

    // hashs the users password and checks it against
    // the known hash
    member x.Authenticate(password) =
        let hashResult = hash (password, "sha1")
        passwordHash = hashResult

    // gets the users logon message
    member x.LogonMessage() =
        Printf.sprintf "Hello, %s" name

    // changes the users password
    member x.ChangePassword(password) =
        passwordHash <- hash password
