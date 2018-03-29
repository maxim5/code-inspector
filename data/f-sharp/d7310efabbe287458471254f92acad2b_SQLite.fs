open System
open System.IO
open System.Data.SQLite

//Creating the sql-scripts for fill data from tables 
// Without System.Data.SQLite this program will not work
//You may:
//1) Install Nuget package manager extension for Visual Studio. 
//2) Right click your project and select "Manage Nuget Packages".
//3) Browse for and install SQLite package.

let createSQL(path:string):unit=
    let connection:SQLiteConnection=new SQLiteConnection("Data Source="+path)
    
    let pattern(sqlQuery:string, func: SQLiteDataReader -> 'T):list<'T>=
        let cmd=connection.CreateCommand()
        connection.Open()
        cmd.CommandText<-sqlQuery
        let reader=cmd.ExecuteReader()
        let values=[ while reader.Read() do
                            yield func(reader) ]
        connection.Close()
        values

    let getNamesTables():list<string>=
        pattern("SELECT name FROM sqlite_master WHERE type='table'", fun reader -> reader.GetString(0))
    
    //first - cid, second - name column, third - type
    let getTableInfo(nameTable:string):list<int*string*string>=
        pattern(String.Format ("pragma table_info('{0}')", nameTable), 
                        fun reader -> (reader.GetInt32(0), reader.GetString(1), reader.GetString(2)))

    let getValues(nameTable:string, infoTable:list<int*string*string>):list<list<string>> =
        let getItem (reader:SQLiteDataReader):list<string>=
             let matchingType i (cid, name, typeColumn) =
                let mutable value=reader.GetValue(i).ToString()
                if (value<>String.Empty) then
                    if (typeColumn="TEXT") then
                        "'"+ value.ToString()+"'"
                    else
                        value
                else
                    "null"
                
             infoTable |> List.mapi matchingType 

        pattern(String.Format("SELECT * FROM {0}", nameTable), getItem)

    let combine state el =if (state<>String.Empty) then String.Format("{0}, {1}", state, el)
                                        else el
    
    let valuesToSql(values:list<list<string>>):string=
        let itemToString(item:list<string>):string=
            List.fold combine String.Empty item
        
        let liItems = [ for i in values do
                            yield String.Format("({0})", itemToString(i))]
        
        List.fold combine String.Empty liItems                   
    
    let saveInFile(nameTable:string, infoTable:list<int*string*string>, sqlPart:string):unit=
        let sw=new StreamWriter(@"C:\temp\"+nameTable+".sql")

        sw.WriteLine("insert or replace into {0}", nameTable)
        let onlyNameColumn=infoTable |> List.map (fun (x, y, z) -> y) 
        sw.WriteLine("({0})", List.fold combine "" onlyNameColumn) 
        sw.WriteLine("values")
        sw.WriteLine(sqlPart)
        sw.Close()
    
    let tables=getNamesTables()
    let tablesInfo=tables |> List.map getTableInfo
    let tablesValues= List.map2 (fun x y -> getValues(x, y)) tables tablesInfo



    ignore(List.map3 (fun x y z -> saveInFile(x, y,valuesToSql(z)))  tables tablesInfo tablesValues)
    
    ignore(Console.ReadLine())

createSQL(@"C:\Temp\DBAndroid1\databases\DAS.db")